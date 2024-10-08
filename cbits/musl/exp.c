/*
 * Double-precision e^x function.
 *
 * Copyright (c) 2018, Arm Limited.
 * SPDX-License-Identifier: MIT
 */

#include <math.h>
#include <stdint.h>
#include "libm.h"
#include "exp_data.h"
#include "../softfloat/softfloat.h"

#define N (1 << EXP_TABLE_BITS)
#define InvLn2N __kadena_exp_data.invln2N
#define NegLn2hiN __kadena_exp_data.negln2hiN
#define NegLn2loN __kadena_exp_data.negln2loN
#define Shift __kadena_exp_data.shift
#define T __kadena_exp_data.tab
#define C2 __kadena_exp_data.poly[5 - EXP_POLY_ORDER]
#define C3 __kadena_exp_data.poly[6 - EXP_POLY_ORDER]
#define C4 __kadena_exp_data.poly[7 - EXP_POLY_ORDER]
#define C5 __kadena_exp_data.poly[8 - EXP_POLY_ORDER]

// Helper function to convert an int64_t to float64_t
static float64_t cast_int64(int64_t value) {
    float64_t result = {value};
    return result;
}

static float64_t f64_shiftr(float64_t v, int s) {
	float64_t result;
	result.v = v.v >> s;
	return result;
}

static float64_t f64_negate(float64_t i) {
	float64_t result = {i.v ^ 0x8000000000000000};
	return result;
}

static float64_t f64_abs(float64_t i) {
	float64_t result = {i.v & 0x7FFFFFFFFFFFFFFF};
	return result;
}

/* Handle cases that may overflow or underflow when computing the result that
   is scale*(1+TMP) without intermediate rounding.  The bit representation of
   scale is in SBITS, however it has a computed exponent that may have
   overflown into the sign bit so that needs to be adjusted before using it as
   a double.  (int32_t)KI is the k used in the argument reduction and exponent
   adjustment of scale, positive k here means the result may overflow and
   negative k means the result may underflow.  */
static inline float64_t specialcase(float64_t tmp, uint64_t sbits, uint64_t ki)
{
	float64_t scale, y;

	if ((ki & 0x80000000) == 0) {
		/* k > 0, the exponent of scale might have overflowed by <= 460.  */
		sbits -= 1009ull << 52;
		scale = cast_int64(sbits); // asdouble(sbits)
		y = f64_mul(cast_int64(0x7f00000000000000), f64_mulAdd(scale, tmp, scale)); // 0x1p1009 * (scale + scale * tmp);
		return y;
	}
	/* k < 0, need special care in the subnormal range.  */
	sbits += 1022ull << 52;
	/* Note: sbits is signed scale.  */
	scale = cast_int64(sbits); // asdouble(sbits);
	y = f64_mulAdd(scale, tmp, scale); // scale + scale * tmp;
	if (f64_lt(f64_abs(y), ui32_to_f64(1))) {
		/* Round y to the right precision before scaling it into the subnormal
		   range to avoid double rounding that can cause 0.5+E/2 ulp error where
		   E is the worst-case ulp error outside the subnormal range.  So this
		   is only useful if the goal is better than 1 ulp worst-case error.  */
		float64_t hi, lo, one = ui32_to_f64(1);
		if (f64_lt(y, ui32_to_f64(0)))
			one = ui32_to_f64(-1);
		lo = f64_mulAdd(scale, tmp, f64_add(scale, f64_negate(y))); // scale - y + scale * tmp;
		hi = f64_add(one, y); //one + y;
		lo = f64_add(one, f64_add(f64_negate(hi), f64_add(y, lo))); // one - hi + y + lo;
		y = f64_sub(f64_add(hi, lo), one); // eval_as_double(hi + lo) - one;
		/* Fix the sign of 0.  */
		if (f64_eq(y, i32_to_f64(0)))
			y = cast_int64(sbits & 0x8000000000000000); // asdouble(sbits & 0x8000000000000000);
		/* The underflow exception needs to be signaled explicitly.  */
		fp_force_eval(fp_barrier(0x1p-1022) * 0x1p-1022);
	}
	y = f64_mul(cast_int64(0x0010000000000000),y);
	return y;
}

// static inline float64_t specialcase(float64_t tmp, uint64_t sbits, uint64_t ki)
// {
// 	double_t scale, y;

// 	if ((ki & 0x80000000) == 0) {
// 		/* k > 0, the exponent of scale might have overflowed by <= 460.  */
// 		sbits -= 1009ull << 52;
// 		scale = asdouble(sbits);
// 		y = 0x1p1009 * (scale + scale * tmp);
// 		return eval_as_double(y);
// 	}
// 	/* k < 0, need special care in the subnormal range.  */
// 	sbits += 1022ull << 52;
// 	scale = asdouble(sbits);
// 	y = scale + scale * tmp;
// 	if (y < 1.0) {
// 		/* Round y to the right precision before scaling it into the subnormal
// 		 range to avoid double rounding that can cause 0.5+E/2 ulp error where
// 		 E is the worst-case ulp error outside the subnormal range.  So this
// 		 is only useful if the goal is better than 1 ulp worst-case error.  */
// 		double_t hi, lo;
// 		lo = scale - y + scale * tmp;
// 		hi = 1.0 + y;
// 		lo = 1.0 - hi + y + lo;
// 		y = eval_as_double(hi + lo) - 1.0;
// 		/* Avoid -0.0 with downward rounding.  */
// 		if (WANT_ROUNDING && y == 0.0)
// 			y = 0.0;
// 		/* The underflow exception needs to be signaled explicitly.  */
// 		fp_force_eval(fp_barrier(0x1p-1022) * 0x1p-1022);
// 	}
// 	y = 0x1p-1022 * y;
// 	return eval_as_double(y);
// }

/* Top 12 bits of a double (sign and exponent bits).  */
static inline uint32_t top12d(double x)
{
	return asuint64(x) >> 52;
}

static inline uint32_t top12(float64_t x)
{
	return x.v >> 52;
}

/* Computes sign*exp(x+xtail) where |xtail| < 2^-8/N and |xtail| <= |x|.
   The sign_bias argument is SIGN_BIAS or 0 and sets the sign to -1 or 1.  */
static float64_t musl_expf64(float64_t x)
{
	uint32_t abstop;
	uint64_t ki, idx, top, sbits;
	float64_t kd, z, r, r2, scale, tail, tmp;

	abstop = top12(x) & 0x7ff;

	float64_t onePN54 = {0x3c90000000000000};
	if (predict_false(abstop - top12(onePN54) >= top12(cast_int64(0x4094480000000000)) - top12(onePN54))) {
		if (abstop - top12(onePN54) >= 0x80000000)
			/* Avoid spurious underflow for tiny x.  */
			/* Note: 0 is common input.  */
			return WANT_ROUNDING ? f64_add(ui32_to_f64(1), x) : ui32_to_f64(1); // 1.0 + x : 1.0;
		if (abstop >= top12(ui32_to_f64(1024))) {
			if (x.v == asuint64(-INFINITY))
				return ui32_to_f64(0);
			if (abstop >= top12d(INFINITY))
				return f64_add(ui32_to_f64(1), x);
			if (x.v >> 63)
				return __kadena_math_uflow_sf(0);
			else
				return __kadena_math_oflow_sf(0);
		}
		/* Large x is special cased below.  */
		abstop = 0;
	}

	/* exp(x) = 2^(k/N) * exp(r), with exp(r) in [2^(-1/2N),2^(1/2N)].  */
	/* x = ln2/N*k + r, with int k and r in [-ln2/2N, ln2/2N].  */
	z = f64_mul(InvLn2N, x);
#if TOINT_INTRINSICS
	kd = roundtoint(z);
	ki = converttoint(z);
#elif EXP_USE_TOINT_NARROW
	/* z - kd is in [-0.5-2^-16, 0.5] in all rounding modes.  */
	kd = eval_as_double(z + Shift);
	ki = asuint64(kd) >> 16;
	kd = (double_t)(int32_t)ki;
#else
	/* z - kd is in [-1, 1] in non-nearest rounding modes.  */
	kd = f64_add(z, Shift); // eval_as_double(z + Shift);
	ki = kd.v; // asuint64(kd)
	kd = f64_sub(kd, Shift); // -= Shift;
#endif
	r = f64_mulAdd(kd, NegLn2loN, f64_mulAdd(kd, NegLn2hiN, x)); //x + kd * NegLn2hiN + kd * NegLn2loN;
	/* 2^(k/N) ~= scale * (1 + tail).  */
	idx = 2 * (ki % N);
	top = ki << (52 - EXP_TABLE_BITS);
	tail = cast_int64(T[idx]); // asdouble(T[idx]);
	/* This is only a valid scale when -1023*N < k < 1024*N.  */
	sbits = T[idx + 1] + top;
	/* exp(x) = 2^(k/N) * exp(r) ~= scale + scale * (tail + exp(r) - 1).  */
	/* Evaluation is optimized assuming superscalar pipelined execution.  */
	r2 = f64_mul(r, r); // r * r;
	/* Without fma the worst case error is 0.25/N ulp larger.  */
	/* Worst case error is less than 0.5+1.11/N+(abs poly error * 2^53) ulp.  */
	tmp =
	f64_mulAdd(f64_mul(r2, r2), f64_mulAdd(r, C5, C4), f64_mulAdd(r2, f64_mulAdd(r, C3, C2), f64_add(tail, r)));
	// tail + r + r2 * (C2 + r * C3) + r2 * r2 * (C4 + r * C5);
	if (predict_false(abstop == 0))
		return specialcase(tmp, sbits, ki);
	scale = cast_int64(sbits);
	/* Note: tmp == 0 or |tmp| > 2^-200 and scale > 2^-739, so there
	   is no spurious underflow here even without fma.  */
	return f64_mulAdd(scale, tmp, scale);
}

double musl_exp(double x){
  float64_t out = musl_expf64(cast_int64(asuint64(x)));
	return asdouble(out.v);
}
