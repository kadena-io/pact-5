/*
 * Double-precision x^y function.
 *
 * Copyright (c) 2018, Arm Limited.
 * SPDX-License-Identifier: MIT
 */

#include <math.h>
#include <stdint.h>
#include "../softfloat/softfloat.h"
#include "libm.h"
#include "exp_data.h"
#include "pow_data.h"

#define WANT_ROUNDING 1
/*
Worst-case error: 0.54 ULP (~= ulperr_exp + 1024*Ln2*relerr_log*2^53)
relerr_log: 1.3 * 2^-68 (Relative error of log, 1.5 * 2^-68 without fma)
ulperr_exp: 0.509 ULP (ULP error of exp, 0.511 ULP without fma)
*/

#define T __kadena_pow_log_data.tab
#define A __kadena_pow_log_data.poly
#define Ln2hi __kadena_pow_log_data.ln2hi
#define Ln2lo __kadena_pow_log_data.ln2lo
#define N (1 << POW_LOG_TABLE_BITS)
#define OFF 0x3fe6955500000000

/* Top 12 bits of a double (sign and exponent bits).  */
static inline uint32_t top12(float64_t x)
{
	return x.v >> 52;
}

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


/* Compute y+TAIL = log(x) where the rounded result is y and TAIL has about
   additional 15 bits precision.  IX is the bit representation of x, but
   normalized in the subnormal range using the sign bit for the exponent.  */
static inline float64_t log_inline(uint64_t ix, float64_t *tail)
{
	/* double_t for better performance on targets with FLT_EVAL_METHOD==2.  */
	float64_t z, r, y, invc, logc, logctail, kd, hi, t1, t2, lo, lo1, lo2, p;
	uint64_t iz, tmp;
	int k, i;

	/* x = 2^k z; where z is in range [OFF,2*OFF) and exact.
	   The range is split into N subintervals.
	   The ith subinterval contains z and c is near its center.  */
	tmp = ix - OFF;
	i = (tmp >> (52 - POW_LOG_TABLE_BITS)) % N;
	k = (int64_t)tmp >> 52; /* arithmetic shift */
	iz = ix - (tmp & 0xfffULL << 52);
	z = cast_int64(iz); // asdouble(iz)
	kd = i32_to_f64(k); // asuint64((double_t)k);

	/* log(x) = k*Ln2 + log(c) + log1p(z/c-1).  */
	invc = T[i].invc;
	logc = T[i].logc;
	logctail = T[i].logctail;

	/* Note: 1/c is j/N or j/N/2 where j is an integer in [N,2N) and
     |z/c - 1| < 1/N, so r = z/c - 1 is exactly representible.  */
// #if __FP_FAST_FMA
// 	r = __builtin_fma(z, invc, -1.0);
// #else
	/* Split z such that rhi, rlo and rhi*rhi are exact and |rlo| <= |r|.  */
	float64_t zhi = {(iz + (1ULL << 31)) & (-1ULL << 32)}; // asdouble((iz + (1ULL << 31)) & (-1ULL << 32));
	float64_t zlo = f64_sub(z,zhi); // z - zhi;
	float64_t neg1 = {0xBFF0000000000000};
	float64_t rhi = f64_mulAdd(zhi, invc, neg1); // zhi * invc - 1.0;
	float64_t rlo = f64_mul(zlo, invc); // zlo * invc;
	r = f64_add(rhi, rlo);// rhi + rlo;

	/* k*Ln2 + log(c) + r.  */
	t1 = f64_mulAdd(kd, Ln2hi, logc);
	t2 = f64_add(t1, r);
	lo1 = f64_mulAdd(kd, Ln2lo, logctail);
	lo2 = f64_add(f64_add(t1, f64_negate(t2)), r); // t1 - t2 + r;


	/* Evaluation is optimized assuming superscalar pipelined execution.  */
	float64_t ar, ar2, ar3, lo3, lo4;
	ar = f64_mul(A[0], r); // A[0] * r; /* A[0] = -0.5.  */
	ar2 = f64_mul(r, ar); // r * ar;
	ar3 = f64_mul(r, ar2); //r * ar2;
	/* k*Ln2 + log(c) + r + A[0]*r*r.  */
// #if __FP_FAST_FMA
	// hi = t2 + ar2;
	// lo3 = __builtin_fma(ar, r, -ar2);
	// lo4 = t2 - hi + ar2;
// #else
	float64_t arhi = f64_mul(A[0], rhi); //A[0] * rhi;
	float64_t arhi2 = f64_mul(rhi, arhi); // rhi * arhi;
	hi = f64_add(t2, arhi2); // t2 + arhi2;
	lo3 = f64_mul(rlo, f64_add(ar, arhi)); // rlo * (ar + arhi);
	lo4 = f64_add(t2, f64_add(f64_negate(hi), arhi2)); // t2 - hi + arhi2;
// #endif
	/* p = log1p(r) - r - A[0]*r*r.  */
	float64_t tmp1, tmp2, tmp3;
	tmp1 = f64_mul(ar2, f64_mulAdd(r, A[6], A[5]));  // ar2 * (A[5] + r * A[6])
	tmp2 = f64_add(A[3], f64_mulAdd(r, A[4], tmp1)); // A[3] + r * A[4] + ar2 * (A[5] + r * A[6])
	tmp3 = f64_mulAdd(r, A[2], f64_mul(ar2, tmp2)); // r * A[2] + ar2 * (A[3] + r * A[4] + ar2 * (A[5] + r * A[6]))
	p = f64_mul(ar3, f64_add(A[1], tmp3)); // (ar3 * (A[1] + r * A[2] + ar2 * (A[3] + r * A[4] + ar2 * (A[5] + r * A[6]))));

	lo = f64_add(lo1, f64_add(lo2, f64_add(lo3, f64_add(lo4, p)))); // lo1 + lo2 + lo3 + lo4 + p;
	y = f64_add(hi, lo); // hi + lo
	*tail = f64_add(hi, f64_add(f64_negate(y), lo)); // hi - y + lo;
	return y;
}

#undef N
#undef T
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
#define C6 __kadena_exp_data.poly[9 - EXP_POLY_ORDER]

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

#define SIGN_BIAS (0x800 << EXP_TABLE_BITS)

/* Computes sign*exp(x+xtail) where |xtail| < 2^-8/N and |xtail| <= |x|.
   The sign_bias argument is SIGN_BIAS or 0 and sets the sign to -1 or 1.  */
static inline float64_t exp_inline(float64_t x, float64_t xtail, uint32_t sign_bias)
{
	uint32_t abstop;
	uint64_t ki, idx, top, sbits;
	/* double_t for better performance on targets with FLT_EVAL_METHOD==2.  */
	float64_t kd, z, r, r2, scale, tail, tmp;

	abstop = top12(x) & 0x7ff;
	float64_t onePN54 = {0x3c90000000000000}; // 0x1p-54
	// float64_t y = {0x4094480000000000}; // 512.0
	if (predict_false(abstop - top12(onePN54) >=
			  top12(cast_int64(0x4094480000000000)) - top12(onePN54))) {
		if (abstop - top12(onePN54) >= 0x80000000) {
			/* Avoid spurious underflow for tiny x.  */
			/* Note: 0 is common input.  */
			float64_t one = WANT_ROUNDING ? f64_add(ui32_to_f64(1), x) : ui32_to_f64(1);
			return sign_bias ? f64_negate(one) : one;
		}
		if (abstop >= top12(cast_int64(0x40b0240000000000))) {
			/* Note: inf and nan are already handled.  */
			if (x.v >> 63)
			  // Underflow to
				return ui32_to_f64(0);
				// __kadena_math_uflow(sign_bias);
			else
				return cast_int64(0x7FF0000000000000); // Overflow to +inf
				// __kadena_math_/oflow(sign_bias);
		}
		/* Large x is special cased below.  */
		abstop = 0;
	}

	/* exp(x) = 2^(k/N) * exp(r), with exp(r) in [2^(-1/2N),2^(1/2N)].  */
	/* x = ln2/N*k + r, with int k and r in [-ln2/2N, ln2/2N].  */
	z = f64_mul(InvLn2N, x); // InvLn2N * x;
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
	ki = kd.v;
	kd = f64_sub(kd, Shift); // -= Shift;
#endif
	r = f64_mulAdd(kd, NegLn2loN, f64_mulAdd(kd, NegLn2hiN, x)); // x + kd * NegLn2hiN + kd * NegLn2loN;
	/* The code assumes 2^-200 < |xtail| < 2^-8/N.  */
	r = f64_add(r, xtail); // += xtail;
	/* 2^(k/N) ~= scale * (1 + tail).  */
	idx = 2 * (ki % N);
	top = (ki + sign_bias) << (52 - EXP_TABLE_BITS);
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
	return f64_mulAdd(scale, tmp, scale); // return eval_as_double(scale + scale * tmp);
}

/* Returns 0 if not int, 1 if odd int, 2 if even int.  The argument is
   the bit representation of a non-zero finite floating-point value.  */
static inline int checkint(uint64_t iy)
{
	int e = iy >> 52 & 0x7ff;
	if (e < 0x3ff)
		return 0;
	if (e > 0x3ff + 52)
		return 2;
	if (iy & ((1ULL << (0x3ff + 52 - e)) - 1))
		return 0;
	if (iy & (1ULL << (0x3ff + 52 - e)))
		return 1;
	return 2;
}

/* Returns 1 if input is the bit representation of 0, infinity or nan.  */
static inline int zeroinfnan(uint64_t i)
{
	return 2 * i - 1 >= 2 * asuint64(INFINITY) - 1;
}


float64_t musl_powf64(float64_t x, float64_t y)
{
	uint32_t sign_bias = 0;
	uint64_t ix, iy;
	uint32_t topx, topy;

	ix = x.v;
	iy = y.v;
	topx = top12(cast_int64(ix));
	topy = top12(cast_int64(iy));
	if (predict_false(topx - 0x001 >= 0x7ff - 0x001 ||
			  (topy & 0x7ff) - 0x3be >= 0x43e - 0x3be)) {
		/* Note: if |y| > 1075 * ln2 * 2^53 ~= 0x1.749p62 then pow(x,y) = inf/0
		   and if |y| < 2^-54 / 1075 ~= 0x1.e7b6p-65 then pow(x,y) = +-1.  */
		/* Special cases: (x < 0x1p-126 or inf or nan) or
		   (|y| < 0x1p-65 or |y| >= 0x1p63 or nan).  */
		if (predict_false(zeroinfnan(iy))) {
			if (2 * iy == 0)
				return ui32_to_f64(1);
			if (ix == asuint64(1.0))
				return ui32_to_f64(1);
			if (2 * ix > 2 * asuint64(INFINITY) ||
			    2 * iy > 2 * asuint64(INFINITY))
				return f64_add(x, y);
			if (2 * ix == 2 * asuint64(1.0))
				return ui32_to_f64(1);
			if ((2 * ix < 2 * asuint64(1.0)) == !(iy >> 63))
				return ui32_to_f64(0); /* |x|<1 && y==inf or |x|>1 && y==-inf.  */
			return f64_mul(y, y);
		}
		if (predict_false(zeroinfnan(ix))) {
			float64_t x2 = f64_mul(x, x);
			if (ix >> 63 && checkint(iy) == 1)
				x2 = f64_negate(x2);
			/* Without the barrier some versions of clang hoist the 1/x2 and
			   thus division by zero exception can be signaled spuriously.  */
			return iy >> 63 ? f64_div(ui32_to_f64(1), x2) : x2;
		}
		/* Here x and y are non-zero finite.  */
		if (ix >> 63) {
			/* Finite x < 0.  */
			int yint = checkint(iy);
			if (yint == 0)
				return __kadena_math_invalid_sf(x);
			if (yint == 1)
				sign_bias = SIGN_BIAS;
			ix &= 0x7fffffffffffffff;
			topx &= 0x7ff;
		}
		if ((topy & 0x7ff) - 0x3be >= 0x43e - 0x3be) {
			/* Note: sign_bias == 0 here because y is not odd.  */
			if (ix == asuint64(1.0))
				return ui32_to_f64(1);
			if ((topy & 0x7ff) < 0x3be) {
				/* |y| < 2^-65, x^y ~= 1 + y*log(x).  */
				if (WANT_ROUNDING)
					return ix > asuint64(1.0) ? f64_add(ui32_to_f64(1), y) :
								    f64_sub(ui32_to_f64(1), y);
				else
					return ui32_to_f64(1);
			}
			return (ix > asuint64(1.0)) == (topy < 0x800) ?
				       __kadena_math_oflow_sf(0) :
				       __kadena_math_uflow_sf(0);
		}
		if (topx == 0) {
			/* Normalize subnormal x so exponent becomes negative.  */
			ix = f64_mul(x, cast_int64(0x4330000000000000)).v;
			ix &= 0x7fffffffffffffff;
			ix -= 52ULL << 52;
		}
	}

	float64_t lo;
	float64_t hi = log_inline(ix, &lo);
	float64_t ehi, elo;
// #if __FP_FAST_FMA
// 	ehi = y * hi;
// 	elo = y * lo + __builtin_fma(y, hi, -ehi);
// #else
	float64_t yhi = cast_int64(iy & -1ULL << 27);
	float64_t ylo = f64_sub(y, yhi); //y - yhi;
	float64_t lhi = cast_int64(hi.v & -1ULL << 27);
	float64_t llo = f64_add(f64_sub(hi, lhi), lo); // hi - lhi + lo;
	ehi = f64_mul(yhi, lhi); // yhi * lhi;
	elo = f64_mulAdd(ylo, yhi, f64_mul(y, llo)); // ylo * lhi + y * llo; /* |elo| < |ehi| * 2^-25.  */
// #endif
	return exp_inline(ehi, elo, sign_bias);
}

double musl_pow(double x, double y) {
	float64_t out = musl_powf64(cast_int64(asuint64(x)), cast_int64(asuint64(y)));
	return asdouble(out.v);
}
