#include "libm.h"
#include "../softfloat/softfloat.h"

double __kadena_math_oflow(uint32_t sign)
{
	return __kadena_math_xflow(sign, 0x1p769);
}

float64_t __kadena_math_oflow_sf(uint32_t sign)
{
	float64_t out = {0x7FF0000000000000};
	return out; //+inf
}

// double __kadena_math_xflow_sf(uint32_t sign, double y)
// {
// 	return eval_as_double(fp_barrier(sign ? -y : y) * y);
// }
