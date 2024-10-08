#include "libm.h"
#include "../softfloat/softfloat.h"

double __kadena_math_invalid(double x)
{
	return (x - x) / (x - x);
}


float64_t __kadena_math_invalid_sf(float64_t x)
{
	return f64_div(f64_sub(x, x), f64_sub(x, x));
}
