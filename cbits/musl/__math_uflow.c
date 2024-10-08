#include "libm.h"
#include "../softfloat/softfloat.h"

double __kadena_math_uflow(uint32_t sign)
{
	return __kadena_math_xflow(sign, 0x1p-767);
}

float64_t __kadena_math_uflow_sf(uint32_t sign)
{
	// overflow towards 0
	return ui32_to_f64(0);
}
