/*
 * Copyright (c) 2018, Arm Limited.
 * SPDX-License-Identifier: MIT
 */
#ifndef _EXP_DATA_H
#define _EXP_DATA_H

/*#include <features.h>*/
#include <stdint.h>
#include "../softfloat/softfloat.h"

#define EXP_TABLE_BITS 7
#define EXP_POLY_ORDER 5
#define EXP_USE_TOINT_NARROW 0
#define EXP2_POLY_ORDER 5
extern const struct exp_data {
	float64_t invln2N;
	float64_t shift;
	float64_t negln2hiN;
	float64_t negln2loN;
	float64_t poly[4]; /* Last four coefficients.  */
	float64_t exp2_shift;
	float64_t exp2_poly[EXP2_POLY_ORDER];
	uint64_t tab[2*(1 << EXP_TABLE_BITS)];
} __kadena_exp_data;

#endif
