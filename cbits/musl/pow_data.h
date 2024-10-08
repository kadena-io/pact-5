/*
 * Copyright (c) 2018, Arm Limited.
 * SPDX-License-Identifier: MIT
 */
#ifndef _POW_DATA_H
#define _POW_DATA_H
#include "../softfloat/softfloat.h"

/*#include <features.h>*/

#define POW_LOG_TABLE_BITS 7
#define POW_LOG_POLY_ORDER 8
extern const struct pow_log_data {
	float64_t ln2hi;
	float64_t ln2lo;
	float64_t poly[POW_LOG_POLY_ORDER - 1]; /* First coefficient is 1.  */
	/* Note: the pad field is unused, but allows slightly faster indexing.  */
	struct {
		float64_t invc, pad, logc, logctail;
	} tab[1 << POW_LOG_TABLE_BITS];
} __kadena_pow_log_data;

#endif
