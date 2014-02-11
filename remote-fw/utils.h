/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_UTILS_H
#define INC_UTILS_H

#include <stdint.h>
#include <string.h>

#define STREQ(x, y) (!strcmp(x, y))
#define STREQ_N(x, y, n) (!strncmp(x, y, n))
#define _(x) ((const char *)x)
#define xmalloc(n, t) ((t *)malloc(n * sizeof(t)))
#define INT_BIT(n) ((int) 1 << n)
#define INT_BIT_SET_P(data,n) (!!(data & INT_BIT(n)))
#define INT_BIT_SET(data,n) ((data) |= INT_BIT(n))
#define INT_BIT_CLEAR(data,n) ((data) &= ~(INT_BIT(n)))

#define BITMASK_CLEAR(DEST,MASK) ((DEST) &= ~(MASK))
#define BITMASK_SET(DEST,MASK) ((DEST) |= (MASK))
#define BITMASK_TOGGLE(DEST,MASK) ((DEST) ^= (MASK))
#define IS_AT_LEAST_ONE_BIT_SET(DEST,MASK) ((DEST) & (MASK))
#define IS_AT_LEAST_ONE_BIT_UNSET(DEST,MASK) ((~DEST) & (MASK))
#define IS_BITMASK_SET(DEST,MASK) (((DEST) & (MASK)) == (MASK))
#define IS_BITMASK_UNSET(DEST,MASK) (((~DEST) & (MASK)) == (MASK))

uint32_t str2uint(const char *, int, int *);
void uint2str(uint32_t, char *);

#endif /* INC_UTILS_H */
