/*
 * Copyright 2011 Frank Terbeck <ft@bewatermyfriend.org>, All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * AUTHOR OR CONTRIBUTORS OF THE PROJECT BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * @file scm-helpers.c
 * @brief Scheme code helper functions
 */

#include <libguile.h>

#include "scm-helpers.h"

char *
cdce_scm2string(SCM s)
{
    char *buf;

    buf = scm_to_locale_string(s);
    if (buf == NULL) {
        (void)printf("scm2string(): Failed to convert scheme-string to C.\n");
        return NULL;
    }
    return buf;
}

int
cdce_scm_variable_exists(char *name)
{
    if (name == NULL)
        return 0;

    return
        (scm_sym2var(scm_from_locale_symbol(name),
                     scm_current_module_lookup_closure(),
                     SCM_BOOL_F)
         != SCM_BOOL_F)
        ? 1
        : 0;
}

int
cdce_scm_bool_var(char *name)
{
    SCM obj;

    if (!cdce_scm_variable_exists(name))
        return 0;

    obj = scm_variable_ref(scm_c_lookup(name));
    if (!scm_is_bool(obj) || scm_is_false(obj))
        return 0;

    return 1;
}

unsigned long int
cdce_scm_ulong_var(char *name, unsigned long int defval)
{
    SCM obj;

    if (!cdce_scm_variable_exists(name))
        return defval;

    obj = scm_variable_ref(scm_c_lookup(name));
    if (!scm_is_integer(obj))
        return defval;

    return scm_to_ulong(obj);
}

uint32_t
cdce_scm_to_uint32(SCM value, char *name, int *err)
{
    if (!scm_is_unsigned_integer(value, 0, UINT32_MAX)) {
        (void)printf("scm2uint32: `%s' must be uint32_t.\n", name);
        *err = 1;
        return 0;
    }

    *err = 0;
    return (uint32_t)scm_to_uint32(value);
}
