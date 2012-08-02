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
 * @file chip-remote-scm.c
 * @brief Scheme API for chip-remote
 *
 * The basic procedures are `cr/open', `cr/close', which open/close the serial
 * device file, `cr/hi' and `cr/bye', which initiate and terminate a
 * conversation with the serial device.
 */

#include <stdio.h>
#include <stdlib.h>

#include <libguile.h>

#include "chip-remote-scm.h"
#include "common.h"
#include "scm-helpers.h"
#include "serial.h"

static void cr_scm_module(UNUSED void *);

SCM
cr_scm_close(UNUSED SCM x)
{
    if (!serial_close()) {
        return SCM_BOOL_F;
    }
    return SCM_BOOL_T;
}

SCM
cr_scm_open(SCM device)
{
    char *buf;
    SCM rc = SCM_BOOL_F;

    if (scm_string_p(device) == SCM_BOOL_F) {
        (void)printf("cr/open: `device' must be a string.\n");
        return SCM_BOOL_F;
    } else if (scm_string_null_p(device) == SCM_BOOL_T) {
        (void)printf("cr/open: `device' must be non-empty.\n");
        return SCM_BOOL_F;
    }

    buf = cr_scm2string(device);
    if (buf == NULL)
        goto done;
    if (!serial_open(buf))
        goto done;

    rc = SCM_BOOL_T;

done:
    free(buf);
    return rc;
}

SCM
cr_scm_read_raw(UNUSED SCM x)
{
    char reply[SERIAL_BUF_MAX + 1];

    if (!serial_read(reply))
        return SCM_BOOL_F;

    return scm_from_locale_string(reply);
}

SCM
cr_scm_write_raw(SCM data)
{
    char *buf;
    SCM rc = SCM_BOOL_F;

    buf = NULL;
    if (scm_string_p(data) == SCM_BOOL_F) {
        (void)printf("cdce/open: `data' must be a string.\n");
        goto done;
    }
    buf = cr_scm2string(data);

    if (buf == NULL)
        goto done;
    if (!serial_write(buf))
        goto done;

    rc = SCM_BOOL_T;

done:
    free(buf);
    return rc;
}

static struct cr_scm_proctab {
    const char *name;
    SCM (*cb)(SCM);
    int req;
    int opt;
    int rest;
} pt[] = {
    { "cr/close", cr_scm_close, 0, 0, 0 },
    { "cr/open", cr_scm_open, 1, 0, 0 },
    { "cr/read-raw", cr_scm_read_raw, 0, 0, 0 },
    { "cr/write-raw", cr_scm_write_raw, 1, 0, 0 },
    { (char *)NULL, NULL, 0, 0, 0 }
};

static void
cr_scm_module(UNUSED void *data)
{
    int i;

    scm_c_define("cr/options:trace", SCM_BOOL_F);
    scm_c_export("cr/options:trace", NULL);
    scm_c_define("cr/options:serial-timeout", scm_from_ulong(SERIAL_TIMEOUT));
    scm_c_export("cr/options:serial-timeout", NULL);
    scm_c_define("cr/last-reply", scm_from_locale_string(""));
    scm_c_export("cr/last-reply", NULL);
    for (i = 0; pt[i].name != NULL; ++i) {
        scm_c_define_gsubr(pt[i].name,
                           pt[i].req,
                           pt[i].opt,
                           pt[i].rest,
                           pt[i].cb);
        scm_c_export(pt[i].name, NULL);
    }
}

void
cr_scm_init(void)
{
    SCM lp;

    scm_c_define_module("chip-remote-primitives",
                        cr_scm_module,
                        NULL);
    scm_c_use_module("chip-remote-primitives");
    /*
     * Add an additional `%load-path' entry for our own modules.
     *
     * We *only* set this if the environment variable `$GUILE_LOAD_PATH' is not
     * set, because it should be able to overrule our compile-time choice. In
     * particular, our test-suite sets that variable to the source tree's
     * `scheme/' directory, so that during tests the modules from the source
     * tree are used instead of the ones that might be installed system-wide.
     */
    if (getenv("GUILE_LOAD_PATH") == NULL) {
        lp = scm_c_lookup("%load-path");
        scm_variable_set_x(
            lp,
            scm_append(
                scm_list_2(scm_list_1(scm_from_locale_string(
                                          CHIP_REMOTE_LOAD_PATH)),
                           scm_variable_ref(lp))));
    }
}
