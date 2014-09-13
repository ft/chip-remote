/*
 * Copyright (c) 2013 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file requests.c
 * @brief Request handling callbacks for chip-remote.c
 *
 * When the chip-remote main loop encounters a request from the host computer,
 * it examines it, by checking if it knows the request and if the number of
 * arguments given makes sense. (It does *not* type-check the arguments.)
 *
 * When it is satisfied it calls the correct handler from this file to actually
 * handle the request. All these functions have the same signature, to be
 * viable as callback-pointers:
 *
 *     int cr_handle_something(int cnt, struct cr_words *words);
 *
 * The `words' parameter points to the original request split up into words (by
 * code from buf-parse.c). This is obviously required to use arguments, that
 * the user has passed with the request.
 *
 * For requests, that case a single reply (those marked CR_SINGLE_LINE), that
 * is everything there is to know. The `cnt' parameter is always set to "0" and
 * the return value is ignored.
 *
 * With multi-line requests (those marked CR_MULTI_LINE), that changes: The
 * return value is important. It signals the dispatching process, if the
 * multi-line request is finished. So, multi-line handlers return 0 if they
 * need to return more lines and they return 1 (or rather anything non-zero) to
 * tell the dispatcher, that it is done. The dispatcher takes care of returning
 * a DONE_REPLY to the host computer.
 *
 * The `cnt' paramater tells multi-line request handler how often it was called
 * before in handling the request. Thus, when a multi-line handler is called
 * for the first time, `cnt' is zero. The next time it is incremented to one,
 * then to two and so on.
 */


#include <string.h>

#include "chip-remote.h"
#include "buf-parse.h"
#include "port.h"
#include "parameters.h"
#include "protocol.h"
#include "proto-utils.h"
#include "requests.h"
#include "transmit.h"
#include "utils.h"

static char *cr_modes[] = {
    [ CR_MODE_NONE ] = "NONE",
    [ CR_MODE_PAREX ] = "PAR-EX",
    [ CR_MODE_SPI ] = "SPI",
    [ CR_MODE_INVALID ] = (char *)NULL
};

static enum cr_port_modes
cr_word2mode(struct cr_words *words, size_t idx)
{
    int i;

    for (i = 0; cr_modes[i] != NULL; ++i)
        if (cr_word_eq(words, idx, cr_modes[i]))
            break;
    if (i > CR_MODE_INVALID)
        i = CR_MODE_INVALID;
    return (enum cr_port_modes)i;
}

static int
cr_return_list(int cnt, char *list[])
{
    if (list[cnt] == (char *)NULL)
        return 1;
    xcr_send_host(list[cnt]);
    return 0;
}

static size_t
cr_numofports(struct cr_port *ports)
{
    size_t i;
    for (i = 0; ports[i].lines != 0; ++i)
        /* NOP */;
    return i;
}

static uint32_t
verify_word_is_int(struct cr_words *words, size_t idx, int *error)
{
    uint32_t rc;
    int err;

    if (words->word[idx].length > CR_INT_MAX_LEN)
        goto broken_value;

    rc = str2uint(words->word[idx].start, words->word[idx].length, &err);
    if (err)
        goto broken_value;

    *error = 0;
    return rc;
broken_value:
    cr_broken_value(words, idx);
    *error = 1;
    return 0;
}

static int
verify_fits_numofports(struct cr_words *words, size_t num)
{
    uint32_t idx;
    int err;

    idx = verify_word_is_int(words, num, &err);
    if (err)
        return 0;

    if (idx >= cr_numofports(cr_ports)) {
        cr_uint_oor(idx);
        return 0;
    }

    return 1;
}

int
cr_handle_address(int cnt, struct cr_words *words)
{
    struct cr_port *port;
    uint32_t addr;
    int err;

    addr = verify_word_is_int(words, 1, &err);
    if (err)
        return 0;
    port = cr_ports + cr_get_focused_port();
    cr_port_address(port, addr);
    return 1;
}

int
cr_handle_features(int cnt, struct cr_words *words)
{
    int i, n;

    for (i = 0, n = 0; requests[i].request != NULL; ++i)
        if (requests[i].optional) {
            if (n == cnt) {
                xcr_send_host(requests[i].request);
                return 0;
            }
            n++;
        }

    return 1;
}

static void
cr_assign_line_role_string(uint32_t port_idx, uint32_t line_idx,
                           struct cr_words *words, int word_idx)
{
    size_t len;
    struct cr_line *l;
    struct cr_word *w;

    w = words->word + word_idx;
    l = cr_ports[port_idx].l + line_idx;
    len = w->length > CR_MAX_ROLE_STRING ? CR_MAX_ROLE_STRING : w->length;
    strncpy(l->rolestr, w->start, len);
    l->rolestr[len] = '\0';
}

int
cr_handle_line(int cnt, struct cr_words *words)
{
    uint32_t pi, li, max;
    int err;

    pi = verify_word_is_int(words, 1, &err);
    if (err)
        return 0;

    max = cr_numofports(cr_ports);
    if (pi >= max) {
        cr_uint_oor(pi);
        return 0;
    }

    li = verify_word_is_int(words, 2, &err);
    if (err)
        return 0;
    if (li >= cr_ports[pi].lines) {
        cr_uint_oor(li);
        return 0;
    }

    cr_assign_line_role_string(pi, li, words, 3);

    xcr_send_host(OK_REPLY);
    return 0;
}

int
cr_handle_lines(int cnt, struct cr_words *words)
{
    static uint32_t idx;

    if (cnt == 0 && !verify_fits_numofports(words, 1))
        return 1;
    if (cnt == 0) {
        int err;
        idx = str2uint(words->word[1].start, words->word[1].length, &err);
    }

    if (cnt >= cr_ports[idx].lines)
        return 1;

    cr_echo_line(idx,
                 cnt,
                 cr_ports[idx].l[cnt].role,
                 cr_ports[idx].l[cnt].index,
                 cr_ports[idx].l[cnt].mutable_p);

    return 0;
}

int
cr_handle_modes(int cnt, struct cr_words *words)
{
    return cr_return_list(cnt, cr_modes);
}

int
cr_handle_port(int cnt, struct cr_words *words)
{
    static uint32_t idx;

    if (cnt == 0 && !verify_fits_numofports(words, 1))
        return 1;
    if (cnt == 0) {
        int err;
        idx = str2uint(words->word[1].start, words->word[1].length, &err);
    }

    if (cnt == 0) {
        cr_echo_mode(cr_ports, idx);
        return 0;
    } else if (cnt == 1) {
        cr_echo_lines(cr_ports, idx);
        return 0;
    } else if (cnt == 2) {
        cr_echo_rate(cr_ports, idx);
        return 0;
    }

    if (cr_ports[idx].params != NULL) {
        cnt -= 3;
        if (cr_ports[idx].params[cnt].name != NULL) {
            cr_echo_parameter(&(cr_ports[idx].params[cnt]));
            return 0;
        }
    }
    return 1;
}

int
cr_handle_ports(int cnt, struct cr_words *words)
{
    if (cnt == 0) {
        cr_echo_ports(cr_numofports(cr_ports));
        return 0;
    } else if (cnt == 1) {
        cr_echo_focus(cr_get_focused_port());
        return 0;
    }
    return 1;
}

int
cr_handle_focus(int cnt, struct cr_words *words)
{
    uint32_t idx, max;
    int err;

    idx = verify_word_is_int(words, 1, &err);
    if (err)
        return 0;

    max = cr_numofports(cr_ports);
    if (idx > max) {
        cr_uint_oor(idx);
        return 0;
    }

    cr_set_focused_port((int)idx);
    xcr_send_host(OK_REPLY);
    return 1;
}

int
cr_handle_hi(int cnt, struct cr_words *words)
{
    xcr_send_host(HI_REPLY);
    return 1;
}

int
cr_handle_init(int cnt, struct cr_words *words)
{
    uint32_t idx, max;
    int err;

    idx = verify_word_is_int(words, 1, &err);
    if (err)
        return 0;
    max = cr_numofports(cr_ports);
    if (idx >= max) {
        cr_uint_oor(idx);
        return 0;
    }
    cr_init_port(&(cr_ports[idx]));
    return 1;
}

int
cr_handle_bye(int cnt, struct cr_words *words)
{
    xcr_send_host(BYE_REPLY);
    return 1;
}

int
cr_handle_set(int cnt, struct cr_words *words)
{
    uint32_t idx, max;
    enum cr_port_modes mode;
    int err, rc;

    idx = verify_word_is_int(words, 1, &err);
    if (err)
        return 0;

    max = cr_numofports(cr_ports);
    if (idx >= max) {
        cr_uint_oor(idx);
        return 0;
    }

    if (cr_word_eq(words, 2, "MODE")) {
        mode = cr_word2mode(words, 3);
        if (mode == CR_MODE_INVALID) {
            cr_broken_value(words, 3);
            return 0;
        }
        err = cr_port_mode_set(&(cr_ports[idx]), mode);
        switch (err) {
        case 0x1:
            cr_fail("Cannot configure non-configurable port.");
            break;
        case 0x2:
            cr_fail("Firmware out-of-memory.");
            break;
        case 0x4:
            cr_fail("Did not set transmission function.");
            break;
        default:
            xcr_send_host(OK_REPLY);
        }
        return 1;
    }
    rc = cr_param_set(cr_ports[idx].params, words, 2, 3);
    tx_init();
    if (rc == 0) {
        tx_add(WTF_REPLY);
        tx_add(" SET: Non-mutable parameter: ");
        tx_add_word(words, 2);
    } else if (rc < 0) {
        tx_add(WTF_REPLY);
        tx_add(" SET: Unknown parameter ");
        tx_add_word(words, 2);
    } else
        tx_add(OK_REPLY);
    tx_trigger();
    return 1;
}

int
cr_handle_transmit(int cnt, struct cr_words *words)
{
    uint32_t value, read_value;
    int err;

    value = verify_word_is_int(words, 1, &err);
    if (err)
        return 0;
    err = cr_transmit(value, &read_value);
    if (err < 0)
        return 1;
    tx_init();
    tx_add_integer(read_value);
    tx_trigger();
    return 1;
}

int
cr_handle_version(int cnt, struct cr_words *words)
{
    xcr_send_host(VERSION_REPLY);
    return 1;
}
