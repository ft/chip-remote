/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file chip-remote.c
 * @brief Main entry point for chip-remote operation
 *
 * This implementation uses bit-banging for implementing slave protocols such
 * as SPI. While this has a performance hit (since all operation has to be
 * carried out in software), it increases portability since it only relies on
 * the existence of general purpose IO pins.
 *
 * Other implementations may choose to leverage hardware-implementations of
 * these interfaces, should a given microcontroller support these.
 *
 * The main operation has to take care of these tasks:
 *
 * 1. Is the chip-remote mode active? If not, assume that some other process is
 *    running and do nothing. Otherwise...
 *
 * 2. Read bytes from the communication link to the host computer. And store
 *    them in a line buffer.
 *
 * 3. If there is data in the line buffer, process it and generate a reply.
 *
 * 4. If a reply has been prepared, send it to the host computer.
 *
 * This operation is a fairly simple task. What requires a bit of though is to
 * determine which parts of the resulting code can be kept the same entirely
 * for any platform, that would ever be supported.
 *
 * Task number "1" is trivial.
 *
 * Most of task number "3" is possible in a completely portable manner, since
 * it consumes an existing string of bytes and a state and produces another
 * string of bytes. Nothing about that deals with platform specific features.
 * But while processing requests input strings (the "TRANSMIT" request), side
 * effects are triggered: Namely, communication with a slave device is
 * performed. This requires the state of actual pins on a micro-controller to
 * change, which is a platform-dependent operation.
 *
 * The remaining tasks deal with actually receiving and sending bytes. This,
 * again, is a platform-dependent task.
 *
 * Platform dependent work for task "3": This is mainly about changing the
 * value of a physical pin on a microcontroller. So, the platform dependent
 * code needs to provide functions like `set_pin', 'clear_pin' and
 * `toggle_pin', that consume a port number and a pin number and carry out the
 * intended operation.
 *
 * The platform dependent work for tasks "2" and "4" are different. They
 * consume bytes, since that is what is exchanged with the host PC. Since we
 * cannot determine when bytes arrive exactly, the platform dependent code
 * works on strings to allow for buffering multiple bytes that arrive. The
 * receiving process is to fill a buffer with a NUL terminated ASCII string and
 * set a flag when that is done. The sending process takes a string and sends
 * it away (technically, the sending process could do with a send_byte routine,
 * but we opt for a string based API for symmetry with the receiving process).
 *
 * Line buffering is enough, since the protocol ensures that neither the
 * controller nor the controlling client ever sends more than line without the
 * reply from the other end of the communication channel.
 *
 * Convention: "cr_" is the prefix for platform independent code; "xcr_" for
 *             platform-dependent code.
 *
 * In `chip-remote', a port defines one physical connection on the remote micro
 * controller, that can be used to talk to a configurable chip. The chip-remote
 * protocol allows for multiple ports on a controller, of which each is
 * configurable with respect to the type of bus it implements.
 *
 * While the reference implementation supports this kind of configurability,
 * the protocol defines it as optional. So other implementations may just
 * decide to have on fixed port that talks - for example - SPI of a fixed
 * variety to a single chip. This reduces complexity and the need to initialise
 * the remote controller with the right set of parameters required for
 * successful operation.
 *
 * A configurable port has to be explicitly initialised before it is put to use
 * and after any configuration changes.
 */

#include <stdlib.h>

#include "chip-remote.h"
#include "buf-parse.h"
#include "protocol.h"
#include "requests.h"
#include "utils.h"

char rxbuf[CR_MAX_LINE + 1];
static struct cr_state cr_data;

static int cr_check_args(enum cr_request_ids, struct cr_words *);
static int cr_in_conv_process(void);
static int cr_no_conv_process(void);
static int cr_multi_line_process(enum cr_request_ids, struct cr_words *);
static void cr_process(void);

/* new optional request macro */
#define NOR(string, id, state, cb, minargs, maxargs) \
    [id] = { string, id, state, cb, { minargs, maxargs }, 1 }

/* new mandatory request macro */
#define NMR(string, id, state, cb, minargs, maxargs) \
    [id] = { string, id, state, cb, { minargs, maxargs }, 0 }

struct cr_request requests[MAX_REQUEST + 1] = {
    NMR("BYE", REQUEST_BYE, CR_SINGLE_LINE, cr_handle_bye, 0, 0),
    NMR("FEATURES", REQUEST_FEATURES, CR_MULTI_LINE, cr_handle_features, 0, 0),
    NOR("FOCUS", REQUEST_FOCUS, CR_SINGLE_LINE, cr_handle_focus, 1, 1),
    NMR("HI", REQUEST_HI, CR_SINGLE_LINE, cr_handle_hi, 0, 0),
    NOR("INIT", REQUEST_INIT, CR_SINGLE_LINE, cr_handle_init, 1, 1),
    NOR("LINES", REQUEST_LINES, CR_MULTI_LINE, cr_handle_lines, 1, 1),
    NOR("LINE", REQUEST_LINE, CR_SINGLE_LINE, cr_handle_line, 3, 3),
    NOR("MODES", REQUEST_MODES, CR_MULTI_LINE, cr_handle_modes, 0, 0),
    NOR("PORT", REQUEST_PORT, CR_MULTI_LINE, cr_handle_port, 1, 1),
    NOR("PORTS", REQUEST_PORTS, CR_MULTI_LINE, cr_handle_ports, 0, 0),
    NOR("SET", REQUEST_SET, CR_SINGLE_LINE, cr_handle_set, 3, 3),
    NMR("TRANSMIT", REQUEST_TRANSMIT, CR_SINGLE_LINE, cr_handle_transmit, 1, 1),
    NMR("VERSION", REQUEST_VERSION, CR_SINGLE_LINE, cr_handle_version, 0, 0),
    NMR(NULL, MAX_REQUEST, CR_SINGLE_LINE, NULL, 0, 0)
};

enum cr_request_ids
cr_string_to_request(struct cr_words *words, size_t idx)
{
    int i;

    for (i = 0; requests[i].request != NULL; ++i)
        if (cr_word_eq(words, idx, requests[i].request))
            return requests[i].id;

    return MAX_REQUEST;
}

static int
cr_check_args(enum cr_request_ids req, struct cr_words *words)
{
    size_t argc;

    argc = words->count - 1;
    if (argc < requests[req].args.min) {
        cr_fail("Too few arguments");
        return 0;
    }
    if (requests[req].args.max < 0)
        return 1;
    if (argc > requests[req].args.max) {
        cr_fail("Too many arguments");
        return 0;
    }
    return 1;
}

static int
cr_multi_line_process(enum cr_request_ids id, struct cr_words *words)
{
#define INACTIVE 0
#define ACTIVE 1
    static int state = INACTIVE;
    static int cnt = 0;

    switch (state) {
    case INACTIVE:
        cnt = 0;
        state = ACTIVE;
        /* FALLTHROUGH */
    default:
        if (cnt > 0 && !cr_word_eq(words, 0, "MORE")) {
            cr_fail("Unknown request in multiline state");
            state = INACTIVE;
            return 1;
        }
        if (requests[id].cb(cnt, words)) {
            state = INACTIVE;
            xcr_send_host(DONE_REPLY);
            return 1;
        }
        cnt++;
    }
    return 0;
#undef INACTIVE
#undef ACTIVE
}

static int
cr_is_multi_line(enum cr_request_ids id)
{
    return (requests[id].state == CR_MULTI_LINE);
}

static int
cr_in_conv_process(void)
{
    static enum cr_conv_states state = CR_SINGLE_LINE;
    static enum cr_request_ids curreq = MAX_REQUEST;
    struct cr_words words;

    cr_split_request(rxbuf, &words);
    switch (state) {
    case CR_SINGLE_LINE:
        if (cr_word_eq(&words, 0, "HI")) {
            cr_fail("Unexpected command");
            return 0;
        }
        curreq = cr_string_to_request(&words, 0);
        if (words.count == 0) {
            cr_fail("Empty request");
            break;
        }
        if (curreq == MAX_REQUEST) {
            cr_fail("Unknown request");
            break;
        }

        if (!cr_check_args(curreq, &words))
            return 0;

        if (cr_is_multi_line(curreq)) {
            state = CR_MULTI_LINE;
            if (cr_multi_line_process(curreq, &words))
                state = CR_SINGLE_LINE;
        } else
            requests[curreq].cb(0, &words);

        if (curreq == REQUEST_BYE) {
            xcr_post_bye();
            return 1;
        }

        break;
    case CR_MULTI_LINE:
        if (cr_multi_line_process(curreq, &words))
            state = CR_SINGLE_LINE;

        break;
    }
    return 0;
}

static int
cr_no_conv_process(void)
{
    static struct cr_words words;

    cr_split_request(rxbuf, &words);
    /* The no-conversation state has no sub-states. */
    if (cr_word_eq(&words, 0, "HI")) {
        if (!cr_check_args(REQUEST_HI, &words))
            return 0;
        cr_handle_hi(0, &words);
        return 1;
    }

    cr_fail("Unexpected command");
    return 0;
}

static void
cr_process(void)
{
    static enum cr_top_states state = CR_NO_CONVERSATION;

    switch (state) {
    case CR_NO_CONVERSATION:
        if (cr_no_conv_process())
            state = CR_IN_CONVERSATION;
        break;
    case CR_IN_CONVERSATION:
        if (cr_in_conv_process())
            state = CR_NO_CONVERSATION;
        break;
    }
    cr_set_line_pending(0);
}

/**
 * Main entry-point for the chip-remote state machine
 *
 * @return void
 */
void
cr_top_level(void)
{
    xcr_pre_top_level();
    if (!cr_data.cr_active)
        return;
    if (cr_data.line_pending)
        cr_process();
}

/**
 * Initialise device for chip-remote operation.
 *
 * @return void
 */
void
cr_init(int activate)
{
    cr_data.cr_active = activate;
    cr_data.line_pending = 0;
    cr_data.fport = -1;
}

void
cr_set_active(int value)
{
    cr_data.cr_active = value;
}

void
cr_set_line_pending(int value)
{
    cr_data.line_pending = value;
}

void
cr_set_focused_port(int value)
{
    cr_data.fport = value;
}

int
cr_get_focused_port(void)
{
    return cr_data.fport;
}
