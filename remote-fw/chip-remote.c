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
 */

#include <stdlib.h>

#include "chip-remote.h"
#include "platform.h"
#include "port.h"
#include "protocol.h"
#include "utils.h"

char rxbuf[CR_MAX_LINE + 1];
static struct cr_state cr_data;

enum cr_top_states {
    CR_IN_CONVERSATION,
    CR_NO_CONVERSATION
};

enum cr_conv_states {
    CR_SINGLE_LINE,
    CR_MULTI_LINE
};

enum cr_multi_line_states {
    CR_ML_FEATURES = 0,
    CR_ML_LINES,
    CR_ML_MODES,
    CR_ML_PORTS,
    CR_ML_NONE
};

static char *cr_features[] = {
    "FOCUS",
    "LINES",
    "MODES",
    "PORTS",
    (char *)NULL
};

static char *cr_modes[] = {
    "SPI",
    (char *)NULL
};

static enum cr_multi_line_states cr_word2state(struct cr_words *, int);
static int cr_multi_line_process(struct cr_words *);
static int cr_is_multi_line(struct cr_words *);
static int cr_in_conv_process(void);
static int cr_no_conv_process(void);
static int cr_check_args(enum cr_requests, struct cr_words *);

static int cr_ml_handle_features(int, struct cr_words *);
static int cr_ml_handle_lines(int, struct cr_words *);
static int cr_ml_handle_modes(int, struct cr_words *);
static int cr_ml_handle_ports(int, struct cr_words *);
static void cr_handle_focus(struct cr_words *);
static void cr_process(void);

typedef int (*cr_ml_jump_table)(int cnt, struct cr_words *words);

static const cr_ml_jump_table ml_jump_table[CR_ML_NONE] = {
    [CR_ML_FEATURES] = cr_ml_handle_features,
    [CR_ML_LINES] = cr_ml_handle_lines,
    [CR_ML_MODES] = cr_ml_handle_modes,
    [CR_ML_PORTS] = cr_ml_handle_ports
};

static struct cr_args cr_arg_defs[MAX_REQUEST] = {
    [REQUEST_HI] = { 0, 0 },
    [REQUEST_BYE] = { 0, 0 },
    [REQUEST_FEATURES] = { 0, 0 },
    [REQUEST_FOCUS] = { 1, 1 },
    [REQUEST_LINES] = { 1, 1 },
    [REQUEST_MODES] = { 0, 0 },
    [REQUEST_PORTS] = { 0, 0 },
    [REQUEST_VERSION] = { 0, 0 }
};

static int
cr_check_args(enum cr_requests req, struct cr_words *words)
{
    size_t argc;

    argc = words->count - 1;
    if (argc < cr_arg_defs[req].min) {
        cr_fail("Too few arguments");
        return 0;
    }
    if (cr_arg_defs[req].max < 0)
        return 1;
    if (argc > cr_arg_defs[req].max) {
        cr_fail("Too many arguments");
        return 0;
    }
    return 1;
}

static int
cr_ml_return_list(int cnt, char *list[])
{
    if (list[cnt] == (char *)NULL)
        return 1;
    xcr_send_host(list[cnt]);
    return 0;
}

static int
cr_ml_handle_features(int cnt, struct cr_words *words)
{
    return cr_ml_return_list(cnt, cr_features);
}

static int
verify_lines_args(struct cr_words *words)
{
    /*
     * The dispatcher code already makes sure, that we got the right number of
     * arguments. So, we got one argument. Make sure it's an integer (according
     * to the protocol spec) and make sure the port that integer is indexing
     * exists.
     */
    uint32_t idx, i;
    int err;
    char buf[CR_INT_MAX_LEN + 1];

    if (words->word[1].length > CR_INT_MAX_LEN)
        goto broken_value;

    strncpy(buf, words->word[1].start, words->word[1].length);
    buf[words->word[1].length] = '\0';
    idx = str2uint(buf, &err);
    if (err)
        goto broken_value;

    for (i = 0; cr_ports[i].nlines > 0; ++i)
        /* NOP */;

    if (idx >= i)
        goto out_of_range;

    return 1;

broken_value:
    cr_broken_value(words->word[1].start, words->word[1].length);
    return 0;
out_of_range:
    cr_uint_oor(idx);
    return 0;
}

static int
cr_ml_handle_lines(int cnt, struct cr_words *words)
{
    static uint32_t idx;

    if (cnt == 0 && !verify_lines_args(words))
        return 1;
    if (cnt == 0) {
        char buf[CR_INT_MAX_LEN + 1];
        int err;
        strncpy(buf, words->word[1].start, words->word[1].length);
        buf[words->word[1].length] = '\0';
        idx = str2uint(buf, &err);
    }

    if (cnt >= cr_ports[idx].nlines)
        return 1;

    cr_echo_line(idx,
                 cnt,
                 cr_ports[idx].lines[cnt].role,
                 cr_ports[idx].lines[cnt].index,
                 cr_ports[idx].lines[cnt].type);

    return 0;
}

static int
cr_ml_handle_modes(int cnt, struct cr_words *words)
{
    return cr_ml_return_list(cnt, cr_modes);
}

static int
cr_ml_handle_ports(int cnt, struct cr_words *words)
{
    if (cnt == 0) {
        cr_echo_ports(cr_numofports(cr_ports));
        return 0;
    } else if (cnt == 1) {
        cr_echo_focus(cr_data.fport);
        return 0;
    }
    return 1;
}

static int
cr_multi_line_process(struct cr_words *words)
{
    static enum cr_multi_line_states state = CR_ML_NONE;
    static int cnt = 0;

    switch (state) {
    case CR_ML_NONE:
        cnt = 0;
        state = cr_word2state(words, 0);
        /* FALLTHROUGH */
    default:
        if (cnt > 0 && !cr_word_eq(words, 0, "MORE")) {
            cr_fail("Unknown request in multiline state");
            state = CR_ML_NONE;
            return 1;
        }
        if (ml_jump_table[state](cnt, words)) {
            state = CR_ML_NONE;
            xcr_send_host(DONE_REPLY);
            return 1;
        }
        cnt++;
    }
    return 0;
}

static void
cr_handle_focus(struct cr_words *words)
{
    uint32_t idx, max;
    int err;
    char buf[CR_INT_MAX_LEN + 1];

    if (words->word[1].length > CR_INT_MAX_LEN)
        goto broken_value;

    strncpy(buf, words->word[1].start, words->word[1].length);
    buf[words->word[1].length] = '\0';
    idx = str2uint(buf, &err);
    if (err)
        goto broken_value;

    max = cr_numofports(cr_ports);
    if (idx > max)
        goto out_of_range;

    cr_data.fport = (int)idx;
    xcr_send_host(OK_REPLY);
    return;
broken_value:
    cr_broken_value(words->word[1].start, words->word[1].length);
    return;
out_of_range:
    cr_uint_oor(idx);
}

static enum cr_multi_line_states
cr_word2state(struct cr_words *words, int idx)
{
    if (cr_word_eq(words, idx, "FEATURES"))
        return CR_ML_FEATURES;
    if (cr_word_eq(words, idx, "LINES"))
        return CR_ML_LINES;
    if (cr_word_eq(words, idx, "MODES"))
        return CR_ML_MODES;
    if (cr_word_eq(words, idx, "PORTS"))
        return CR_ML_PORTS;
    return CR_ML_NONE;
}

static int
cr_is_multi_line(struct cr_words *words)
{
    if (cr_word_eq(words, 0, "FEATURES")) {
        if (cr_check_args(REQUEST_FEATURES, words))
            return 1;
        return -1;
    }
    if (cr_word_eq(words, 0, "LINES")) {
        if (cr_check_args(REQUEST_LINES, words))
            return 1;
        return -1;
    }
    if (cr_word_eq(words, 0, "MODES")) {
        if (cr_check_args(REQUEST_MODES, words))
            return 1;
        return -1;
    }
    if (cr_word_eq(words, 0, "PORTS")) {
        if (cr_check_args(REQUEST_PORTS, words))
            return 1;
        return -1;
    }

    return 0;
}

static int
cr_in_conv_process(void)
{
    static enum cr_conv_states state = CR_SINGLE_LINE;
    static struct cr_words words;
    int rc;

    cr_split_request(rxbuf, &words);
    switch (state) {
    case CR_SINGLE_LINE:
        if (cr_word_eq(&words, 0, "BYE")) {
            if (!cr_check_args(REQUEST_BYE, &words))
                return 0;
            xcr_send_host(BYE_REPLY);
            xcr_post_bye();
            return 1;
        } else if (cr_word_eq(&words, 0, "FOCUS")) {
            if (!cr_check_args(REQUEST_FOCUS, &words))
                return 0;
            cr_handle_focus(&words);
        } else if (cr_word_eq(&words, 0, "VERSION")) {
            if (!cr_check_args(REQUEST_BYE, &words))
                return 0;
            xcr_send_host(VERSION_REPLY);
        } else if ((rc = cr_is_multi_line(&words))) {
            if (rc > 0 && !cr_multi_line_process(&words))
                state = CR_MULTI_LINE;
        } else {
            cr_fail("Unknown request");
        }
        break;
    case CR_MULTI_LINE:
        if (cr_multi_line_process(&words))
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
        xcr_send_host(HI_REPLY);
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
