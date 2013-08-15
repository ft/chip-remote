#include <string.h>

#include "chip-remote.h"
#include "port.h"
#include "protocol.h"
#include "requests.h"
#include "utils.h"

static char *cr_modes[] = {
    "SPI",
    (char *)NULL
};

static int verify_lines_args(struct cr_words *);
static int cr_return_list(int, char *[]);

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

    for (i = 0; cr_ports[i].lines.value > 0; ++i)
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

int
cr_handle_lines(int cnt, struct cr_words *words)
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

    if (cnt >= cr_ports[idx].lines.value)
        return 1;

    cr_echo_line(idx,
                 cnt,
                 cr_ports[idx].l[cnt].role,
                 cr_ports[idx].l[cnt].index,
                 cr_ports[idx].l[cnt].type);

    return 0;
}

static int
cr_return_list(int cnt, char *list[])
{
    if (list[cnt] == (char *)NULL)
        return 1;
    xcr_send_host(list[cnt]);
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

    if (cnt == 0 && !verify_lines_args(words))
        return 1;
    if (cnt == 0) {
        char buf[CR_INT_MAX_LEN + 1];
        int err;
        strncpy(buf, words->word[1].start, words->word[1].length);
        buf[words->word[1].length] = '\0';
        idx = str2uint(buf, &err);
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
    /* TODO: Protocol specific properties need to by echoed */
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

    cr_set_focused_port((int)idx);
    xcr_send_host(OK_REPLY);
    return 1;
broken_value:
    cr_broken_value(words->word[1].start, words->word[1].length);
    return 1;
out_of_range:
    cr_uint_oor(idx);
    return 1;
}

int
cr_handle_hi(int cnt, struct cr_words *words)
{
    xcr_send_host(HI_REPLY);
    return 0;
}

int
cr_handle_bye(int cnt, struct cr_words *words)
{
    xcr_send_host(BYE_REPLY);
    return 0;
}

int
cr_handle_version(int cnt, struct cr_words *words)
{
    xcr_send_host(VERSION_REPLY);
    return 0;
}
