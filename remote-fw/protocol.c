#include <stdlib.h>
#include <string.h>

#include "platform.h"
#include "protocol.h"
#include "utils.h"

static int eos(char);
static char *find_end(char *);
static char *find_start(char *);
static int space(char);
static void echo_int(char *, uint32_t);

static int
eos(char c)
{
    if (c == '\0')
        return 1;
    return 0;
}

static int
space(char c)
{
    if (c == ' ')
        return 1;
    return 0;
}

static char *
find_end(char *buf)
{
    while (!space(*buf) && !eos(*buf))
        buf++;
    return buf;
}

static char *
find_start(char *buf)
{
    while (space(*buf))
        buf++;
    return buf;
}

void
cr_split_request(char *buf, struct cr_words *words)
{
    char *start, *end;
    size_t n;

    end = buf;
    n = 0;
    while (n < CR_MAX_WORDS) {
        start = find_start(end);
        end = find_end(start);
        words->word[n].start = start;
        words->word[n].length = end - start;
        words->count = ++n;
        if (*end == '\0')
            break;
    }
}

int
cr_word_eq(struct cr_words *words, size_t idx, const char *data)
{
    size_t dl;

    dl = strlen(data);
    if (dl != words->word[idx].length)
        return 0;
    return STREQ_N(words->word[idx].start, data, dl);
}

void
cr_fail(const char *reason)
{
    char buf[CR_MAX_LINE + 1];

    strncpy(buf, WTF_REPLY, CR_MAX_LINE);
    strncat(buf, " ", CR_MAX_LINE);
    strncat(buf, reason, CR_MAX_LINE);
    buf[CR_MAX_LINE] = '\0';
    xcr_send_host(buf);
}

void
cr_broken_value(char *value, size_t len)
{
    char buf[CR_MAX_LINE + 1];

    strncpy(buf, BROKEN_VALUE_REPLY, CR_MAX_LINE);
    strncat(buf, " ", CR_MAX_LINE);
    strncat(buf, value, len);
    buf[CR_MAX_LINE] = '\0';
    xcr_send_host(buf);
}

void
cr_uint_oor(uint32_t value)
{
    char buf[CR_MAX_LINE + 1];
    char ibuf[CR_INT_MAX_LEN + 1];

    strncpy(buf, VALUE_OUT_OF_RANGE_REPLY, CR_MAX_LINE);
    strncat(buf, " ", CR_MAX_LINE);
    uint2str(value, ibuf);
    strncat(buf, ibuf, CR_INT_MAX_LEN);
    buf[CR_MAX_LINE] = '\0';
    xcr_send_host(buf);
}

static char *roles[] = {
    [CR_ROLE_NONE] = "NONE",
    [CR_ROLE_SPI_CLK] = "CLK",
    [CR_ROLE_SPI_CS] = "CS",
    [CR_ROLE_SPI_MOSI] = "MOSI",
    [CR_ROLE_SPI_MISO] = "MISO",
};

void
cr_echo_line(size_t port, size_t line, enum cr_pin_role role, int idx)
{
    char buf[CR_MAX_LINE + 1];
    char ibuf[CR_INT_MAX_LEN + 1];

    strncpy(buf, LINE_REPLY, CR_MAX_LINE);
    strncat(buf, " ", CR_MAX_LINE);
    uint2str(port, ibuf);
    strncat(buf, ibuf, CR_INT_MAX_LEN);
    strncat(buf, " ", CR_MAX_LINE);
    uint2str(line, ibuf);
    strncat(buf, ibuf, CR_INT_MAX_LEN);
    strncat(buf, " ", CR_MAX_LINE);
    strncat(buf, roles[role], CR_INT_MAX_LEN);
    if (idx >= 0) {
        strncat(buf, ":", CR_MAX_LINE);
        uint2str(idx, ibuf);
        strncat(buf, ibuf, CR_INT_MAX_LEN);
    }
    xcr_send_host(buf);
}

void
cr_echo_ports(size_t num)
{
    echo_int(PORTS_REPLY, num);
}

void
cr_echo_focus(int num)
{
    if (num < 0)
        xcr_send_host(FOCUS_REPLY " NONE");
    else
        echo_int(FOCUS_REPLY, num);
}

static void
echo_int(char *prefix, uint32_t num)
{
    char buf0[CR_MAX_LINE + 1];
    char buf1[CR_INT_MAX_LEN + 1];

    strcpy(buf0, prefix);
    strncat(buf0, " ", CR_MAX_LINE);
    uint2str(num, buf1);
    strncat(buf0, buf1, CR_MAX_LINE);
    buf0[CR_MAX_LINE] = '\0';
    xcr_send_host(buf0);
}
