#include <stdlib.h>
#include <string.h>

#include "platform.h"
#include "protocol.h"
#include "utils.h"

static int eos(char);
static char *find_end(char *);
static char *find_start(char *);
static int space(char);

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
