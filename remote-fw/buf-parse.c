/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stdint.h>
#include <string.h>
#include "chip-remote.h"
#include "buf-parse.h"
#include "utils.h"

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
    words->count = 0;
    n = 0;
    while (n < CR_MAX_WORDS) {
        start = find_start(end);
        end = find_end(start);
        if (*end == '\0' && *start == '\0')
            break;
        words->word[n].start = start;
        words->word[n].length = (size_t)(end - start);
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
