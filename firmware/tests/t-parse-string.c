/*
 * Copyright (c) 2020-2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file t-parse-string.c
 * @brief Unit tests for parse-string module
 */

#include "chip-remote.h"
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <parse-string.h>

#include <test/tap.h>
#include <c/compat/strings.h>
#include <common/compiler.h>

static bool
tokens_used(struct cr_tokens *t, unsigned int n)
{
    bool rv = ok(t->used == n, "    tokens: %u", n);

    unless (rv) {
        pru32(t->used, n);
    }

    return rv;
}

static bool
t_token_type(struct cr_tokens *t, unsigned int n,
             enum cr_argument_type type, const char *typestr)
{
    bool rv = ok(t->token[n].type == type, "    type($%u): %s", n, typestr);
    unless(rv) {
        pru32(t->token[n].type, type);
    }

    return rv;
}

#define token_type(t,n,type) t_token_type(t,n,CR_PROTO_ARG_TYPE_ ## type, #type)

static bool
t_result_type(const char *input,
              enum cr_parser_result actual,
              enum cr_parser_result expected,
              const char *actualstr)
{
    bool rv = ok(actual == expected, "parse('%s') => %s", input, actualstr);
    unless(rv) {
        pru32(actual, expected);
    }

    return rv;
}

#define result_type(s,a,e) t_result_type(s, a, CR_PARSER_ ## e, #e)

static void
symbol_arg_is(struct cr_tokens *t, unsigned int n, const char *expected)
{
    size_t actual_len = strlen(t->token[n].data.symbol);
    size_t expected_len = strlen(expected);

    unless (ok(actual_len == expected_len,
               "    len($%u): expected (%zu)", n, expected_len))
    {
        pru32(actual_len, expected_len);
    }

    size_t len = (actual_len < expected_len) ? actual_len : expected_len;
    unless (cmp_mem(t->token[n].data.symbol, expected, len,
                    "    symbol($%u): '%s'", n, expected))
    {
        printf("# expected: '%s'\n#   actual: '%s'\n",
               t->token[n].data.symbol,
               expected);
    }
}

static void
kv_arg_is(struct cr_tokens *t, unsigned int n,
          const char *expectedsym, cr_number expectednum)
{
    size_t actual_len = strlen(t->token[n].data.kv.key);
    size_t expected_len = strlen(expectedsym);

    unless (ok(actual_len == expected_len,
               "    len(key($%u)): expected (%zu)", n, expected_len))
    {
        pru32(actual_len, expected_len);
    }

    size_t len = (actual_len < expected_len) ? actual_len : expected_len;
    unless (cmp_mem(t->token[n].data.kv.key, expectedsym, len,
                    "    key($%u): '%s'", n, expectedsym))
    {
        printf("# expected: '%s'\n#   actual: '%s'\n",
               t->token[n].data.kv.key,
               expectedsym);
    }

    unless (ok(t->token[n].data.kv.value == expectednum,
               "    value($%u): 0x%"PRIxCRN, n, expectednum))
    {
        pru64(t->token[n].data.kv.value, expectednum);
    }
}

static void
number_arg_is(struct cr_tokens *t, unsigned int n, cr_number expected)
{
    unless (ok(t->token[n].data.number == expected,
               "    number($%u): 0x%"PRIxCRN, n, expected))
    {
        pru64(t->token[n].data.number, expected);
    }
}

static void
boolean_arg_is(struct cr_tokens *t, unsigned int n, bool expected)
{
    unless (ok(t->token[n].data.boolean == expected,
               "    boolean($%u): %s", n, expected ? "true" : "false"))
    {
        printf("# expected: '%s'\n#   actual: '%s'\n",
               t->token[n].data.symbol ? "true" : "false",
               expected ? "true" : "false");
    }
}

int
main(UNUSED int argc, UNUSED char *argv[])
{
    plan(113u);

    enum cr_parser_result result;
    struct cr_tokens ts;
    struct cr_value vs[32];

    struct cr_parser_state p;
    char *original;
    char input[256];

    /* Single symbol token */
    original = "version";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, SYMBOL)) {
            symbol_arg_is(&ts, 0, "version");
        }
    }

    /* Symbols can't start with a decimal digit */
    original = "1version";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, GENERIC_FAILURE);
    tokens_used(&ts, 0u);

    /* Default number format is decimal */
    original = "1234";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, INTEGER)) {
            number_arg_is(&ts, 0, 1234);
        }
    }

    /* Explicit decimal */
    original = "d#1234";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, INTEGER)) {
            number_arg_is(&ts, 0, 1234);
        }
    }

    /* Hex Numbers */
    original = "h#af12";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, INTEGER)) {
            number_arg_is(&ts, 0, 0xaf12ULL);
        }
    }

    /* Hex Numbers: Digit error */
    original = "h#af12g";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, INVALID_DIGIT_HEX);
    tokens_used(&ts, 0u);

    /* Octal Numbers */
    original = "o#0755";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, INTEGER)) {
            number_arg_is(&ts, 0, 0755ULL);
        }
    }

    /* Octal Numbers: Digit error */
    original = "o#0855";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, INVALID_DIGIT_OCT);
    tokens_used(&ts, 0u);

    /* Binary Numbers */
    original = "b#1001";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, INTEGER)) {
            number_arg_is(&ts, 0, 9u);
        }
    }

    /* Binary Numbers: Digit error */
    original = "b#1201";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, INVALID_DIGIT_BIN);
    tokens_used(&ts, 0u);

    /* Boolean Symbol: true */
    original = "true";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, BOOLEAN)) {
            boolean_arg_is(&ts, 0, true);
        }
    }

    /* Boolean Symbol: false */
    original = "false";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, BOOLEAN)) {
            boolean_arg_is(&ts, 0, false);
        }
    }

    /* Key-Value Pair */
    original = "foo:123";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, KEYVALUE)) {
            kv_arg_is(&ts, 0, "foo", 123);
        }
    }

    /* Key-Value Pair: Key error */
    original = "1foo:123";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, GENERIC_FAILURE);
    tokens_used(&ts, 0u);

    /* Key-Value Pair: value error */
    original = "w:1a23";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, GENERIC_FAILURE);
    tokens_used(&ts, 0u);

    /* Leading whitespace does not matter */
    original = "\t   version";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, SYMBOL)) {
            symbol_arg_is(&ts, 0, "version");
        }
    }

    /* Trailing whitespace does not matter */
    original = "version\t   ";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, SYMBOL)) {
            symbol_arg_is(&ts, 0, "version");
        }
    }

    /* Surrounding whitespace does not matter */
    original = "   \tversion\t   ";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 1u)) {
        if (token_type(&ts, 0, SYMBOL)) {
            symbol_arg_is(&ts, 0, "version");
        }
    }

    /* More than one token */
    original =
        "   \t"
        "set"
        "\t   "
        "thingy"
        "\t"
        "true"
        "   ";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 3u)) {
        if (token_type(&ts, 0, SYMBOL)) {
            symbol_arg_is(&ts, 0, "set");
        }
        if (token_type(&ts, 1, SYMBOL)) {
            symbol_arg_is(&ts, 1, "thingy");
        }
        if (token_type(&ts, 2, BOOLEAN)) {
            boolean_arg_is(&ts, 2, true);
        }
    }

    /* Complex case with most features */
    original =
        "   \t"
        "transmit"
        "\t   "
        "w:8"
        "       "
        "123 h#abc b#111    o#7654321\t 0 1 2 3"
        "\t"
        "r:12"
        "   "
        "true"
        " "
        "false";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, SUCCESS);
    if (tokens_used(&ts, 13u)) {
        if (token_type(&ts, 0, SYMBOL)) {
            symbol_arg_is(&ts, 0, "transmit");
        }
        if (token_type(&ts, 1, KEYVALUE)) {
            kv_arg_is(&ts, 1, "w", 8u);
        }
        if (token_type(&ts, 2, INTEGER)) {
            number_arg_is(&ts, 2, 123u);
        }
        if (token_type(&ts, 3, INTEGER)) {
            number_arg_is(&ts, 3, 0xabcu);
        }
        if (token_type(&ts, 4, INTEGER)) {
            number_arg_is(&ts, 4, 7u);
        }
        if (token_type(&ts, 5, INTEGER)) {
            number_arg_is(&ts, 5, 07654321u);
        }
        if (token_type(&ts, 6, INTEGER)) {
            number_arg_is(&ts, 6, 0u);
        }
        if (token_type(&ts, 7, INTEGER)) {
            number_arg_is(&ts, 7, 1u);
        }
        if (token_type(&ts, 8, INTEGER)) {
            number_arg_is(&ts, 8, 2u);
        }
        if (token_type(&ts, 9, INTEGER)) {
            number_arg_is(&ts, 9, 3u);
        }
        if (token_type(&ts, 10, KEYVALUE)) {
            kv_arg_is(&ts, 10, "r", 12u);
        }
        if (token_type(&ts, 11, BOOLEAN)) {
            boolean_arg_is(&ts, 11, true);
        }
        if (token_type(&ts, 12, BOOLEAN)) {
            boolean_arg_is(&ts, 12, false);
        }
    }

    /* Too many tokens */
    original = "foo bar baz";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 2);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, TOO_MANY_TOKENS);
    tokens_used(&ts, 2u);

    /* A backslash is not in our alphabet */
    original = "f\\oo";
    strlcpy(input, original, sizeof(input));
    cr_tokens_init(&ts, vs, 32);
    cr_parser_init(&p, input);
    result = cr_parse(&p, &ts);
    result_type(original, result, GENERIC_FAILURE);
    tokens_used(&ts, 0u);

    return EXIT_SUCCESS;
}
