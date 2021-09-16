/*
 * Copyright (c) 2020-2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file t-sx-parser.c
 * @brief Unit tests for s-expression parser module
 */

#include "sx-types.h"
#include <inttypes.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include <sx-parser.h>
#include <sx-utils.h>

#include <test/tap.h>
#include <common/compiler.h>

static void
t_sx_util_api(void)
{
    /* Low level */
    ok(issymch('1'), "1 is a symbol character");
    ok(issyminitch('1') == false, "1 is a NOT symbol init character");
    ok(issyminitch('+'), "+ is a symbol init character");

    /* High level */
    ok(looking_at("#x1", 3u, 0u) == LOOKING_AT_INT_HEX,
       "#x1 could start a hex literal");
    ok(looking_at("#xa", 3u, 0u) == LOOKING_AT_INT_HEX,
       "#xa could start a hex literal");
    ok(looking_at("#xF", 3u, 0u) == LOOKING_AT_INT_HEX,
       "#xF could start a hex literal");
    ok(looking_at("#xg", 3u, 0u) != LOOKING_AT_INT_HEX,
       "#xg cannot start a hex literal");
    ok(looking_at("#xg", 3u, 0u) == LOOKING_AT_UNKNOWN,
       "#xg can't be anything reasonable");
    ok(looking_at("(", 1u, 0u) == LOOKING_AT_PAREN_OPEN,
       "( could start another level of list");
    ok(looking_at(")", 1u, 0u) == LOOKING_AT_PAREN_CLOSE,
       ") could start another level of list");
    ok(looking_at("1", 1u, 0u) == LOOKING_AT_INT_DEC,
       "1 could start a decimal integer literal");
    ok(looking_at("+", 1u, 0u) == LOOKING_AT_SYMBOL,
       "+ could start a symbol");
}

/* sx_parse_token() test cases */

static void
t_sx_parse_token_empty(void)
{
    struct sx_parse_result p = sx_parse_token("", 0, 0);
    ok(p.node == NULL, "Empty string parses to NULL");
    ok(p.status == SXS_SUCCESS, "Empty string does not indicate error");
}

static void
t_sx_parse_token_only_whitespace(void)
{
    struct sx_parse_result p = sx_parse_token(" \t   ", 5, 0);
    ok(p.node == NULL, "Just whitespace parses to NULL");
    ok(p.status == SXS_SUCCESS, "Just whitespace does not indicate error");
}

static void
t_sx_parse_token_symbol(void)
{
    struct sx_parse_result p = sx_parse_token("foobar", 6, 0);
    unless (ok(p.node != NULL, "foobar parses to non-NULL")) {
        goto cleanup;
    }
    unless (ok(p.node->type == SXT_SYMBOL, "foobar parses to SXT_SYMBOL")) {
        goto cleanup;
    }
    ok(strcmp(p.node->data.symbol, "foobar") == 0, "foobar parses to symbol");
    ok(p.status == SXS_SUCCESS, "foobar does not indicate error");

cleanup:
    sx_destroy(&p.node);
}

static void
t_sx_parse_token_int_dec(void)
{
    struct sx_parse_result p = sx_parse_token("12345", 5, 0);
    unless (ok(p.node != NULL, "12345 parses to non-NULL")) {
        goto cleanup;
    }
    unless (ok(p.node->type == SXT_INTEGER, "12345 parses to SXT_INTEGER")) {
        goto cleanup;
    }
    unless(ok(p.node->data.u64 == 12345, "12345 parses to integer 12345")) {
        pru64(p.node->data.u64, 0x400);
    }
    ok(p.status == SXS_SUCCESS, "12345 does not indicate error");

cleanup:
    sx_destroy(&p.node);
}

static void
t_sx_parse_token_int_hex(void)
{
    struct sx_parse_result p = sx_parse_token("#x400", 5, 0);
    unless (ok(p.node != NULL, "#x400 parses to non-NULL")) {
        goto cleanup;
    }
    unless (ok(p.node->type == SXT_INTEGER, "#x400 parses to SXT_INTEGER")) {
        goto cleanup;
    }
    unless(ok(p.node->data.u64 == 0x400, "#x400 parses to integer 0x400")) {
        pru64(p.node->data.u64, 0x400);
    }
    ok(p.status == SXS_SUCCESS, "#x400 does not indicate error");

cleanup:
    sx_destroy(&p.node);
}

static void
t_sx_parse_token_error_dec(void)
{
    struct sx_parse_result p = sx_parse_token("1234a", 5, 0);
    ok(p.node == NULL, "1234a parses to NULL");
    ok(p.status == SXS_BROKEN_INTEGER, "1234a indicates integer-error");
    ok(p.position == 4, "...error at position 4 == %" PRIu64, p.position);
}

static void
t_sx_parse_token_error_hex(void)
{
    struct sx_parse_result p = sx_parse_token("#x12g", 5, 0);
    ok(p.node == NULL, "#x12g parses to NULL");
    ok(p.status == SXS_BROKEN_INTEGER, "#x12g indicates integer-error");
    ok(p.position == 4, "...error at position 4 == %" PRIu64, p.position);
}

static void
t_sx_parse_token_error_symbol(void)
{
    struct sx_parse_result p = sx_parse_token("foo{}bar", 5, 0);
    ok(p.node == NULL, "foo{}bar parses to NULL");
    ok(p.status == SXS_BROKEN_SYMBOL, "foo{}bar indicates symbol-error");
    ok(p.position == 3, "...error at position 3 == %" PRIu64, p.position);
}

/* sx_parse() test cases */

static void
t_sx_parse_empty_list(void)
{
    struct sx_parse_result p = sx_parse("()", 2, 0);
    unless (ok(p.node != NULL, "() parses to non-NULL")) {
        goto cleanup;
    }
    ok(p.node->type == SXT_EMPTY_LIST, "() parses to SXT_EMPTY_LIST");
    ok(p.status == SXS_SUCCESS, "() does not indicate an error");

cleanup:
    sx_destroy(&p.node);
}

static void
t_sx_parse_empty_one_elem_list(void)
{
    struct sx_parse_result p = sx_parse("(1)", 3, 0);
    unless (ok(p.node != NULL, "(1) parses to non-NULL")) {
        goto cleanup;
    }
    unless (ok(p.node->type == SXT_PAIR, "(1) parses to pair")) {
        pru16(p.node->type, SXT_PAIR);
        goto cleanup;
    }
    ok(p.node->data.pair->car->type == SXT_INTEGER, "car of (1) is an INTEGER");
    ok(p.node->data.pair->car->data.u64 == 1, "car of (1) is 1");
    unless (ok(p.node->data.pair->cdr->type == SXT_EMPTY_LIST,
               "cdr of (1) is the empty list"))
    {
        pru16(p.node->data.pair->cdr->type, SXT_EMPTY_LIST);
    }
    ok(p.status == SXS_SUCCESS, "(1) indicates an success");

cleanup:
    sx_destroy(&p.node);
}

static void
t_sx_parse_empty_two_elem_list(void)
{
    struct sx_parse_result p = sx_parse("(1 2)", 5, 0);
    unless (ok(p.node != NULL, "(1 2) parses to non-NULL")) {
        goto cleanup;
    }

    unless (ok(p.node->type == SXT_PAIR, "(1 2) parses to pair")) {
        pru16(p.node->type, SXT_PAIR);
        goto cleanup;
    }
    ok(p.node->data.pair->car->type == SXT_INTEGER,
       "car of (1 2) is an INTEGER");
    ok(p.node->data.pair->car->data.u64 == 1, "car of (1 2) is 1");

    unless (ok(p.node->data.pair->cdr->type == SXT_PAIR,
               "cdr of (1 2) parses to pair"))
    {
        pru16(p.node->data.pair->cdr->type, SXT_PAIR);
        goto cleanup;
    }

    ok(p.node->data.pair->cdr->data.pair->car->type == SXT_INTEGER,
       "cadr of (1 2) is an INTEGER");
    ok(p.node->data.pair->cdr->data.pair->car->data.u64 == 2,
       "cadr of (1 2) is 2");
    ok(p.node->data.pair->cdr->data.pair->cdr->type == SXT_EMPTY_LIST,
       "cddr of (1 2) is the empty list");

    ok(p.status == SXS_SUCCESS, "(1 2) does not indicate an error");

cleanup:
    sx_destroy(&p.node);
}

static void
t_sx_parse_incomplete_list(void)
{
    struct sx_parse_result p = sx_parse("(", 1, 0);
    ok(p.status == SXS_UNEXPECTED_END, "( signals unexpected end");
    ok(p.node == NULL, "( returns NULL");

    p = sx_parse_string("(1 2");
    ok(p.status == SXS_UNEXPECTED_END, "(1 2 signals unexpected end");
    ok(p.node == NULL, "(1 2 returns NULL");

    p = sx_parse_string("(foobar (stuff) (1 2)");
    /*            0123456789012345678901
     *            0000000000111111111122 */
    ok(p.status == SXS_UNEXPECTED_END, "(foobar (stuff) (1 2) signals unexpected end");
    ok(p.node == NULL, "(foobar (stuff) (1 2) returns NULL");
}

static void
t_cxr(void)
{
    const char *expr = "((1 (a b c) 3) (q w e) r t (5) 6)";
    /*                  0123456789012345678901234567890123
     *                  0000000000111111111122222222223333 */
    struct sx_parse_result p = sx_parse_string(expr);
    struct sx_node *n;
    unless(ok(p.status == SXS_SUCCESS, "%s signals success", expr)) {
        pru16(p.status, SXS_SUCCESS);
    }

    n = sx_cxr(p.node, "aa");
    unless (ok(n != NULL, "caar of %s does not return NULL", expr)) {
        goto cleanup;
    }
    ok(n->type == SXT_INTEGER, "caar of %s is an integer", expr);
    ok(n->data.u64 == 1, "caar of %s is 1", expr);

    n = sx_cxr(p.node, "adada");
    unless (ok(n != NULL, "cadadar of %s does not return NULL", expr)) {
        goto cleanup;
    }
    ok(n->type == SXT_SYMBOL, "cadadar of %s is a symbol", expr);
    ok(strcmp(n->data.symbol, "b") == 0, "cadadar of %s is b", expr);

    n = sx_cxr(p.node, "aad");
    unless (ok(n != NULL, "caadr of %s does not return NULL", expr)) {
        goto cleanup;
    }
    ok(n->type == SXT_SYMBOL, "caadr of %s is a symbol", expr);
    ok(strcmp(n->data.symbol, "q") == 0, "caadr of %s is q", expr);

    n = sx_cxr(p.node, "addd");
    unless (ok(n != NULL, "cadddr of %s does not return NULL", expr)) {
        goto cleanup;
    }
    ok(n->type == SXT_SYMBOL, "cadddr of %s is a symbol", expr);
    ok(strcmp(n->data.symbol, "t") == 0, "cadddr of %s is t", expr);

    n = sx_cxr(p.node, "aadddd");
    unless (ok(n != NULL, "caaddddr of %s does not return NULL", expr)) {
        goto cleanup;
    }
    ok(n->type == SXT_INTEGER, "caaddddr of %s is an integer", expr);
    ok(n->data.u64 == 5, "caaddddr of %s is 5", expr);

    n = sx_cxr(p.node, "addddd");
    unless (ok(n != NULL, "cadddddr of %s does not return NULL", expr)) {
        goto cleanup;
    }
    ok(n->type == SXT_INTEGER, "cadddddr of %s is an integer", expr);
    ok(n->data.u64 == 6, "cadddddr of %s is 6", expr);

    n = sx_cxr(p.node, "dddddd");
    unless (ok(n != NULL, "cddddddr of %s does not return NULL", expr)) {
        goto cleanup;
    }
    ok(n->type == SXT_EMPTY_LIST, "cddddddr of %s is the empty list", expr);

cleanup:
    sx_destroy(&p.node);
}

static void
t_pop(void)
{
    /* Make a tree by parsing this expression, then traverse it using sx_pop,
     * inspecting all that is revealed. sx_pop makes sure you only have to
     * destroy the elements that have been returned to you. Thus, running this
     * test-suite in a memory tester like valgrind should still show that no
     * memory was leaked. */
    const char *expr = "((1 (a b c) 3) (q w e) r t (5) 6)";
    /*                  0123456789012345678901234567890123
     *                  0000000000111111111122222222223333 */
    struct sx_parse_result p = sx_parse_string(expr);
    struct sx_node *n;

    unless(ok(p.status == SXS_SUCCESS, "%s signals success", expr)) {
        pru16(p.status, SXS_SUCCESS);
    }

    /* lst1 is (1 (a b c) 3) */
    struct sx_node *lst1 = sx_pop(&p.node);
    ok(lst1->type == SXT_PAIR, "pop reveals a pair");
    n = sx_pop(&lst1);
    ok(n->type == SXT_INTEGER, "pop reveals an integer");
    ok(n->data.u64 == 1, "pop reveals the integer 1");
    sx_destroy(&n);

    /* lst2 is (a b c) */
    struct sx_node *lst2 = sx_pop(&lst1);
    ok(lst2->type == SXT_PAIR, "pop reveals a pair");
    n = sx_pop(&lst2);
    ok(n->type == SXT_SYMBOL, "pop reveals an integer");
    ok(strcmp(n->data.symbol, "a") == 0, "pop reveals a", expr);
    sx_destroy(&n);
    n = sx_pop(&lst2);
    ok(n->type == SXT_SYMBOL, "pop reveals an integer");
    ok(strcmp(n->data.symbol, "b") == 0, "pop reveals b", expr);
    sx_destroy(&n);
    n = sx_pop(&lst2);
    ok(n->type == SXT_SYMBOL, "pop reveals an integer");
    ok(strcmp(n->data.symbol, "c") == 0, "pop reveals c", expr);
    sx_destroy(&n);
    ok(lst2->type == SXT_EMPTY_LIST, "pop made lst2 into the empty list");
    n = sx_pop(&lst2);
    ok(n->type == SXT_EMPTY_LIST, "pop reveals the empty list");
    sx_destroy(&n);

    n = sx_pop(&lst1);
    ok(n->type == SXT_INTEGER, "pop reveals an integer");
    ok(n->data.u64 == 3, "pop reveals the integer 3");
    sx_destroy(&n);
    ok(lst1->type == SXT_EMPTY_LIST, "pop made lst1 into the empty list");
    n = sx_pop(&lst1);
    ok(n->type == SXT_EMPTY_LIST, "pop reveals the empty list");
    sx_destroy(&n);

    /* lst3 is (q w e) */
    struct sx_node *lst3 = sx_pop(&p.node);
    ok(lst3->type == SXT_PAIR, "pop reveals a pair");
    n = sx_pop(&lst3);
    ok(n->type == SXT_SYMBOL, "pop reveals a symbol");
    ok(strcmp(n->data.symbol, "q") == 0, "pop reveals q", expr);
    sx_destroy(&n);
    n = sx_pop(&lst3);
    ok(n->type == SXT_SYMBOL, "pop reveals a symbol");
    ok(strcmp(n->data.symbol, "w") == 0, "pop reveals w", expr);
    sx_destroy(&n);
    n = sx_pop(&lst3);
    ok(n->type == SXT_SYMBOL, "pop reveals a symbol");
    ok(strcmp(n->data.symbol, "e") == 0, "pop reveals e", expr);
    sx_destroy(&n);
    ok(lst3->type == SXT_EMPTY_LIST, "pop made lst3 into the empty list");
    n = sx_pop(&lst3);
    ok(n->type == SXT_EMPTY_LIST, "pop reveals the empty list");
    sx_destroy(&n);

    /* Now on to r and t from p.node */
    n = sx_pop(&p.node);
    ok(n->type == SXT_SYMBOL, "pop reveals a symbol");
    ok(strcmp(n->data.symbol, "r") == 0, "pop reveals r", expr);
    sx_destroy(&n);
    n = sx_pop(&p.node);
    ok(n->type == SXT_SYMBOL, "pop reveals a symbol");
    ok(strcmp(n->data.symbol, "t") == 0, "pop reveals t", expr);
    sx_destroy(&n);

    /* lst4 is (5) */
    struct sx_node *lst4 = sx_pop(&p.node);
    ok(lst4->type == SXT_PAIR, "pop reveals a pair");
    n = sx_pop(&lst4);
    ok(n->type == SXT_INTEGER, "pop reveals an integer");
    ok(n->data.u64 == 5, "pop reveals the integer 5");
    sx_destroy(&n);
    ok(lst4->type == SXT_EMPTY_LIST, "pop made lst4 into the empty list");
    n = sx_pop(&lst4);
    ok(n->type == SXT_EMPTY_LIST, "pop reveals the empty list");
    sx_destroy(&n);

    /* p.node is now (6) */
    n = sx_pop(&p.node);
    ok(n->type == SXT_INTEGER, "pop reveals an integer");
    ok(n->data.u64 == 6, "pop reveals the integer 6");
    sx_destroy(&n);
    ok(p.node->type == SXT_EMPTY_LIST, "pop made lst4 into the empty list");
    n = sx_pop(&p.node);
    ok(n->type == SXT_EMPTY_LIST, "pop reveals the empty list");
    sx_destroy(&n);
}

static void
t_sx_parse_token(void)
{
    t_sx_parse_token_empty();
    t_sx_parse_token_only_whitespace();
    t_sx_parse_token_symbol();
    t_sx_parse_token_int_dec();
    t_sx_parse_token_int_hex();
    t_sx_parse_token_error_dec();
    t_sx_parse_token_error_hex();
    t_sx_parse_token_error_symbol();
}

static void
t_sx_parse(void)
{
    t_sx_parse_empty_list();
    t_sx_parse_empty_one_elem_list();
    t_sx_parse_empty_two_elem_list();
    t_sx_parse_incomplete_list();
}

int
main(UNUSED int argc, UNUSED char *argv[])
{
    plan(121);

    t_sx_util_api();
    t_sx_parse_token();
    t_sx_parse();

    t_cxr();
    t_pop();

    return EXIT_SUCCESS;
}
