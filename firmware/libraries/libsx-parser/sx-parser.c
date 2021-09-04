/**
 * @file sx-parser.c
 * @brief Simple S-Expression Parser
 *
 * This module implements an S-Expression parser, for use in instrumentation
 * of the chip-remote firmware in native builds. As such it is allowed more
 * liberal use of dynamically allocated memory.
 *
 * It is a very small subset of scheme's s-expressions:
 *
 *   - Lists              (exp exp exp ...)
 *   - Symbols            foobar
 *   - Unsigned Integers  decimal: 1234, hex: #x1234
 *
 * That's it. Lists can be nested, so this allows for arbitrarily complex
 * structures.
 */

#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <stdio.h>

#include "sx-parser.h"
#include "sx-types.h"
#include "sx-utils.h"

void
sx_destroy(struct sx_node **n)
{
    struct sx_node *node = *n;
    if (node == NULL)
        return;

    if (node->type == SXT_PAIR) {
        sx_destroy(&node->data.pair->car);
        sx_destroy(&node->data.pair->cdr);
        free(node->data.pair);
        node->data.pair = NULL;
    } else if (node->type == SXT_SYMBOL) {
        free(node->data.symbol);
        node->data.symbol = NULL;
    }

    free(node);
    *n = NULL;
}

static size_t
skip_ws(const char *s, const size_t n, size_t i)
{
    while (i < n && isspace(s[i])) {
        i += 1;
    }

    return i;
}

struct sx_parse_result
sx_parse_token(const char *s, const size_t n, const size_t i)
{
    struct sx_parse_result rv = SX_PARSE_RESULT_INIT;
    size_t j = skip_ws(s, n, i);

    if (j == n)
        return rv;


    switch (looking_at(s, n, j)) {
    case LOOKING_AT_INT_DEC:
        rv.node = make_integer(s, n, &j);
        if (rv.node == NULL)
            rv.status = SXS_BROKEN_INTEGER;
        break;
    case LOOKING_AT_INT_HEX:
        rv.node = make_hinteger(s, n, &j);
        if (rv.node == NULL)
            rv.status = SXS_BROKEN_INTEGER;
        break;
    case LOOKING_AT_SYMBOL:
        rv.node = make_symbol(s, n, &j);
        if (rv.node == NULL)
            rv.status = SXS_BROKEN_SYMBOL;
        break;
    case LOOKING_AT_PAREN_OPEN:
        rv.status = SXS_FOUND_LIST;
        j++;
        break;
    case LOOKING_AT_PAREN_CLOSE:
        rv.node = make_end_of_list(s, n, &j);
        break;
    case LOOKING_AT_UNKNOWN:
        /* FALLTHROUGH */
    default:
        rv.status = SXS_UNKNOWN_INPUT;
        break;
    }

    rv.position = j;
    return rv;
}

static inline bool
result_is_empty_listp(const struct sx_parse_result *res)
{
    return (res->status == SXS_SUCCESS && res->node->type == SXT_EMPTY_LIST);
}

static inline bool
result_is_error(const struct sx_parse_result *res)
{
    return (res->status != SXS_SUCCESS && res->status != SXS_FOUND_LIST);
}

static struct sx_parse_result sx_parse_(const char*, size_t, size_t);
static struct sx_parse_result sx_parse_list(const char*, size_t, size_t);

static struct sx_parse_result
sx_parse_list(const char *s, const size_t n, const size_t i)
{
    if (i >= n) {
        struct sx_parse_result rv = SX_PARSE_RESULT_INIT;
        rv.status = SXS_UNEXPECTED_END;
        return rv;
    }
    struct sx_parse_result carres = sx_parse_(s, n, i);
    if (result_is_empty_listp(&carres) || result_is_error(&carres)) {
        return carres;
    }

    struct sx_node *cons = make_pair(s, n, &carres.position);
    cons->data.pair->car = carres.node;

    struct sx_parse_result cdrres = sx_parse_list(s, n, carres.position - 1);
    cons->data.pair->cdr = cdrres.node;

    carres.node = cons;
    carres.position = cdrres.position;
    carres.status = cdrres.status;

    return carres;
}

static struct sx_parse_result
sx_parse_(const char *s, const size_t n, const size_t i)
{
    struct sx_parse_result rv = sx_parse_token(s, n, i);
    if (rv.status == SXS_FOUND_LIST) {
        return sx_parse_list(s, n, rv.position);
    }
    if (i >= n && rv.node == NULL) {
        rv.status = SXS_UNEXPECTED_END;
        return rv;
    }
    return rv;
}

struct sx_parse_result
sx_parse(const char *s, const size_t n, const size_t i)
{
    struct sx_parse_result rv = sx_parse_(s, n, i);
    if (result_is_error(&rv)) {
        sx_destroy(&rv.node);
    }
    return rv;
}

struct sx_parse_result
sx_parse_stringn(const char *s, const size_t n)
{
    return sx_parse(s, n, 0);
}

struct sx_parse_result
sx_parse_string(const char *s)
{
    return sx_parse_stringn(s, strlen(s));
}

struct sx_node *
sx_cxr(struct sx_node *root, const char *addr)
{
    struct sx_node *ptr = root;
    size_t i = strlen(addr);
    if (i == 0)
        return root;

    --i;
    for (;;) {
        if (ptr->type != SXT_PAIR) {
            return NULL;
        }
        switch (addr[i]) {
        case 'a':
            ptr = ptr->data.pair->car;
            break;
        case 'd':
            ptr = ptr->data.pair->cdr;
            break;
        default:
            return NULL;
        }
        if (i == 0u)
            break;
        --i;
    }

    return ptr;
}
