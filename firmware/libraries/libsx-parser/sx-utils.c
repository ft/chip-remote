#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <common/compiler.h>

#include "sx-types.h"
#include "sx-utils.h"

static char *digits = "0123456789abcdef";

static inline uint64_t
minu64(const uint64_t a, const uint64_t b)
{
    return a < b ? a : b;
}

static inline uint64_t
digit2int(const char c)
{
    uint64_t rv = 0u;
    while (rv < 16) {
        if (digits[rv] == c)
            return rv;
        rv++;
    }
    return 0;
}

static const char *syminitchtab =
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "+%|/_:;.!?$&=*<>~";

bool
issyminitch(const char c)
{
    return strchr(syminitchtab, c) != NULL;
}

bool
issymch(const char c)
{
    return issyminitch(c) || isdigit(c);
}

static void NORETURN
sxoom(const char *f, const int n)
{
    fprintf(stderr, "%s:%d: Could not allocate memory!\n", f, n);
    exit(1);
}

static bool
nextisdelimiter(const char c)
{
    return c == '(' || c == ')' || isspace(c);
}

struct sx_node *
make_symbol(const char *s, const size_t n, size_t *i)
{
    size_t j = *i;

    while (j < n) {
        if (issymch(s[j]) == false)
            break;
        j++;
    }

    if ((j < n) && (nextisdelimiter(s[j]) == false)) {
        *i = j;
        return NULL;
    }

    size_t len = j - *i;
    struct sx_node *rv = malloc(sizeof *rv);
    if (rv == NULL) {
        sxoom(__FILE__, __LINE__);
    }
    rv->data.symbol = calloc(sizeof(char), len + 1);
    if (rv == NULL) {
        sxoom(__FILE__, __LINE__);
    }
    rv->type = SXT_SYMBOL;
    strncpy(rv->data.symbol, s + *i, len);
    *i = minu64(j, n);

    return rv;
}

struct sx_node *
make_integer(const char *s, const size_t n, size_t *i)
{
    size_t j = *i;

    while (j < n) {
        if (isdigit(s[j]) == false)
            break;
        j++;
    }

    if ((j < n) && (nextisdelimiter(s[j]) == false)) {
        *i = j;
        return NULL;
    }

    struct sx_node *rv = malloc(sizeof *rv);
    if (rv == NULL) {
        sxoom(__FILE__, __LINE__);
    }

    rv->type = SXT_INTEGER;
    rv->data.u64 = 0;

    size_t mult = 1u;
    size_t save = j;
    j--;
    while (j >= *i) {
        rv->data.u64 += mult * digit2int(s[j]);
        mult *= 10u;
        if (j == 0u)
            break;
        j--;
    }
    *i = minu64(save, n);

    return rv;
}

struct sx_node *
make_hinteger(const char *s, const size_t n, size_t *i)
{
    size_t j = *i + 2u;

    while (j < n) {
        if (isxdigit(s[j]) == false)
            break;
        j++;
    }

    if ((j < n) && (nextisdelimiter(s[j+1]) == false)) {
        *i = j;
        return NULL;
    }

    struct sx_node *rv = malloc(sizeof *rv);
    if (rv == NULL) {
        sxoom(__FILE__, __LINE__);
    }

    rv->type = SXT_INTEGER;
    rv->data.u64 = 0;

    size_t mult = 1u;
    size_t save = j;
    j--;
    while (j >= *i) {
        rv->data.u64 += mult * digit2int(tolower(s[j]));
        mult *= 16u;
        if (j == 0u)
            break;
        j--;
    }
    *i = minu64(save, n);

    return rv;
}

struct sx_node *
make_pair(UNUSED const char *s, UNUSED const size_t n, size_t *i)
{
    *i += 1;

    struct sx_node *rv = calloc(sizeof *rv, 1u);
    if (rv == NULL) {
        sxoom(__FILE__, __LINE__);
    }

    rv->data.pair = calloc(sizeof *rv->data.pair, 1u);
    if (rv->data.pair == NULL) {
        sxoom(__FILE__, __LINE__);
    }

    rv->type = SXT_PAIR;
    return rv;
}

struct sx_node *
make_end_of_list(UNUSED const char *s, UNUSED const size_t n, size_t *i)
{
    *i += 1;

    struct sx_node *rv = malloc(sizeof *rv);
    if (rv == NULL) {
        sxoom(__FILE__, __LINE__);
    }

    rv->type = SXT_EMPTY_LIST;
    return rv;
}

enum sx_what
looking_at(const char *s, const size_t n, const size_t i)
{
    if ((n > i + 1) && s[i] == '#' && s[i+1] == 'x' && isxdigit(s[i+2])) {
        return LOOKING_AT_INT_HEX;
    }
    if (s[i] == '(') {
        return LOOKING_AT_PAREN_OPEN;
    }
    if (s[i] == ')') {
        return LOOKING_AT_PAREN_CLOSE;
    }
    if (isdigit(s[i])) {
        return LOOKING_AT_INT_DEC;
    }
    if (issyminitch(s[i])) {
        return LOOKING_AT_SYMBOL;
    }

    return LOOKING_AT_UNKNOWN;
}
