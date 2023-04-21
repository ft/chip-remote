/*
 * Copyright (c) 2020-2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file parse-string.c
 * @brief Chip-remote protocol parser
 */

#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#include <ufw/compiler.h>

#include <commands.h>
#include <chip-remote.h>
#include <cr-utilities.h>
#include <parse-string.h>

enum scanner_scalar {
    SCANNER_INT_DEFAULT,
    SCANNER_INT_BINARY,
    SCANNER_INT_OCTAL,
    SCANNER_INT_DECIMAL,
    SCANNER_INT_HEXADECIMAL,
    SCANNER_BOOLEAN,
    SCANNER_SYMBOL
};

struct scanner_pair {
    char *sep;
    enum scanner_scalar a;
    enum scanner_scalar b;
};

enum scanner_type {
    /** Looked like something but that something was invalid */
    SCANNER_INVALID,
    /** Sub-scanner doesn't know what it's looking at */
    SCANNER_NOTHING,
    /** Found a scalar value */
    SCANNER_SCALAR,
    /** Found a pair value */
    SCANNER_PAIR
};

struct scanner_hint {
    enum scanner_type type;
    union {
        enum scanner_scalar scalar;
        struct scanner_pair pair;
    } info;
};

#define SH_INIT { .type = SCANNER_NOTHING }

static inline void
sh_scalar(struct scanner_hint *sh, enum scanner_scalar scalar)
{
    sh->type = SCANNER_SCALAR;
    sh->info.scalar = scalar;
}

static inline void
sh_pair(struct scanner_hint *sh, char *sep,
        enum scanner_scalar a, enum scanner_scalar b)
{
    sh->type = SCANNER_PAIR;
    sh->info.pair.sep = sep;
    sh->info.pair.a = a;
    sh->info.pair.b = b;
}

static inline void
sh_invalid(struct scanner_hint *sh)
{
    sh->type = SCANNER_INVALID;
}

struct word {
    char *start;
    char *end;
};

static inline size_t word_length(const struct word*);
static bool space(char);
static bool eos(char);
static char* find_token_separator(char*);
static char* find_token_start(char*);
static struct word next_word(char*);
static inline void split_input_at(char*);

static inline size_t
word_length(const struct word *w)
{
    return w->end - w->start;
}

static bool
space(const char c)
{
    return (c == ' ' || c == '\t');
}

static bool
eos(const char c)
{
    return (c == '\0');
}

static char *
find_token_separator(char *input)
{
    while ((space(*input) || eos(*input)) == false)
        input++;

    return input;
}

static char *
find_token_start(char *input)
{
    while (space(*input))
        input++;

    return input;
}

static inline void
split_input_at(char *ptr)
{
    *ptr = '\0';
}

static struct word
next_word(char *input)
{
    char *start = find_token_start(input);
    struct word result = {
        .start = start,
        .end = find_token_separator(start)
    };
    split_input_at(result.end);
    return result;
}

void
cr_parser_init(struct cr_parser_state *p, char *s)
{
    p->input = s;
    p->length = strlen(s);
    p->position = 0u;
}

void
cr_tokens_init(struct cr_tokens *t, struct cr_value *d, size_t n)
{
    t->token = d;
    t->size = n;
    t->used = 0u;
}

static int
c_from_set(const int c, const int *set, const size_t n)
{
    for (size_t i = 0u; i < n; ++i) {
        if (c == set[i])
            return 1;
    }
    return 0;
}

static int
issymbolch(const int c)
{
    static int set[] = { '-', '_', '/', '+' };
    return (isalnum(c) || c_from_set(c, set, sizeof(set) / sizeof(int)));
}

static int
issymbolinitch(const int c)
{
    return ((isdigit(c) == false) && issymbolch(c));
}

static int
isbdigit(const int c)
{
    static int set[] = { '0', '1' };
    return c_from_set(c, set, 2u);
}

static int
isodigit(const int c)
{
    static int set[] = { '0', '1', '2', '3', '4', '5', '6', '7' };
    return c_from_set(c, set, 8u);
}

static bool
looking_at(const char *s, const size_t sn, const char *prefix, const size_t pn)
{
    return ((sn >= pn) && (strncmp(s, prefix, pn) == 0));
}

static bool
looking_at_predicate(const char *s, const size_t sn, int(*pred)(int))
{
    for (size_t i = 0u; i < sn; ++i) {
        if (pred(s[i]) == 0)
            return false;
    }
    return true;
}

typedef int (*chpredicate)(int);

struct number_format {
    char *prefix;
    size_t prefix_len;
    chpredicate predicate;
    enum scanner_scalar type;
} number_formats[] = {
    { "b#", 2u, isbdigit, SCANNER_INT_BINARY      },
    { "o#", 2u, isodigit, SCANNER_INT_OCTAL       },
    { "d#", 2u, isdigit,  SCANNER_INT_DECIMAL     },
    { "h#", 2u, isxdigit, SCANNER_INT_HEXADECIMAL },
    { NULL, 0u, isdigit,  SCANNER_INT_DEFAULT     },
    { NULL, 0u, NULL,     SCANNER_BOOLEAN }
};

static struct scanner_hint
looking_at_number(const char *s)
{
    struct scanner_hint rv = SH_INIT;
    const size_t sn = strlen(s);

    if (sn == 0u) {
        return rv;
    }

    for (size_t i = 0u; number_formats[i].predicate != NULL; ++i) {
        struct number_format *nf = number_formats + i;
        if (nf->prefix == NULL) {
            if (looking_at_predicate(s, sn, nf->predicate)) {
                sh_scalar(&rv, nf->type);
                break;
            }
        } else {
            if (looking_at(s, sn, nf->prefix, nf->prefix_len) == false) {
                continue;
            }
            sh_scalar(&rv, nf->type);
            size_t restlen = sn - nf->prefix_len;
            const char *rest = s + nf->prefix_len;
            if (looking_at_predicate(rest, restlen, nf->predicate) == false) {
                sh_invalid(&rv);
            }
            break;
        }
    }

    return rv;
}

static struct scanner_hint
looking_at_bool(const char *s)
{
    struct scanner_hint rv = SH_INIT;

    if ((strcmp(s, "true") == 0) || (strcmp(s, "false") == 0)) {
        sh_scalar(&rv, SCANNER_BOOLEAN);
    }

    return rv;
}

static struct scanner_hint
looking_at_keyvalue(const char *s)
{
    struct scanner_hint rv = SH_INIT;

    /* Key can be anything and will reach up to the last colon in the string.
     * Everything after the colon has to be a number. */
    char *colon = strrchr(s, ':');
    if (colon == NULL)
        return rv;

    struct scanner_hint vh = looking_at_number(colon + 1);
    if (vh.type == SCANNER_SCALAR) {
        sh_pair(&rv, colon, SCANNER_SYMBOL, vh.info.scalar);
    }

    return rv;
}

static bool
issymbol(const char *s)
{
    const size_t len = strlen(s);
    return (len > 0u) && issymbolinitch(*s)
        && ((len == 1u) || looking_at_predicate(s + 1u, len - 1u, issymbolch));
}

static struct scanner_hint
looking_at_symbol(const char *s)
{
    struct scanner_hint rv = SH_INIT;

    if (issymbol(s)) {
        sh_scalar(&rv, SCANNER_SYMBOL);
    }

    return rv;
}

static enum cr_parser_result
parse_number_real(cr_number *n, char *buf, struct scanner_hint *h)
{
    enum scanner_scalar typehint;

    switch (h->type) {
    case SCANNER_SCALAR: typehint = h->info.scalar; break;
    case SCANNER_PAIR:   typehint = h->info.pair.b; break;
    default: assert(false);
    }

    unsigned int offset = 2u, base = 10u, error;
    char *stop;

    switch (typehint) {
    case SCANNER_INT_BINARY:      base = 2u;   break;
    case SCANNER_INT_OCTAL:       base = 8u;   break;
    case SCANNER_INT_HEXADECIMAL: base = 16u;  break;
    case SCANNER_INT_DECIMAL:                  break;
    case SCANNER_INT_DEFAULT:     offset = 0u; break;
    default: return CR_PARSER_GENERIC_FAILURE;
    }

    *n = cr_parse_number(buf + offset, base, &stop, &error);
    switch (error) {
    case 0: return CR_PARSER_SUCCESS;
    case 1:
        switch (base) {
        case 2:  return CR_PARSER_INVALID_DIGIT_BIN;
        case 8:  return CR_PARSER_INVALID_DIGIT_OCT;
        case 10: return CR_PARSER_INVALID_DIGIT_DEC;
        case 16: return CR_PARSER_INVALID_DIGIT_HEX;
        default: return CR_PARSER_GENERIC_FAILURE;
        }
    case 2:  return CR_PARSER_VALUE_OUT_OF_RANGE;
    default: return CR_PARSER_GENERIC_FAILURE;
    }
}

static enum cr_parser_result
parse_number(struct cr_tokens *t, struct word *w, struct scanner_hint *h)
{
    cr_number n;
    enum cr_parser_result rv = parse_number_real(&n, w->start, h);
    if (rv == CR_PARSER_SUCCESS) {
        struct cr_value *cur = t->token + t->used;
        cur->data.number = n;
        cur->type = CR_PROTO_ARG_TYPE_INTEGER;
        t->used++;
    }

    return rv;
}

static enum cr_parser_result
parse_boolean(struct cr_tokens *t, struct word *w, UNUSED struct scanner_hint *h)
{
    struct cr_value *cur = t->token + t->used;
    cur->type = CR_PROTO_ARG_TYPE_BOOLEAN;

    if (string_bool_true(w->start)) {
        cur->data.boolean = true;
    } else if (string_bool_false(w->start)) {
        cur->data.boolean = false;
    } else {
        return CR_PARSER_GENERIC_FAILURE;
    }

    t->used++;
    return CR_PARSER_SUCCESS;
}

static enum cr_parser_result
parse_keyvalue(struct cr_tokens *t, struct word *w, struct scanner_hint *h)
{
    cr_number n;

    /* The scanner left us a hint as to where the separator is, lets insert a
     * NUL byte to make the key a possible symbol. */
    *h->info.pair.sep = '\0';

    if (issymbol(w->start) == false) {
        return CR_PARSER_GENERIC_FAILURE;
    }

    enum cr_parser_result rv = parse_number_real(&n, h->info.pair.sep + 1u, h);
    if (rv != CR_PARSER_SUCCESS) {
        return rv;
    }

    struct cr_value *cur = t->token + t->used;
    cur->type = CR_PROTO_ARG_TYPE_KEYVALUE;
    cur->data.kv.key = w->start;
    cur->data.kv.value = n;
    t->used++;

    return CR_PARSER_SUCCESS;
}

static enum cr_parser_result
parse_symbol(struct cr_tokens *t, struct word *w, UNUSED struct scanner_hint *h)
{
    struct cr_value *cur = t->token + t->used;
    cur->data.symbol = w->start;
    cur->type = CR_PROTO_ARG_TYPE_SYMBOL;
    t->used++;
    return CR_PARSER_SUCCESS;
}

typedef struct scanner_hint (*scanner)(const char *);
typedef enum cr_parser_result (*parser)(struct cr_tokens*,
                                        struct word*,
                                        struct scanner_hint*);

struct {
    scanner scan;
    parser parse;
} s2p[] = {
    { looking_at_number,   parse_number   },
    { looking_at_bool,     parse_boolean  },
    { looking_at_keyvalue, parse_keyvalue },
    { looking_at_symbol,   parse_symbol   },
    { NULL, NULL }
};

static enum cr_parser_result
cr_parse_token(struct cr_tokens *t, struct word *w)
{
    if (t->used == t->size) {
        return CR_PARSER_TOO_MANY_TOKENS;
    }

    struct scanner_hint hint = SH_INIT;
    for (size_t i = 0u; s2p[i].scan != NULL; ++i) {
        hint = s2p[i].scan(w->start);
        if (hint.type == SCANNER_INVALID) {
            break;
        } else if (hint.type != SCANNER_NOTHING) {
            return s2p[i].parse(t, w, &hint);
        }
    }

    if (hint.type == SCANNER_INVALID) {
        switch (hint.info.scalar) {
        case SCANNER_INT_BINARY:
            return CR_PARSER_INVALID_DIGIT_BIN;
        case SCANNER_INT_OCTAL:
            return CR_PARSER_INVALID_DIGIT_OCT;
        case SCANNER_INT_HEXADECIMAL:
            return CR_PARSER_INVALID_DIGIT_HEX;
        case SCANNER_INT_DEFAULT:
            /* FALLTHROUGH */
        case SCANNER_INT_DECIMAL:
            return CR_PARSER_INVALID_DIGIT_DEC;
        default:
            break;
        }
    }

    return CR_PARSER_GENERIC_FAILURE;
}

static inline bool
parser_done(const struct cr_parser_state *p)
{
    return (p->position >= p->length);
}

enum cr_parser_result
cr_parse(struct cr_parser_state *p, struct cr_tokens *t)
{
    while (parser_done(p) == false) {
        /* Find start of next word, skipping possible whitespace */
        char *input = p->input + p->position;
        struct word w = next_word(input);
        p->position += w.start - input;
        if (parser_done(p)) {
            break;
        }
        /* Add NUL byte to mark end of token */
        split_input_at(w.end);
        /* Parse current token */
        enum cr_parser_result rv = cr_parse_token(t, &w);
        if (rv != CR_PARSER_SUCCESS) {
            return rv;
        }
        /* Move parser state past current token */
        p->position += word_length(&w) + 1u;
   }

    return CR_PARSER_SUCCESS;
}
