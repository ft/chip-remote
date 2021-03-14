/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file parse-string.c
 * @brief Chip-remote protocol parser
 */

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include <commands.h>
#include <chip-remote.h>
#include <cr-utilities.h>
#include <parse-string.h>

struct word {
    char *start;
    char *end;
};

static bool space(char);
static bool eos(char);
static char* find_token_separator(char*);
static char* find_token_start(char*);
static struct word next_word(char*);
static inline void split_input_at(char*);
static inline bool spec_missing_arg(const struct cr_command*, unsigned int);
static inline bool string_bool_true(const char*);
static inline bool string_bool_false(const char*);

static bool
space(const char c)
{
    return (c == ' ');
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

static inline bool
spec_missing_arg(const struct cr_command *cmd, unsigned int argn)
{
    const struct cr_argument *arg = cmd->args + argn;
    return ((arg->type != CR_PROTO_ARG_TYPE_VOID) && arg->optional == false);
}

static struct cr_value
parse_argument(string_sink reply, const struct cr_argument *spec, char *input)
{
    struct cr_value result;
    result.type = spec->type;
    switch(spec->type) {
    case CR_PROTO_ARG_TYPE_BOOLEAN:
        if (string_bool_true(input)) {
            result.data.boolean = true;
        } else if (string_bool_false(input)) {
            result.data.boolean = false;
        } else {
            reply("BROKEN-VALUE Not a boolean: ");
            reply(input);
            reply("\n");
            result.type = CR_PROTO_ARG_TYPE_VOID;
        }
        break;
    case CR_PROTO_ARG_TYPE_INTEGER: {
        int error = 0;
        result.data.u32 = cr_parse_u32(input, &error);
        if (error != 0) {
            reply("BROKEN-VALUE Not an integer: ");
            reply(input);
            reply("\n");
            result.type = CR_PROTO_ARG_TYPE_VOID;
        }
    } break;
    case CR_PROTO_ARG_TYPE_STRING:
        result.data.string = input;
        break;
    default:
        break;
    }

    return result;
}

static inline bool
string_bool_true(const char *input)
{
    return (strcmp(input, "TRUE") == 0);
}

static inline bool
string_bool_false(const char *input)
{
    return (strcmp(input, "FALSE") == 0);
}

enum cr_proto_result
cr_parse_string(string_sink reply,
                char *input,
                struct cr_proto_parse *result)
{
    /* Find first word in input buffer and do a command lookup on it */
    const char *end_of_input = input + strlen(input);
    struct word current = next_word(input);
    struct cr_command *cmd = cr_lookup_command(current.start);

    if (cmd->id == CR_PROTO_CMD_UNKNOWN) {
        reply("WTF Unknown command: ");
        reply(current.start);
        reply("\n");
        return CR_PROTO_RESULT_WTF;
    }

    /* Iterate through arguments and parse them according to spec */
    unsigned int argn = 0u;
    while (current.end < end_of_input) {
        /* Make sure we're not overflowing argument list */
        if (argn >= CR_PROTO_MAX_ARGS) {
            reply("MALFORMED-REQUEST Broken command definition. This is a bug!");
            reply("\n");
            return CR_PROTO_RESULT_MALFORMED;
        }

        struct cr_argument *spec = cmd->args + argn;
        /* Check if we're exceeding the command's argument spec */
        if (spec->type == CR_PROTO_ARG_TYPE_VOID) {
            reply("MALFORMED-REQUEST Too many arguments");
            reply("\n");
            return CR_PROTO_RESULT_MALFORMED;
        }

        current = next_word(current.end + 1);
        result->args[argn] = parse_argument(reply, spec, current.start);
        if (result->args[argn].type == CR_PROTO_ARG_TYPE_VOID) {
            return CR_PROTO_RESULT_BROKEN_VALUE;
        }
        argn++;
    }

    if (spec_missing_arg(cmd, argn)) {
        reply("MALFORMED-REQUEST Not enough arguments");
        reply("\n");
        return CR_PROTO_RESULT_MALFORMED;
    }

    /* All good; Before returning make sure all result data is correct. */
    result->cmd = cmd;
    result->argn = argn;
    return CR_PROTO_RESULT_OK;
}
