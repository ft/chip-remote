#ifndef INC_CR_UTILITIES_H
#define INC_CR_UTILITIES_H

#include <stdint.h>
#include <string.h>

#include <chip-remote.h>
#include <cr-process.h>

void cr_proto_put_space(const struct cr_protocol*);
void cr_proto_put_newline(const struct cr_protocol*);
void cr_proto_put_number(const struct cr_protocol*, cr_number);
cr_number cr_parse_number(const char*, unsigned int, char**, unsigned int*);

static inline bool
string_bool_true(const char *input)
{
    return (strcmp(input, "true") == 0);
}

static inline bool
string_bool_false(const char *input)
{
    return (strcmp(input, "false") == 0);
}

#endif /* INC_CR_UTILITIES_H */
