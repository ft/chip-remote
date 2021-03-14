#ifndef INC_CR_UTILITIES_H
#define INC_CR_UTILITIES_H

#include <stdint.h>
#include <string.h>

#include "cr-process.h"

void cr_proto_put_space(const struct cr_protocol*);
void cr_proto_put_newline(const struct cr_protocol*);
void cr_proto_put_u32(const struct cr_protocol*, uint32_t);
uint32_t cr_parse_u32(const char*, int*);


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

#endif /* INC_CR_UTILITIES_H */
