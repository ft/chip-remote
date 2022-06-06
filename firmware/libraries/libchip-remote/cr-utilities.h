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
void cr_number_to_bytes(cr_number, uint8_t*);
cr_number cr_number_from_bytes(uint8_t*);

bool cr_require_arg_type(struct cr_protocol*, struct cr_value*,
                      unsigned int, enum cr_argument_type);
bool cr_require_numofargs(struct cr_protocol*, cr_number, cr_number);
bool cr_unknown_port(struct cr_protocol*, struct cr_port*);
bool cr_value_max(struct cr_protocol*, struct cr_value*, unsigned int,
                  cr_number);

#define REQUIRE_ARG_TYPE(p,v,n,t)                       \
    cr_require_arg_type(p,v,n, CR_PROTO_ARG_TYPE_ ## t)

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
