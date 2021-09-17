#ifndef INC_SX_PARSER_H
#define INC_SX_PARSER_H

#include <stddef.h>

#include "sx-types.h"

#define SX_PARSER_INIT { .state = SXS_INIT, .error = SXE_NONE, .position = 0u }

struct sx_parse_result sx_parse_string(const char*);
struct sx_parse_result sx_parse_stringn(const char*, size_t);
struct sx_parse_result sx_parse(const char*, size_t, size_t);
struct sx_parse_result sx_parse_token(const char*, size_t, size_t);
void sx_destroy(struct sx_node**);

struct sx_node *sx_cxr(struct sx_node*, const char*);
struct sx_node *sx_pop(struct sx_node**);
struct sx_node *sx_append(struct sx_node*, struct sx_node*);
void sx_foreach(struct sx_node*, sx_nodefnc, void*);

#endif /* INC_SX_PARSER_H */
