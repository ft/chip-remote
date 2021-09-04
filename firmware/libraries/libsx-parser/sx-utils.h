#ifndef INC_SX_UTILS_H
#define INC_SX_UTILS_H

#include <stdbool.h>
#include <stddef.h>

#include "sx-types.h"

enum sx_what {
    LOOKING_AT_UNKNOWN,
    LOOKING_AT_SYMBOL,
    LOOKING_AT_INT_DEC,
    LOOKING_AT_INT_HEX,
    LOOKING_AT_PAREN_OPEN,
    LOOKING_AT_PAREN_CLOSE
};

bool issyminitch(char);
bool issymch(char);
enum sx_what looking_at(const char*, size_t, size_t);
struct sx_node *make_symbol(const char*, size_t, size_t*);
struct sx_node *make_integer(const char*, size_t, size_t*);
struct sx_node *make_hinteger(const char*, size_t, size_t*);
struct sx_node *make_pair(const char*, size_t, size_t*);
struct sx_node *make_end_of_list(const char*, size_t, size_t*);

#endif /* INC_SX_UTILS_H */
