#ifndef INC_CR_UTILITIES_H
#define INC_CR_UTILITIES_H

#include <stdint.h>

#include "cr-process.h"

void cr_proto_put_space(const struct cr_protocol*);
void cr_proto_put_newline(const struct cr_protocol*);
void cr_proto_put_u32(const struct cr_protocol*, uint32_t);

#endif /* INC_CR_UTILITIES_H */
