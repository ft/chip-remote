#ifndef INC_PROPERTIES_H
#define INC_PROPERTIES_H

#include "chip-remote.h"

void cr_int_prop_set(struct cr_int_prop *, int, int);
void cr_string_prop_set(struct cr_string_prop *, char *, int);

#endif /* INC_PROPERTIES_H */
