#ifndef INC_LINE_H
#define INC_LINE_H

#include "chip-remote.h"

struct cr_line *cr_get_line(struct cr_port *, char *);
struct cr_line *cr_get_indexed_line(struct cr_port *, char *, int);

#endif /* INC_LINE_H */