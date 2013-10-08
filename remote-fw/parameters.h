#ifndef INC_PARAMETERS_H
#define INC_PARAMETERS_H

#include "chip-remote.h"

void cr_destroy_params(struct cr_port *);
void cr_port_reset_line_roles(struct cr_port *);
void cr_destroy_port_map(struct cr_port *);

int cr_bool_param_get(struct cr_parameter *, char *);
int cr_uint_param_get(struct cr_parameter *, char *);
int cr_symb_param_get(struct cr_parameter *, char *, struct cr_symb_tab *);

char *cr_param_get(struct cr_parameter *, char *);
int cr_param_set(struct cr_parameter *, struct cr_words *, int, int);

void cr_param_init(struct cr_parameter *, int, char *, char *);
void cr_end_param_init(struct cr_parameter *, int);

#define CR_BOOL_PARAM_GET(p, k, val)            \
    do {                                        \
        int tmp;                                \
        tmp = cr_bool_param_get(p, k);          \
        if (tmp < 0)                            \
            return 0;                           \
        val = tmp;                              \
    } while (0)

#define CR_UINT_PARAM_GET(p, k, val)            \
    do {                                        \
        int tmp;                                \
        tmp = cr_uint_param_get(p, k);          \
        if (tmp < 0)                            \
            return 0;                           \
        val = tmp;                              \
    } while (0)

#define CR_SYMB_PARAM_GET(p, k, t, val)         \
    do {                                        \
        int tmp;                                \
        tmp = cr_symb_param_get(p, k, t);       \
        if (tmp < 0)                            \
            return 0;                           \
        val = tmp;                              \
    } while (0)

#endif /* INC_PARAMETERS_H */
