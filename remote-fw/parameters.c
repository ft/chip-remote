#include <limits.h>

#include "chip-remote.h"
#include "parameters.h"
#include "properties.h"
#include "protocol.h"
#include "utils.h"

static struct cr_symb_tab bool_table[] = {
    { "TRUE", 1 },
    { "FALSE", 0 },
    { "true", 1 },
    { "false", 0 },
    { "ON", 1 },
    { "OFF", 0 },
    { "on", 1 },
    { "off", 0 },
    { "1", 1 },
    { "0", 0 },
    { NULL, -1 }
};

void
cr_destroy_params(struct cr_port *port)
{
    free(port->params);
    port->params = NULL;
}

void
cr_port_reset_line_roles(struct cr_port *port)
{
}

int
cr_uint_param_get(struct cr_parameter *p, char *k)
{
    uint32_t retval;
    int err;
    char *value;

    value = cr_param_get(p, k);
    if (value == NULL)
        return -1;

    retval = str2uint(value, 0, &err);
    if (err)
        return -1;
    if (retval > INT_MAX)
        return -1;

    return (int)retval;
}

int
cr_bool_param_get(struct cr_parameter *p, char *k)
{
    return cr_symb_param_get(p, k, bool_table);
}

int
cr_symb_param_get(struct cr_parameter *p, char *k, struct cr_symb_tab *t)
{
    char *symbol;
    int i;

    symbol = cr_param_get(p, k);
    if (symbol == NULL) {
        cr_unknown_param(k);
        return -1;
    }

    for (i = 0; t[i].symbol != NULL; ++i)
        if (STREQ(t[i].symbol, symbol))
            return t[i].value;

    cr_broken_param(k, symbol);
    return -1;
}

char *
cr_param_get(struct cr_parameter *params, char *key)
{
    int i;

    for (i = 0; params[i].name != NULL; ++i)
        if (STREQ(params[i].name, key))
            return params[i].value.value;
    return NULL;
}

int
cr_param_set(struct cr_parameter *params,
             struct cr_words *words, int key, int value)
{
    int i;

    for (i = 0; params[i].name != NULL; ++i)
        if (STREQ_N(params[i].name,
                    words->word[key].start,
                    words->word[key].length))
        {
            if (!params[i].value.mutable_p)
                return 0;

            cr_string_prop_set_n(&(params[i].value),
                                 words->word[value].start,
                                 words->word[value].length,
                                 CR_MUTABLE);
            return 1;
        }
    return -1;
}

void
cr_param_init(struct cr_parameter *params, int idx, char *key, char *value)
{
    params[idx].name = key;
    cr_string_prop_set(&(params[idx].value), value, CR_MUTABLE);
}

void
cr_end_param_init(struct cr_parameter *params, int idx)
{
    params[idx].name = NULL;
    params[idx].value.value[0] = '\0';
    params[idx].value.mutable_p = CR_IMMUTABLE;
}
