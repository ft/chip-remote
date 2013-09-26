#include <string.h>
#include "chip-remote.h"
#include "properties.h"

void
cr_int_prop_set(struct cr_int_prop *p, int value, int mutable)
{
    p->value = value;
    p->mutable_p = mutable;
}

void
cr_string_prop_set(struct cr_string_prop *p, char *value, int mutable)
{
    strncpy(p->value, value, CR_STRING_PROP_MAX + 1);
    p->value[CR_STRING_PROP_MAX] = '\0';
    p->mutable_p = mutable;
}
