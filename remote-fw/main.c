#include <stdlib.h>

#include "chip-remote.h"
#include "platform.h"

#ifdef CR_SIM
int
main(int argc, char *argv[])
#else
void
main(void)
#endif
{
    cr_init(1);
    for (;;)
        cr_top_level();
#ifdef CR_SIM
    return EXIT_SUCCESS;
#endif
}
