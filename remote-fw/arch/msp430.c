#include <stdint.h>

#include "../chip-remote.h"
#include "../platform.h"
#include "../config.msp430.h"

#include "cr-msp430.h"

int
access_port1(struct cr_line *line, enum cr_access_mode mode, int value)
{
    return 0;
}

void
xcr_pre_top_level(void)
{
    /*
     * This function could be used to poll data from the UART port until a line
     * is complete and then work on that. The simulator version of the firmware
     * does exactly that. But on this microcontroller, we do have interupts and
     * we will use that to handle incoming data instead.  This doesn't have any
     * work to do, therefore.
     */
}

void
xcr_post_bye(void)
{
    /*
     * The xcr_post_bye function is mostly in place for the simulator version
     * of the firmware to be able to exit after the conversation is done. The
     * actual firmware should just wait for the next conversion. So this is a
     * no-op here.
     */
}

void
xcr_send_host(char *buf)
{
}

void
xcr_wait(uint32_t n)
{
    uint32_t i;

    for (i = 0; i < n; ++i)
        /* NOP */;
}
