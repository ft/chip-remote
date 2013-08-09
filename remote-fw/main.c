#include <stdlib.h>

#include "chip-remote.h"
#include "platform.h"
#include "port.h"

#ifdef CR_MSP430F1481

static struct cr_line port1_lines[] = {
    { P1IN_, P1OUT_, 0x01 },
    { P1IN_, P1OUT_, 0x02 },
    { P1IN_, P1OUT_, 0x04 },
    { P1IN_, P1OUT_, 0x08 }
};

static struct cr_line port2_lines[] = {
    { P1IN_, P1OUT_, 0x10 },
    { P1IN_, P1OUT_, 0x20 },
    { P1IN_, P1OUT_, 0x40 },
    { P1IN_, P1OUT_, 0x80 }
};

struct cr_port cr_ports[] = {
    { NULL, 4, port1_lines },
    { NULL, 4, port2_lines },
    { NULL, 0, NULL }
};

#endif /* CR_MSP430F1481 */

#ifdef CR_STDOUT

static struct cr_line port1_lines[] = {
    { 0x3200abcd, 0x3200abce, 1<<0 },
    { 0x3200abcd, 0x3200abce, 1<<1 },
    { 0x3200abcd, 0x3200abce, 1<<2 },
    { 0x3200abcd, 0x3200abce, 1<<3 }
};

static struct cr_line port2_lines[] = {
    { 0x3200abcd, 0x3200abce, 1<<4 },
    { 0x3200abcd, 0x3200abce, 1<<5 },
    { 0x3200abcd, 0x3200abce, 1<<6 },
    { 0x3200abcd, 0x3200abce, 1<<7 },
    { 0x3200abcd, 0x3200abce, 1<<8 },
    { 0x3200abcd, 0x3200abce, 1<<9 },
    { 0x3200abcd, 0x3200abce, 1<<10 },
    { 0x3200abcd, 0x3200abce, 1<<11 }
};

struct cr_port cr_ports[] = {
    { NULL, 4, port1_lines },
    { NULL, 8, port2_lines },
    { NULL, 0, NULL }
};

#endif /* CR_STDOUT */

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
