/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stdlib.h>

#include "chip-remote.h"
#include "platform.h"
#include "utils.h"

/* -------------------------------------------------------------------------- */

#define DEFAULT_RATE { CR_IMMUTABLE, -1 }
#define NO_RATE { CR_IMMUTABLE, 0 }

#define NEW_LINE(a,d,m)   \
    {                      \
        a,                 \
        d,                 \
        m,                 \
        { (char)0 },       \
        CR_ROLE_NONE,      \
        CR_NO_INDEX,       \
        CR_MUTABLE         \
    }

#define LINE_LIST_END \
    { NULL, NULL, 0, { (char)0 }, CR_ROLE_NONE, CR_NO_INDEX, CR_MUTABLE }

#define NEW_PORT(n,l)                   \
    {                                   \
        n,                              \
        0,                              \
        DEFAULT_RATE,                   \
        { CR_MUTABLE, CR_MODE_NONE },   \
        NULL,                           \
        l,                              \
        NULL                            \
    }

#define PORT_LIST_END \
    { 0, 0, NO_RATE, { CR_IMMUTABLE, CR_MODE_INVALID }, NULL, NULL, NULL }

/* -------------------------------------------------------------------------- */

#ifdef CR_MSP430F1481

#include "arch/cr-msp430.h"

static struct cr_line port1_lines[] = {
    NEW_LINE(access_port5, dir_port5, 1<<0),
    NEW_LINE(access_port5, dir_port5, 1<<1),
    NEW_LINE(access_port5, dir_port5, 1<<2),
    NEW_LINE(access_port5, dir_port5, 1<<3),
    LINE_LIST_END
};

struct cr_port cr_ports[] = {
    NEW_PORT(4, port1_lines)
};

#endif /* CR_MSP430F1481 */

#ifdef CR_STDOUT

#include "arch/stdout.h"

static struct cr_line port1_lines[] = {
    NEW_LINE(access_portA, dir_portA, 1<<0),
    NEW_LINE(access_portA, dir_portA, 1<<1),
    NEW_LINE(access_portA, dir_portA, 1<<2),
    NEW_LINE(access_portA, dir_portA, 1<<3),
    LINE_LIST_END
};

static struct cr_line port2_lines[] = {
    NEW_LINE(access_portA, dir_portA, 1<<4),
    NEW_LINE(access_portA, dir_portA, 1<<5),
    NEW_LINE(access_portA, dir_portA, 1<<6),
    NEW_LINE(access_portA, dir_portA, 1<<7),
    NEW_LINE(access_portA, dir_portA, 1<<8),
    NEW_LINE(access_portA, dir_portA, 1<<9),
    NEW_LINE(access_portA, dir_portA, 1<<10),
    NEW_LINE(access_portA, dir_portA, 1<<11),
    NEW_LINE(access_portA, dir_portA, 1<<12),
    NEW_LINE(access_portA, dir_portA, 1<<13),
    NEW_LINE(access_portA, dir_portA, 1<<14),
    NEW_LINE(access_portA, dir_portA, 1<<15),
    LINE_LIST_END
};

struct cr_port cr_ports[] = {
    NEW_PORT(4, port1_lines),
    NEW_PORT(12, port2_lines),
    PORT_LIST_END
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
    /*
     * This is where you'd perform default port configuration. If you don't,
     * all ports defined above do not have any default configuration with all
     * lines and parameters are marked as mutable.
     */

#ifdef CR_MSP430F1481

#if 0
#define MEASURE_CPU_SPEED
#endif

#define UART_SEL (P3SEL)
#define UART_OUT (P3OUT)
#define UART_IN  (P3IN)
#define UART_DIR (P3DIR)
#define UART_TX  (0x10)
#define UART_RX  (0x20)
/* This firmware doesn't use CTS or RTS in MSP430F1481 mode at this point. */
#define UART_CTS (0x40)
#define UART_RTS (0x80)

    /* Disable watchdog */
    WDTCTL = WDTPW + WDTHOLD;

    /* clock initialisation */
    DCOCTL = DCO2|DCO1|DCO0;
    BCSCTL1 = XT2OFF|RSEL2|RSEL1|RSEL0;
    /* DCOR == 1 means: "use an external resistor at P2.5" */
    BITMASK_SET(BCSCTL2, DCOR);

#ifdef MEASURE_CPU_SPEED
    /* Make P5.0 through P5.5 outputs */
    P5DIR |= 0x1f;
    P5OUT = 0x00;
    /*
     * This is an endless loop, which does bit-set, bit-clear on bit 1 of
     * `0x31' (which is where Port 5 is located); thus toggling the pin.
     *
     * The loop below takes 12 cpu cycles: 5 cycles per bit-set/clear (with
     * this particular addressing scheme) and 2 cycles for the jump.
     *
     * On a prototype, this loop toggles pin P5.0 with a cycle period of
     * 1.602 micro-seconds (roughly 624kHz). That means that the CPU core
     * clk is running at 12 * 624kHz = 7.488MHz.
     *
     * You obviously don't have to measure core clk-speed if you're using an
     * external oscillator as a clock source.
     */
    asm("MEASURE: bis.b #0x01,&0x31");
    asm("         bic.b #0x01,&0x31");
    asm("         jmp MEASURE");

#endif /* MEASURE_CPU_SPEED */

    /* Reset uart state machine */
    BITMASK_SET(UCTL0,SWRST);

    /*
     * uart calculator: http://mspgcc.sourceforge.net/baudrate.html
     * this program license is at:
     *
     *     http://www.fsf.org/licenses/licenses.html#GPL
     *
     * this program is distributed WITHOUT ANY WARRANTY
     *
     * clock: 7488000Hz
     * desired baud rate: 19200bps
     * division factor: 390
     * effective baud rate: 19200bps
     * maximum error: 0us   0.00%
     *
     * time table (microseconds):
     * event      desired effective  error   error%
     * startbit->D0     52.08     52.08       +0  +0.00
     * D0->D1          104.17    104.17       +0  +0.00
     * D1->D2          156.25    156.25       +0  +0.00
     * D2->D3          208.33    208.33       +0  +0.00
     * D3->D4          260.42    260.42       +0  +0.00
     * D4->D5          312.50    312.50       +0  +0.00
     * D5->D6          364.58    364.58       +0  +0.00
     * D6->D7          416.67    416.67       +0  +0.00
     * D7->stopbit     468.75    468.75       +0  +0.00
     * end of stopb    520.83    520.83       +0  +0.00
     */

    /* uart0 7488000Hz 19200bps */
    UBR00=0x86;
    UBR10=0x01;
    UMCTL0=0x00;

    /*
     * We want the uart module to talk in 8N1 mode, symbol-rate is determined
     * above. Setting the CHAR bit means 8-Bit words. Clearing PENA means that
     * parity bit generation is disabled. And clearing SPB mean that only one
     * stop-bit is generated. Thus, 8N1.
     */
    BITMASK_SET(UCTL0, CHAR);
    BITMASK_CLEAR(UCTL0, PENA | SPB);

    /* Set clock input SMCLK */
    BITMASK_SET(U0TCTL, SSEL1 | SSEL0);

    /* Set module functionality for UART0 */
    BITMASK_SET(ME1, UTXE0|URXE0);
    BITMASK_SET(UART_SEL, UART_TX | UART_RX);

    /* Set RTS as output, CTS as input */
    BITMASK_SET(UART_DIR, UART_RTS);
    BITMASK_CLEAR(UART_DIR, UART_CTS);
    BITMASK_CLEAR(UART_SEL, UART_RTS);
    BITMASK_CLEAR(UART_SEL, UART_CTS);

    /* Initialise uart state machine */
    BITMASK_CLEAR(UCTL0, SWRST);

    /* Enable RX Interrupt */
    BITMASK_SET(IE1, URXIE0);

    /*
     * Enable interrupt processing
     *
     * Without this, ISRs are NOT entered even if a given interupt (like
     * URXIE0) is explicitly enabled!
     */
    asm(" eint");

#endif /* CR_MSP430F1481 */

    cr_init(1);
    for (;;)
        cr_top_level();

#ifdef CR_SIM
    return EXIT_SUCCESS;
#endif
}
