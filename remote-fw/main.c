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

#define NEW_PORT(n,l)                           \
    {                                           \
        n,                                      \
        0,                                      \
        DEFAULT_RATE,                           \
        { CR_MUTABLE, CR_MODE_NONE, { NULL } }, \
        NULL,                                   \
        l,                                      \
        NULL                                    \
    }

#define PORT_LIST_END \
    { 0, 0, NO_RATE, { CR_IMMUTABLE, CR_MODE_INVALID, {NULL}},  \
      NULL, NULL, NULL }

/* -------------------------------------------------------------------------- */

#ifdef CR_MSP430F1481

#include "arch/cr-msp430.h"

/*
 * Ports in the adu1e9 project:
 *
 * These are the links to the FPGA:
 *
 * P1.0 -> NC    P2.0 -> L22   P3.0 -> DIP0
 * P1.1 -> NC    P2.1 -> M21   P3.1 -> DIP1
 * P1.2 -> NC    P2.2 -> P20   P3.2 -> DIP2
 * P1.3 -> NC    P2.3 -> P19   P3.3 -> DIP3
 * P1.4 -> K23   P2.4 -> P26   P3.4 -> UART_TX
 * P1.5 -> L24   P2.5 -> R26   P3.5 -> UART_RX
 * P1.6 -> L23   P2.6 -> N24   P3.6 -> LED0
 * P1.7 -> M24   P2.7 -> P25   P3.7 -> LED1
 *
 * P4.0 -> C24   P5.0 -> R22   P6.0 -> T25
 * P4.1 -> B25   P5.1 -> T24   P6.1 -> N26
 * P4.2 -> M19   P5.2 -> T23   P6.2 -> M25
 * P4.3 -> M22   P5.3 -> T22   P6.3 -> L25
 * P4.4 -> N23   P5.4 -> R21   P6.4 -> K26
 * P4.5 -> P24   P5.5 -> R20   P6.5 -> L26
 * P4.6 -> P23   P5.6 -> B26   P6.6 -> K25
 * P4.7 -> R23   P5.7 -> D25   P6.7 -> M26
 *
 * That's 5*8 - 4 = 36 pins.
 *
 * There are three devices using a regular SPI interface. One device with SPI
 * plus five other pins. The FPGA is controlled via the parex interface, that
 * needs CLK, 3*ADDR and 8*DATA pins. Thus:
 *
 * 4*4 + 5 + 1 + 3 + 8 pins = 33 pins.
 *
 * In order to limit the amount of memory required for the port mapping (the
 * MSP430f1481 only sports 2KiB of RAM), let's group the SPI devices onto one
 * SPI port with four chip-select lines. The parex port gets its own cr-port.
 */

static struct cr_line port1_lines[] = {
    /* CLK, MOSI, MISO plus for times CS: */
    NEW_LINE(access_port1, dir_port1, 1<<4),
    NEW_LINE(access_port1, dir_port1, 1<<5),
    NEW_LINE(access_port1, dir_port1, 1<<6),
    NEW_LINE(access_port1, dir_port1, 1<<7),
    NEW_LINE(access_port2, dir_port2, 1<<0),
    NEW_LINE(access_port2, dir_port2, 1<<1),
    NEW_LINE(access_port2, dir_port2, 1<<2),
    LINE_LIST_END
};

static struct cr_line port2_lines[] = {
    /* CLK, 3*ADDRESS, 8*DATA: */
    NEW_LINE(access_port4, dir_port4, 1<<0),
    NEW_LINE(access_port4, dir_port4, 1<<1),
    NEW_LINE(access_port4, dir_port4, 1<<2),
    NEW_LINE(access_port4, dir_port4, 1<<3),
    NEW_LINE(access_port4, dir_port4, 1<<4),
    NEW_LINE(access_port4, dir_port4, 1<<5),
    NEW_LINE(access_port4, dir_port4, 1<<6),
    NEW_LINE(access_port4, dir_port4, 1<<7),
    NEW_LINE(access_port5, dir_port5, 1<<0),
    NEW_LINE(access_port5, dir_port5, 1<<1),
    NEW_LINE(access_port5, dir_port5, 1<<2),
    NEW_LINE(access_port5, dir_port5, 1<<3),
    LINE_LIST_END
};

struct cr_port cr_ports[] = {
    NEW_PORT(7, port1_lines),
    NEW_PORT(12, port2_lines),
    PORT_LIST_END
};

#endif /* CR_MSP430F1481 */

#ifdef CR_STDOUT

#include "arch/stdout.h"

static struct cr_line port1_lines[] = {
    NEW_LINE(access_portA, dir_portA, 1<<0),
    NEW_LINE(access_portA, dir_portA, 1<<1),
    NEW_LINE(access_portA, dir_portA, 1<<2),
    NEW_LINE(access_portA, dir_portA, 1<<3),
    NEW_LINE(access_portA, dir_portA, 1<<4),
    NEW_LINE(access_portA, dir_portA, 1<<5),
    NEW_LINE(access_portA, dir_portA, 1<<6),
    LINE_LIST_END
};

static struct cr_line port2_lines[] = {
    NEW_LINE(access_portB, dir_portB, 1<<0),
    NEW_LINE(access_portB, dir_portB, 1<<1),
    NEW_LINE(access_portB, dir_portB, 1<<2),
    NEW_LINE(access_portB, dir_portB, 1<<3),
    NEW_LINE(access_portB, dir_portB, 1<<4),
    NEW_LINE(access_portB, dir_portB, 1<<5),
    NEW_LINE(access_portB, dir_portB, 1<<6),
    NEW_LINE(access_portB, dir_portB, 1<<7),
    NEW_LINE(access_portB, dir_portB, 1<<8),
    NEW_LINE(access_portB, dir_portB, 1<<9),
    NEW_LINE(access_portB, dir_portB, 1<<10),
    NEW_LINE(access_portB, dir_portB, 1<<11),
    LINE_LIST_END
};

struct cr_port cr_ports[] = {
    NEW_PORT(7, port1_lines),
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

#define P3_LED0 (0x40)
#define P3_LED1 (0x80)

    /* Disable watchdog */
    WDTCTL = WDTPW + WDTHOLD;

    /* P3.0-3 are connected to DIP switches: Inputs */
    BITMASK_CLEAR(P3DIR, 0x0f);

    /* P3.6-7 are connected to LEDs: Outputs */
    BITMASK_SET(P3DIR, 0xc0);

    /*
     * Clock initialisation
     *
     * Upon bootup, the controller sources the master clock from the internal
     * oscillator, that drifts pretty heavily with temperature. Since we'll be
     * using an UART block (one of the requirements of an asynchronous
     * interface is to have a stable clock source, from which to derive the
     * interfaces baudrate), it makes sense to use an external crystal
     * oscillator. We *could* just use it just for the auxiliary clock. But
     * then, if it's there, it makes sense to use it for everything.
     *
     * slau049f, p.4-12 describes how to switch the master clock to the extenal
     * crystal oscillator:
     *
     *   1. Turn the oscillator on.
     *   2. Clear the OFIFG flag in IFG1.
     *   3. Wait for *at* *least* 50 microseconds.
     *   4. Test OFIFG and repeat steps 2 and 3 until it remains cleared.
     *   5. Select LFXT1CLK in BCSCTL2.
     */

    /* Turn on LED0 while the oscillator is enabled: */
    BITMASK_SET(P3OUT, P3_LED0);
#if 0
    /*
     * The Status register is zero'd upon bootup. So the OSCOFF bit should be
     * cleared by default. Thus this shouldn't be needed. _BIC_SR() doesn't
     * sound too portable across compilers anyway.
     */
    _BIC_SR(OSCOFF); /* Turn oscillator on */
#endif
    BITMASK_SET(BCSCTL1, XT2OFF); /* Disable the XT2 input */
    do {
        unsigned char i;
        BITMASK_SET(BCSCTL1, XTS); /* Enable high-frequency mode */
        for (i = 0xff; i > 0; --i) /* Delay */
            asm(" nop");
    } while (IS_BITMASK_SET(IFG1, OFIFG));
    /* Make sure the MCLK divider is set to "1": */
    BITMASK_CLEAR(BCSCTL2, DIVM1 | DIVM0);
    /* Make sure the ACLK divider is set to "1": */
    BITMASK_CLEAR(BCSCTL1, DIVA1 | DIVA0);
    /* Select the crystal for MCLK: */
    BITMASK_SET(BCSCTL2, SELM1 | SELM0);
    BITMASK_CLEAR(P3OUT, P3_LED0);

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
