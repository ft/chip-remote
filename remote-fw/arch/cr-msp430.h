/*
 * Copyright (c) 2013-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_CR_MSP430F1481_H
#define INC_CR_MSP430F1481_H

#include "../config.msp430.h"

/*
 * Within the adu1e9 project, ports 1,2,4,5 and 6 are connected to configurable
 * and controllable devices. Port 3 is used for status bits like LEDs and
 * switches.
 */

int access_port1(struct cr_line *, enum cr_access_mode, int);
void dir_port1(struct cr_line *, enum cr_access_mode);
int access_port2(struct cr_line *, enum cr_access_mode, int);
void dir_port2(struct cr_line *, enum cr_access_mode);
int access_port3(struct cr_line *, enum cr_access_mode, int);
void dir_port3(struct cr_line *, enum cr_access_mode);
int access_port4(struct cr_line *, enum cr_access_mode, int);
void dir_port4(struct cr_line *, enum cr_access_mode);
int access_port5(struct cr_line *, enum cr_access_mode, int);
void dir_port5(struct cr_line *, enum cr_access_mode);
int access_port6(struct cr_line *, enum cr_access_mode, int);
void dir_port6(struct cr_line *, enum cr_access_mode);

#ifdef MSPGCC_BUILD

/*
 * This defines XINTERRUPT and INTERRUPT_PROTOTYPE for use with mspgcc.
 */

#define XINTERRUPT(VECTOR, SYMBOL)                              \
    /* prototype to shut up the compiler */                     \
    void __attribute__((__interrupt__(VECTOR))) SYMBOL(void);   \
    void __attribute__((__interrupt__(VECTOR))) SYMBOL(void)

#define INTERRUPT_PROTOTYPE(VECTOR, SYMBOL)                     \
    void __attribute__((__interrupt__(VECTOR)))  SYMBOL(void);

#else /* MSPGCC_BUILD */

/*
 * This defines XINTERRUPT and INTERRUPT_PROTOTYPE for use with Texas
 * Intrument's C compiler from its Code Composer Studio platform.
 *
 * This also needs this before:
 *
 *         #pragma vector=VECTOR
 */

#define XINTERRUPT(VECTOR, SYMBOL) \
    __interrupt void SYMBOL(void)

#define INTERRUPT_PROTOTYPE(VECTOR, SYMBOL)     \
    __interrupt void SYMBOL(void);

#endif /* !MSPGCC_BUILD */

#endif /* INC_CR_MSP430F1481_H */
