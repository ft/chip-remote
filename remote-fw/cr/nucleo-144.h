/*
 * Copyright (c) 2013-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_NUCLEO_144_H
#define INC_NUCLEO_144_H

#include "chip-remote.h"

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

void xcr_pre_top_level(void);
void xcr_post_bye(void);
void xcr_send_host(char *);
void xcr_wait(uint32_t);

#endif /* INC_NUCLEO_144_H */
