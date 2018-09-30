/*
 * Copyright (c) 2013-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_CR_INTERFACE_H
#define INC_CR_INTERFACE_H

#include <chip-remote.h>

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

#endif /* INC_CR_INTERFACE_H */
