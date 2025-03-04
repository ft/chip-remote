/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_CHIP_REMOTE_H_54e96e01
#define INC_CHIP_REMOTE_H_54e96e01

#include <ufw/endpoints.h>
#include <ufw/register-protocol.h>

#include "server.h"

int chip_remote_init(RegP *protocol,
                     Source source, Sink sink,
                     RegisterTable *registers);
int chip_remote_process(RegP *protocol);
int chip_remote_tcp_boot(struct cr_tcp_server *srv);

#endif /* INC_CHIP_REMOTE_H_54e96e01 */
