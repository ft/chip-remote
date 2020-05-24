/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-hi.h
 * @brief API of HI command
 */

#ifndef INC_COMMAND_HI_H
#define INC_COMMAND_HI_H

#include <chip-remote.h>

enum cr_proto_result
cr_handle_hi(const struct cr_command*, const struct cr_value*, unsigned int);

#endif /* INC_COMMAND_HI_H */
