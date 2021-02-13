/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file commands.c
 * @brief API for protocol command table
 */

#ifndef INC_COMMANDS_H
#define INC_COMMANDS_H

#include <chip-remote.h>

struct cr_command* cr_lookup_command(const char*);

#endif /* INC_COMMANDS_H */
