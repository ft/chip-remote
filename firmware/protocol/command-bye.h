/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-bye.h
 * @brief API of BYE command
 */

#ifndef INC_COMMAND_BYE_H
#define INC_COMMAND_BYE_H

#include <chip-remote.h>

enum cr_proto_result
cr_handle_bye(const struct cr_command*, const struct cr_value*, unsigned int);

#endif /* INC_COMMAND_BYE_H */
