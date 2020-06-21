/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-bye.c
 * @brief Implemenation of BYE command
 */

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>

/**
 * Implement BYE command
 *
 * The BYE command moves the protocol state from active to idle. It takes no
 * arguments.
 *
 * @param  cmd   Pointer to the command table entry for BYE
 * @param  arg   Pointer to list of arguments supplied to command
 * @param  argn  Number of arguments in arg list
 *
 * @return OK or MALFORMED
 * @sideeffects none
 */
enum cr_proto_state
cr_handle_bye(const struct cr_protocol *proto,
              UNUSED const struct cr_command *cmd,
              UNUSED const struct cr_value *arg,
              UNUSED unsigned int argn)
{
    proto->reply("Have a nice day.\n");
    return CR_PROTO_STATE_IDLE;
}