/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-hi.c
 * @brief Implemenation of HI command
 */

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>

/**
 * Implement HI command
 *
 * The HI command moves the protocol state from idle to active. It takes no
 * arguments.
 *
 * @param  cmd   Pointer to the command table entry for HI
 * @param  arg   Pointer to list of arguments supplied to command
 * @param  argn  Number of arguments in arg list
 *
 * @return OK or MALFORMED
 * @sideeffects none
 */
struct cr_command_result
cr_handle_hi(const struct cr_protocol *proto,
             UNUSED const struct cr_command *cmd,
             UNUSED const struct cr_value *arg,
             unsigned int argn)
{
    struct cr_command_result rv = {
        .result = (argn == 0u) ? CR_PROTO_RESULT_OK : CR_PROTO_RESULT_MALFORMED,
        .next_state = CR_PROTO_STATE_ACTIVE };
    proto->reply("Hi there, stranger!\n");
    return rv;
}
