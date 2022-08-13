/*
 * Copyright (c) 2020-2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-bye.c
 * @brief Implemenation of BYE command
 */

#include <ufw/compiler.h>

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
void
cr_handle_bye(struct cr_protocol *proto, UNUSED struct cr_command *cmd,
              UNUSED struct cr_value *t, UNUSED unsigned int n)
{
    proto->reply("Have a nice day.\n");
}
