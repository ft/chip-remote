/*
 * Copyright 2011 Frank Terbeck <ft@bewatermyfriend.org>, All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * AUTHOR OR CONTRIBUTORS OF THE PROJECT BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * @file chip-remote.c
 * @brief Remote-control for configurable chips
 *
 * This program communicates with a serial device which reads and writes data
 * according to a certain protocol. This protocol works as follows:
 *
 * The serial device *never* just sends data on its own. It always waits for
 * input from this program. This input forms the command set of the
 * protocol. For every command, the device will reply something; even if it's
 * just the string "OK". If a reply takes forever to occur, the program will
 * timeout after a while. While the program waits for a reply from the serial
 * device, it will not react to any input.
 *
 * Each reply is terminated by an ASCII linefeed (0x0a) character. If a reply
 * is made up of multiple records, these records are separated by ASCII colons
 * (0x3a).
 *
 * All numeric values in replies are in hexadecimal format *WITHOUT* a leading
 * `0x'.
 *
 * A command is always a string plus an ASCII space (0x20) separated list of
 * arguments, terminated of an ASCII linefeed (0x0a).
 *
 *
 * The program reads user input via STDIN and writes normal output (including
 * warnings and error messages) to STDOUT. Trace output (the raw data from and
 * to the serial device) is written to STDERR, if an appropriate option is set
 * accordingly.
 *
 * The conversation with a serial device is always initiated by a `HI' command
 * and should always be terminated by a `BYE' command.
 *
 * An example session could look like this:
 *
 * @code
 *     << HI
 *     >> Hi there, stranger.
 *     << VERSION
 *     >> v0.1
 *     << READ 0
 *     >> 683c0ed0
 *     << WRITE 0 ea3c0ed0
 *     >> OK
 *     << BYE
 *     >> Have a nice day.
 * @endcode
 *
 * Normally, you don't write raw commands to the device but use the appropriate
 * scheme API. Doing that, you can virtually script the device as much as you'd
 * like. Here's a minimal example:
 *
 * @code
 *     #!/usr/local/bin/chip-remote -s
 *     !#
 *     (cr/open "/dev/ttyS0")
 *     (cr/hi)
 *     (define REMOTE_VERSION (cr/get-version))
 *     (cr/bye)
 *     (format #t "Remote version: ~a\n" REMOTE_VERSION)
 *     (newline)
 * @endcode
 *
 * ...which would output "Remote version: v0.1", given the raw communication as
 * illustrated above.
 */

#include <stdio.h>
#include <stdlib.h>

#include <libguile.h>

#include "chip-remote-scm.h"
#include "common.h"

static void real_main(void *, int, char **);
int main(int, char **);

static void
real_main(UNUSED void *data, int argc, char **argv)
{
    cr_scm_init();
    scm_shell(argc, argv);
}

int
main(int argc, char **argv)
{
    scm_boot_guile(argc, argv, real_main, NULL);
    return EXIT_SUCCESS; /* Never reached */
}
