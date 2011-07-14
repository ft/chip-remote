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
 * @file common.h
 * @brief Common definitions
 */

#ifndef INC_COMMON_H_
#define INC_COMMON_H_

/** Macro to mark unused parameters. */
#define UNUSED __attribute__((unused))

/** How long before timing out, when reading from serial? */
#define SERIAL_TIMEOUT 2

/**
 * Maximum length of one transmission in the serial protocol.
 *
 * The longest reply I can think of right now, is the one to "read all regs",
 * which would require (32/4)*13+12 = 116 bytes. All other replies should be
 * way shorter. So, the following should suffice easily.
 */
#define SERIAL_BUF_MAX 127

/** An additional `%load-path' entry for our modules. */
#define CDCE_REMOTE_LOAD_PATH "/usr/local/share/cdce-remote"

#endif /* INC_COMMON_H_ */
