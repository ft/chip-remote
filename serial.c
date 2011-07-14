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
 * @file serial.c
 * @brief Low-level serial communication code
 */

#include <sys/select.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "common.h"
#include "serial.h"
#include "scm-helpers.h"

static int cdce_serial_non_open(void);
static int really_read(int, char *);

/** Global file descriptor of the serial device opened by `cdce/open'. */
static int cdce_serial_fd = -1;

/**
 * Perform sane reads from a serial device.
 *
 * This also performs input sanitisation. All input bytes *must* be regular
 * 7-bit ascii. A response delimiter (0x0a) must be found before
 * `#SERIAL_BUF_MAX+1' bytes are read.
 *
 * Broken replies will be echoed to `STDOUT'.
 *
 * @param fd       The file descriptor of the serial device to write to.
 * @param buf      A pointer to a buffer to copy the reply to.
 */
static int
really_read(int fd, char *buf)
{
    int err, i, cnt;
    char c;

    err = i = 0;
    while ((cnt = read(fd, (void *)&c, sizeof(char))) >= 0) {
        if (cnt == 0) {
            (void)printf("read(): Got EOF from device. Giving up.\n");
            return -1;
        }
        if (c == '\n') {
            buf[i] = '\0';
            break;
        }
        if (err || (i > SERIAL_BUF_MAX) || !isascii(c)) {
            if (!err) {
                (void)printf("read(): Broken reply: %s", buf);
                err = 1;
            } else
                (void)printf("%c", c);
        } else {
            buf[i++] = c;
        }
    }
    if (err) {
        (void)printf("\n");
        return 0;
    } else {
        if (cdce_scm_bool_var("cdce/options:trace"))
            (void)fprintf(stderr, " >> %s\n", buf);
        scm_c_define("cdce/last-reply", scm_from_locale_string(buf));
    }

    return 1;
}

/**
 * Check if `#cdce_serial_fd' has a sensible value.
 *
 * That is, it's different from its initial value (which is a negative
 * integer).
 *
 * @return `1' in case of a problem; `0' otherwise.
 */
static int
cdce_serial_non_open(void)
{
    if (cdce_serial_fd < 0) {
        (void)printf("Serial device not opened. Forgot `cdce/open'?\n");
        return 1;
    }
    return 0;
}

/**
 * Read a response from the serial device behind `#cdce_serial_fd'.
 *
 * Timeout if a response takes too long. The input sanitisation is delegated to
 * `#really_read()'.
 *
 * @param buf      Pointer to a character buffer to copy the response to.
 *
 * @return `-2' in case the device is not open yet. `-1' in case of an
 *         error. Any other integer value otherwise (`0' indicates `read()' saw
 *         `end-of-file').
 */
int
serial_read(char *buf)
{
    int rc;
    fd_set fds;
    struct timeval t;

    if (cdce_serial_non_open())
        return -2;

    FD_ZERO(&fds);
    FD_SET(cdce_serial_fd, &fds);
    t.tv_sec = SERIAL_TIMEOUT;
    t.tv_usec = 0;

    rc = select(cdce_serial_fd + 1, &fds, NULL, NULL, &t);
    if (rc == -1)
        (void)printf("select(): %s\n", strerror(errno));
    else if (rc == 0)
        (void)printf("Reading from serial device timed out (%d seconds).\n",
                     SERIAL_TIMEOUT);
    else
        rc = really_read(cdce_serial_fd, buf);

    return rc;
}

/**
 * Write a buffer to `#cdce_serial_fd' entirely.
 *
 * Only give up if an error occurs. Also, throttle the transmission if it takes
 * more than one `write()' to bang the whole buffer out to the serial device.
 *
 * Bails out if `buf' exceeds the maximum length (`#SERIAL_BUF_MAX').
 *
 * @param buf      The character buffer to write to the opened device.
 *
 * @return `0' in case of any error; `1' otherwise.
 */
int
serial_write(char *buf)
{
    ssize_t sofar, len, rc;

    if (cdce_serial_non_open())
        return 0;

    len = strlen(buf);
    if (len > SERIAL_BUF_MAX) {
        (void)printf("serial_write(): Input length exceeds limits (%d > %d).\n",
                     len, SERIAL_BUF_MAX);
        (void)printf("serial_write(): was: \"%s\"\n", buf);
        return 0;
    }
    rc = sofar = 0;
    while (sofar >= 0 && sofar != len) {
        if (sofar > 0)
            /*
             * Throttle if it takes multiple writes to get the buffer out.
             * This should probably be shorter. `nanosleep()'?
             */
            sleep(1);
        rc = write(cdce_serial_fd, buf + sofar, len - sofar);
        if (rc >= 0)
            sofar += rc;
        else
            sofar = rc;
    }
    if (sofar >= 0)
        sofar = write(cdce_serial_fd, "\n", 1);
    if (sofar < 0) {
        (void)printf("write(): %s\n", strerror(errno));
        return 0;
    } else if (cdce_scm_bool_var("cdce/options:trace"))
        (void)fprintf(stderr, " << %s\n", buf);

    return 1;
}

int
serial_open(char *dev)
{
    int rc;

    if (cdce_serial_fd >= 0) {
        (void)printf("serial_open(): Serial device already opened.\n");
        return 0;
    }

    rc = open(dev, O_RDWR);
    if (rc < 0) {
        (void)printf("open(): %s: %s\n", dev, strerror(errno));
        return 0;
    }
    cdce_serial_fd = rc;
    return 1;
}

int
serial_close(void)
{
    int rc;

    if (cdce_serial_non_open())
        return 0;

    rc = close(cdce_serial_fd);
    cdce_serial_fd = -1;
    if (rc < 0) {
        (void)printf("close(): %s\n", strerror(errno));
        return 0;
    }
    return 1;
}
