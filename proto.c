/**
 * @file proto.c
 * @brief Serial protocol between host and serial device
 */

#include <string.h>

#include "common.h"
#include "proto.h"
#include "serial.h"

int
proto_expect_ok(void)
{
    return proto_expect_reply("OK");
}

int
proto_expect_reply(char *expected)
{
    int rc;
    char got[SERIAL_BUF_MAX + 1];

    rc = serial_read(got);
    if (rc <= 0)
        return 0;
    if (!strcmp(expected, got))
        return 1;
    return 0;
}

int
proto_hi(void)
{
    return serial_write("HI");
}


int
proto_bye(void)
{
    return serial_write("BYE");
}
