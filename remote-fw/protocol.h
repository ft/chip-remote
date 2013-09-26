#ifndef INC_PROTOCOL_H
#define INC_PROTOCOL_H

#include <stdint.h>
#include <stdlib.h>

#include "chip-remote.h"

#define BYE_REPLY "Have a nice day."
#define BROKEN_VALUE_REPLY "BROKEN-VALUE"
#define DONE_REPLY "DONE"
#define FOCUS_REPLY "FOCUS"
#define HI_REPLY "Hi there, stranger."
#define LINE_REPLY "LINE"
#define LINES_REPLY "LINES"
#define MODE_REPLY "MODE"
#define OK_REPLY "OK"
#define PORTS_REPLY "PORTS"
#define RATE_REPLY "RATE"
#define VALUE_OUT_OF_RANGE_REPLY "VALUE-OUT-OF-RANGE"
#define VERSION_REPLY "VERSION 2 0 0"
#define WTF_REPLY "WTF"

void cr_broken_value(char *, size_t);
void cr_echo_focus(int);
void cr_echo_line(size_t, size_t, enum cr_pin_role, int, int);
void cr_echo_lines(struct cr_port *, size_t);
void cr_echo_mode(struct cr_port *, size_t);
void cr_echo_ports(size_t);
void cr_echo_rate(struct cr_port *, size_t);
void cr_fail(char *);
void cr_uint_oor(uint32_t);

extern char rxbuf[CR_MAX_LINE + 1];

#endif /* INC_PROTOCOL_H */
