#ifndef INC_PROTOCOL_H
#define INC_PROTOCOL_H

#include <stdint.h>
#include <stdlib.h>

#include "port.h"

#define CR_MAX_WORDS 8
#define CR_MAX_LINE 127
#define CR_INT_MAX_LEN 8

#define BYE_REPLY "Have a nice day."
#define BROKEN_VALUE_REPLY "BROKEN-VALUE"
#define DONE_REPLY "DONE"
#define FOCUS_REPLY "FOCUS"
#define HI_REPLY "Hi there, stranger."
#define LINE_REPLY "LINE"
#define OK_REPLY "OK"
#define PORTS_REPLY "PORTS"
#define VALUE_OUT_OF_RANGE_REPLY "VALUE-OUT-OF-RANGE"
#define VERSION_REPLY "VERSION 2 0 0"
#define WTF_REPLY "WTF"

enum cr_requests {
    REQUEST_HI = 0,
    REQUEST_BYE,
    REQUEST_FEATURES,
    REQUEST_FOCUS,
    REQUEST_LINES,
    REQUEST_MODES,
    REQUEST_PORTS,
    REQUEST_VERSION,
    MAX_REQUEST
};

struct cr_args {
    unsigned int min;
    /** If -1, there is no maximum */
    int max;
};

struct cr_word {
    char *start;
    size_t length;
};

struct cr_words {
    struct cr_word word[CR_MAX_WORDS];
    size_t count;
};

void cr_broken_value(char *, size_t);
void cr_echo_focus(int);
void cr_echo_line(size_t, size_t, enum cr_pin_role, int, enum cr_value_type);
void cr_echo_ports(size_t);
void cr_fail(const char *);
void cr_split_request(char *, struct cr_words *);
void cr_uint_oor(uint32_t);
int cr_word_eq(struct cr_words *, size_t, const char *);

extern char rxbuf[CR_MAX_LINE + 1];

#endif /* INC_PROTOCOL_H */
