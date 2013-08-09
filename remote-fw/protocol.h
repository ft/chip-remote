#ifndef INC_PROTOCOL_H
#define INC_PROTOCOL_H

#include <stdlib.h>

#define CR_MAX_WORDS 8
#define CR_MAX_LINE 127

#define BYE_REPLY "Have a nice day."
#define DONE_REPLY "DONE"
#define HI_REPLY "Hi there, stranger."
#define OK_REPLY "OK"
#define VERSION_REPLY "VERSION 2 0 0"
#define WTF_REPLY "WTF"

enum cr_requests {
    REQUEST_HI = 0,
    REQUEST_BYE,
    REQUEST_FEATURES,
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

void cr_fail(const char *);
void cr_split_request(char *, struct cr_words *);
int cr_word_eq(struct cr_words *, size_t, const char *);

extern char rxbuf[CR_MAX_LINE + 1];

#endif /* INC_PROTOCOL_H */