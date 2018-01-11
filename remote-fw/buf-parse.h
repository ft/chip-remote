/*
 * Copyright (c) 2013-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_BUF_PARSE_H
#define INC_BUF_PARSE_H

void cr_split_request(char *, struct cr_words *);
int cr_word_eq(struct cr_words *, size_t, const char *);

#endif /* INC_BUF_PARSE_H */
