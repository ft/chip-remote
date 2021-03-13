/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file parse-string.h
 * @brief API for chip-remote protocol parser
 */

#ifndef INC_PARSE_STRING_H
#define INC_PARSE_STRING_H

#include <chip-remote.h>

enum cr_proto_result cr_parse_string(
    string_sink, char*, struct cr_proto_parse*);

#endif /* INC_PARSE_STRING_H */
