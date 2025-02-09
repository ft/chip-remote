/*
 * Copyright (c) 2025 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_FIRMWARE_SRC_SERVER_H_409b2063
#define INC_FIRMWARE_SRC_SERVER_H_409b2063

#include <zephyr/net/socket.h>

#include <stddef.h>

#include <ufw/byte-buffer.h>

#define CR_MAX_CLIENTS           5u
#define CR_POLL_TIMEOUT        500u
#define CR_CLIENT_BUFFER_SIZE 1024u

#define CR_CLIENT_CONNECTED 1u

struct cr_tcp_client {
    unsigned int flags;
    int socket;
    ByteBuffer rx;
    ByteBuffer tx;
    struct {
        unsigned char rx[CR_CLIENT_BUFFER_SIZE];
        unsigned char tx[CR_CLIENT_BUFFER_SIZE];
    } mem;
    void *data;
};

struct cr_tcp_server;

typedef int (*cr_client_process)(struct cr_tcp_server*,
                                 struct cr_tcp_client*);

typedef int (*cr_client_init)(struct cr_tcp_client*, size_t);

struct cr_tcp_server {
    int socket;
    int clients;
    int port;
    struct sockaddr_in addr;
    struct cr_tcp_client client[CR_MAX_CLIENTS];
    cr_client_process process;
    cr_client_init cinit;
};

int crs_init(struct cr_tcp_server *srv, uint16_t port,
             cr_client_process fnc, cr_client_init cinit);
int crs_setup(struct cr_tcp_server *srv);
void crs_run(struct cr_tcp_server *srv);

#endif /* INC_FIRMWARE_SRC_SERVER_H_409b2063 */
