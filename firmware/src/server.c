/*
 * Copyright (c) 2025 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file server.c
 * @brief Single threaded tcp server implementation
 */

#include <zephyr/net/socket.h>

#include <errno.h>

#include <ufw/byte-buffer.h>
#include <ufw/hexdump.h>
#include <ufw/sx.h>

#include "server.h"

static int
crs_client_init(struct cr_tcp_client *client, cr_client_init cinit,
                const size_t n)
{
    byte_buffer_space(&client->rx, client->mem.rx, CR_CLIENT_BUFFER_SIZE);
    byte_buffer_space(&client->tx, client->mem.tx, CR_CLIENT_BUFFER_SIZE);

    if (cinit != NULL) {
        const int rc = cinit(client, n);
        if (rc < 0) {
            return rc;
        }
    }

    return 0;
}

int
crs_init(struct cr_tcp_server *srv, const uint16_t port,
         cr_client_process fnc, cr_client_init cinit)
{
    memset(srv, 0, sizeof(*srv));
    srv->addr.sin_family = AF_INET;
    srv->addr.sin_port = htons(port);
    srv->port = port;
    srv->process = fnc;
    for (size_t i = 0u; i < CR_MAX_CLIENTS; ++i) {
        const int rc = crs_client_init(&srv->client[i], cinit, i);
        if (rc < 0) {
            printk("Custom client initialisation failed at %zu: %s (%d)\n",
                   i, strerror(-rc), -rc);
            return rc;
        }
    }

    return 0;
}

int
crs_setup(struct cr_tcp_server *srv)
{
    srv->socket = zsock_socket(srv->addr.sin_family, SOCK_STREAM, IPPROTO_TCP);
    if (srv->socket < 0) {
        return -errno;
    }

    {
        int value = 1;
        int orc = zsock_setsockopt(srv->socket, SOL_SOCKET,
                                   SO_REUSEADDR, &value, sizeof (value));
        if (orc < 0) {
            printk("setsockopt: %s (%d)\n", strerror(-errno), -errno);
        }
    }

    const struct sockaddr *sa = (struct sockaddr*)&srv->addr;
    const int brc = zsock_bind(srv->socket, sa, sizeof(srv->addr));
    if (brc < 0) {
        return -errno;
    }

    const int lrc = zsock_listen(srv->socket, CR_MAX_CLIENTS);
    if (lrc < 0) {
        return -errno;
    }

    return 0;
}

static int
crs_process_client(struct cr_tcp_server *srv, struct cr_tcp_client *client)
{
    unsigned char buf[64];
    /* Implement a Source and Sink instance for zsock_recv() and
     * zsock_send()! This is temporary. */
    ssize_t rc = zsock_recv(client->socket, buf, 64, 0);
    if (rc <= 0) {
        if (errno == EAGAIN || errno == EINTR) {
            return 0;
        } else {
            return -EIO;
        }
    }

    byte_buffer_add(&client->rx, buf, rc);
    const size_t rest = byte_buffer_rest(&client->rx);
    return (rest == 0) ? 0 : srv->process(srv, client);
}

static int
count_clients(struct cr_tcp_server *srv)
{
    int rv = 0;
    for (size_t i = 0u; i < CR_MAX_CLIENTS; ++i) {
        if (srv->client[i].flags & CR_CLIENT_CONNECTED) {
            rv++;
        }
    }
    return rv;
}

static int
crs_accept_new_client(struct cr_tcp_server *srv)
{
    printk("New client!\n");
    struct sockaddr *sa = (struct sockaddr*)&srv->addr;
    socklen_t len = sizeof(srv->addr);
    const int arc = zsock_accept(srv->socket, sa, &len);
    if (arc < 0) {
        return -errno;
    }

    for (size_t i = 0u; i < CR_MAX_CLIENTS; ++i) {
        if (srv->client[i].flags & CR_CLIENT_CONNECTED) {
            continue;
        }
        srv->client[i].socket = arc;
        srv->client[i].flags |= CR_CLIENT_CONNECTED;
        srv->clients = count_clients(srv);
        byte_buffer_reset(&srv->client[i].rx);
        byte_buffer_reset(&srv->client[i].tx);
        return 1;
    }

    printk("Too many clients!\n");
    zsock_shutdown(arc, ZSOCK_SHUT_RDWR);
    zsock_close(arc);
    return 0;
}

static void
crs_disconnect_client(struct cr_tcp_server *srv, struct cr_tcp_client *client)
{
    for (size_t i = 0u; i < CR_MAX_CLIENTS; ++i) {
        if (srv->client[i].socket == client->socket) {
            srv->client[i].flags &= ~(CR_CLIENT_CONNECTED);
            break;
        }
    }
    zsock_shutdown(client->socket, ZSOCK_SHUT_RDWR);
    zsock_close(client->socket);
}

static inline void
update_fds(struct cr_tcp_server *srv, struct zsock_pollfd *fds)
{
    size_t j = 0u;
    for (size_t i = 0u; i < CR_MAX_CLIENTS; ++i) {
        if (srv->client[i].flags & CR_CLIENT_CONNECTED) {
            fds[j].fd = srv->client[i].socket;
            fds[j].events = ZSOCK_POLLIN;
            j++;
        }
    }
}

static inline void
crs_process(struct cr_tcp_server *srv, struct zsock_pollfd *fds)
{
    const int nfds = srv->clients + 1;
    const int prc = zsock_poll(fds, nfds, CR_POLL_TIMEOUT);
    if (prc <= 0) {
        return;
    }
    if (fds[0].revents & ZSOCK_POLLIN) {
        /* New client, accept connection */
        if (crs_accept_new_client(srv) > 0) {
            update_fds(srv, fds + 1);
            return;
        }
    }
    for (size_t i = 0u; i < srv->clients; ++i) {
        if (fds[i + 1u].revents & ZSOCK_POLLIN) {
            const int procrc = crs_process_client(srv, &srv->client[i]);
            if (procrc < 0) {
                /* The processor returns errors that cannot be recovered from.
                 * When this happens, the only recourse is to disconnect the
                 * client entirely. */
                printk("Disconnecting client %d (%s, %d)...\n",
                       srv->client[i].socket, strerror(-procrc), -procrc);
                crs_disconnect_client(srv, &srv->client[i]);
                update_fds(srv, fds + 1);
            }
            size_t output = byte_buffer_rest(&srv->client[i].tx);
            while (output > 0) {
                const ssize_t src = zsock_send(srv->client[i].socket,
                                               byte_buffer_readptr(&srv->client[i].tx),
                                               output, 0);
                if (src < 0) {
                    switch (errno) {
                    case EAGAIN:
                    case EINTR:
                        continue;
                    default:
                        printk("send error: Disconnecting client %d (%s, %d)...\n",
                               srv->client[i].socket, strerror(errno), errno);
                        crs_disconnect_client(srv, &srv->client[i]);
                        update_fds(srv, fds + 1);
                        return;
                    }
                }
                byte_buffer_markread(&srv->client[i].tx, src);
                output = byte_buffer_rest(&srv->client[i].tx);
            }
            byte_buffer_rewind(&srv->client[i].tx);
        }
    }
}

void
crs_run(struct cr_tcp_server *srv)
{
    struct zsock_pollfd fds[CR_MAX_CLIENTS + 1];
    fds[0].fd = srv->socket;
    fds[0].events = ZSOCK_POLLIN;

    for (;;) {
        crs_process(srv, fds);
    }
}
