/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <zephyr/kernel.h>

#include <stdbool.h>
#include <string.h>

#include <ufw/compat/errno.h>
#include <ufw/endpoints.h>
#include <ufw/hexdump.h>
#include <ufw/register-protocol.h>
#include <ufw/register-table.h>
#include <ufw/register-utilities.h>
#include <ufwz/slab-allocator.h>

#include "peripherals.h"
#include "registers.h"
#include "server.h"

#define PROTO_SLAB_SLOTS 4u
#define PROTO_SLAB_SIZE  (128u + sizeof(RPFrame))

K_MEM_SLAB_DEFINE_STATIC(proto_slab, PROTO_SLAB_SIZE, PROTO_SLAB_SLOTS, 4);
BlockAllocator palloc = UFWZ_SLAB_BLOCKALLOC(&proto_slab, PROTO_SLAB_SIZE);

static bool
cmd_was_used(const RegisterTable *t, const RegisterHandle h, const RPFrame *f)
{
    const RegisterEntry *e = register_get_entry(t, h);
    const size_t size = register_entry_size(e);
    return (f->header.address == e->address && f->header.blocksize == size);
}

int
chip_remote_process(RegP *protocol)
{
    bool normal_processing = true;
    RPMaybeFrame mf;
    const int recvrc = regp_recv(protocol, &mf);
    if (recvrc < 0 && recvrc != -EAGAIN && recvrc != -ENODATA) {
        printk("# Error in regp_recv(): %d\n", recvrc);
    }

    /*
     * This special behaviour is implemented before processing happens. As
     * such, the command value was not committed to the register table yet
     * and therefore we cannot use things like register_was_touched() or
     * register_get() to use its value. We must look into the new frame
     * ourselves.
     *
     * We are requiring the remote side to write the all data it needs to
     * before doing a single write to exactly and only the command regi-
     * ster. Therefore the rest of the processing can indeed be fetched
     * from the register table.
     *
     * This has the additional guarantee (beyond ufw-regp), that any side
     * effects caused by the command processing will have been carried out
     * before the remote side receives the acknowledging response for the
     * command write request.
     */
    if (mf.error.id == 0 && regp_is_write_request(mf.frame)) {
        struct peripheral_control **pc = periph_ctrl;
        for (size_t i = 0u; pc[i] != NULL; ++i) {
            if (cmd_was_used(&registers, pc[i]->cmd, mf.frame)) {
                process_command(&registers, pc[i], mf.frame);
                normal_processing = false;
            }
        }
    }

    if (normal_processing) {
        const int procrc = regp_process(protocol, &mf);
        if (procrc < 0) {
            printk("# Error in regp_process(): %d (%s)\n",
                   procrc, strerror(-procrc));
        }
    } else {
        /* Writes to command registers are not processed regularly. We're
         * just acknowledging them and move on. This means, that these
         * registers will, for the reader stay zero at all times. */
        regp_resp_ack(protocol, mf.frame, NULL, 0);
    }
    regp_free(protocol, mf.frame);

    return recvrc;
}

int
chip_remote_init(RegP *protocol,
                 Source source, Sink sink,
                 RegisterTable *registers)
{
    register_make_bigendian(registers, true);
    const RegisterInit regtabrc = register_init(registers);

    if (regtabrc.code != REG_INIT_SUCCESS) {
        register_table_print(stdout, "# ", registers);
        printf("#\n");
        register_init_print(stdout, "# ", regtabrc);
        return -EINVAL;
    }

    regp_init(protocol);
    regp_use_allocator(protocol, &palloc);
    regp_use_memory16(protocol, regread, regwrite);
    regp_use_channel(protocol, RP_EP_SERIAL, source, sink);

    return 0;
}

/*
 * TCP Server Setup
 */

struct cr_multi {
    RegP regp;
};

static struct cr_multi client_data[CR_MAX_CLIENTS];

static int
process_init(struct cr_tcp_client *client, const size_t n)
{
    if (n >= CR_MAX_CLIENTS) {
        return -EINVAL;
    }

    Source src;
    Sink snk;
    struct cr_multi *cd = client_data + n;

    byte_buffer_reset(&client->rx);
    byte_buffer_reset(&client->tx);

    source_from_buffer(&src, &client->rx);
    sink_to_buffer(&snk, &client->tx);

    regp_init(&cd->regp);
    regp_use_allocator(&cd->regp, &palloc);
    regp_use_memory16(&cd->regp, regread, regwrite);
    regp_use_channel(&cd->regp, RP_EP_TCP, src, snk);

    client->data = cd;

    return 0;
}

static int
process(struct cr_tcp_server *srv,
        struct cr_tcp_client *client)
{
#if 0
    printk("cr: Processing %zu bytes of data...\n",
           byte_buffer_rest(&client->rx));
#endif

    hexdump_stdout(byte_buffer_readptr(&client->rx),
                   byte_buffer_rest(&client->rx), 0);

    struct cr_multi *cd = client->data;
    size_t rest = 0;
    do {
        ByteBufferPos pos;
        byte_buffer_getpos(&client->rx, &pos);
        const int rc = chip_remote_process(&cd->regp);
        if (rc == -ENODATA) {
#if 0
            printk("Not enough data, resetting position.\n");
#endif
            byte_buffer_setpos(&client->rx, &pos);
            break;
        } else if (rc < 0) {
            process_init(client, 0);
            return rc;
        } else {
            byte_buffer_rewind(&client->rx);
        }
        rest = byte_buffer_rest(&client->rx);
    } while (rest > 0);

    return 0;
}

int
chip_remote_tcp_boot(struct cr_tcp_server *srv)
{
    register_make_bigendian(&registers, true);
    const RegisterInit regtabrc = register_init(&registers);

    if (regtabrc.code != REG_INIT_SUCCESS) {
        register_table_print(stdout, "# ", &registers);
        printf("#\n");
        register_init_print(stdout, "# ", regtabrc);
        return -EINVAL;
    }

    const int irc = crs_init(srv, 1234, process, process_init);
    if (irc < 0) {
        printk("Initialising TCP server failed: %s (%d)\n",
               strerror(-irc), -irc);
        return irc;
    }

    const int src = crs_setup(srv);
    if (src < 0) {
        printk("Setting up TCP server failed: %s (%d)\n",
               strerror(-src), -src);
        return src;
    }

    return 0;
}
