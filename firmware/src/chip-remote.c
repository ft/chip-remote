/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <zephyr/kernel.h>

#include <stdbool.h>

#include <ufw/endpoints.h>
#include <ufw/register-protocol.h>
#include <ufw/register-table.h>
#include <ufw/register-utilities.h>
#include <ufwz/slab-allocator.h>

#include "peripherals.h"

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
    if (recvrc < 0 && recvrc != -EAGAIN) {
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
            printk("# Error in regp_process(): %d\n", procrc);
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
