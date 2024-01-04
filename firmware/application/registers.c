/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <ufw/register-table.h>

#include "registers.h"

#define INFO(x)   (INFO_START   + (x))
#define STATUS(x) (STATUS_START + (x))
#define MEMORY(x) (MEMORY_START + (x))

RegisterTable registers = {
    .area = (RegisterArea[]) {
        [REG_AREA_INFO]   = MEMORY_AREA_RO(INFO_START,   INFO_SIZE),
        [REG_AREA_STATUS] = MEMORY_AREA_RO(STATUS_START, STATUS_SIZE),
        [REG_AREA_MEMORY] = MEMORY_AREA(   MEMORY_START, MEMORY_SIZE),
        REGISTER_AREA_END
    },

    .entry = (RegisterEntry[]) {
        REG_U16(R_FW_MAJOR, INFO(  0x00ul),   FIRMWARE_VERSION_MAJOR),
        REG_U16(R_FW_MINOR, INFO(  0x01ul),   FIRMWARE_VERSION_MINOR),
        REG_U16(R_FW_PATCH, INFO(  0x02ul),   FIRMWARE_PATCHLEVEL),
        REG_U16(R_STATUS,   STATUS(0x00ul),   0u),
        REGISTER_ENTRY_END
    }
};
