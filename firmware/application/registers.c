/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/* A bit of a generic name there zephyr, but alright. */
#include <version.h>

#include <ufw/register-table.h>
#include <ufw/meta.h>

#ifdef BIT_MASK
#undef BIT_MASK
#endif /* BIT_MASK */

#include "registers.h"

#define INFO(x)  (INFO_START  + (x))
#define INDEX(x) (INDEX_START + (x))
#define IFC(x)   (IFC_START   + (x))
#define FB(x)    (FB_START    + (x))

RegisterTable registers = {
    .area = (RegisterArea[]) {
        [REG_AREA_INFO]  = MEMORY_AREA_RO(INFO_START,  INFO_SIZE),
        [REG_AREA_INDEX] = MEMORY_AREA_RO(INDEX_START, INDEX_SIZE),
        [REG_AREA_IFC]   = MEMORY_AREA(   IFC_START,   IFC_SIZE),
        [REG_AREA_FB]    = MEMORY_AREA(   FB_START,    FB_SIZE),
        REGISTER_AREA_END
    },

    .entry = (RegisterEntry[]) {
        /* Service Discovery Block */
        REG_U16(R_TAB_SEMANTICS, INFO( 0x00ul),      FW_TABLE_SEMANTICS),
        REG_U32(R_INDEX_ADDRESS, INFO( 0x01ul),      FW_INDEX_ADDRESS),
        /* Version Information Block */
        REG_U16(R_FW_MAJOR,      INFO( 0x10ul),      FIRMWARE_VERSION_MAJOR),
        REG_U16(R_FW_MINOR,      INFO( 0x11ul),      FIRMWARE_VERSION_MINOR),
        REG_U16(R_FW_PATCH,      INFO( 0x12ul),      FIRMWARE_PATCHLEVEL),
        REG_U16(R_FW_GIT_INC,    INFO( 0x13ul),      0u),
        REG_U16(R_FW_GIT_LEVEL,  INFO( 0x14ul),      0u),
        REG_U16(R_FW_GIT_META,   INFO( 0x15ul),      0u),
        REG_U16(R_ZEPHYR_MAJOR,  INFO( 0x16ul),      KERNEL_VERSION_MAJOR),
        REG_U16(R_ZEPHYR_MINOR,  INFO( 0x17ul),      KERNEL_VERSION_MINOR),
        REG_U16(R_ZEPHYR_PATCH,  INFO( 0x18ul),      KERNEL_PATCHLEVEL),
        REG_U16(R_UFW_MAJOR,     INFO( 0x19ul),      UFW_LIBRARY_MAJOR),
        REG_U16(R_UFW_MINOR,     INFO( 0x1aul),      UFW_LIBRARY_MINOR),
        REG_U16(R_UFW_PATCH,     INFO( 0x1bul),      UFW_LIBRARY_PATCH),
        /* Interface Index */
        REG_U16(R_IDX_SIZE,      INDEX(0x00ul),      FW_INTERFACE_COUNT),
#ifdef CR_WITH_SPI_0
        REG_U32(R_IDX_SPI0,      INDEX(FW_IDX_SPI0), FW_IFC_SPI0_OFFSET),
#endif /* CR_WITH_SPI_0 */
#ifdef CR_WITH_SPI_1
        /* TODO: The offset for this needs to be generated too. */
        REG_U32(R_IDX_SPI1,      INDEX(FW_IDX_SPI1), FW_IFC_SPI1_OFFSET),
#endif /* CR_WITH_SPI_1 */
#ifdef CR_WITH_SPI_0
        IFC_SPI(0, FW_IFC_SPI0_OFFSET),
#endif /* CR_WITH_SPI_0 */
#ifdef CR_WITH_SPI_1
        IFC_SPI(1, FW_IFC_SPI1_OFFSET),
#endif /* CR_WITH_SPI_1 */
        REGISTER_ENTRY_END
    }
};
