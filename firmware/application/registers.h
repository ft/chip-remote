/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_REGISTERS_H_6a5bd7c3
#define INC_REGISTERS_H_6a5bd7c3

#include <ufw/register-protocol.h>
#include <ufw/register-table.h>

#include "interfaces.h"

extern RegisterTable registers;

/* General Definitions */

#define FW_TABLE_SEMANTICS 0u
#define FW_INDEX_ADDRESS   0x1000u

#define FIRMWARE_VERSION_MAJOR 0u
#define FIRMWARE_VERSION_MINOR 0u
#define FIRMWARE_PATCHLEVEL    1u

/* Table Layout */

#define INFO_START          0x0000ul
#define INFO_SIZE             0x20ul
#define INDEX_START         0x1000ul
#define INDEX_SIZE            0x10ul
#define IFC_START           0x8000ul
#define IFC_SIZE             0x100ul
#define FBTX_START R_DEFAULT_FBTX_ADDR
#define FBTX_SIZE  R_DEFAULT_FBRX_SIZE
#define FBRX_START R_DEFAULT_FBRX_ADDR
#define FBRX_SIZE  R_DEFAULT_FBRX_SIZE

/* Area Names */

typedef enum FirmwareRegisterArea {
    REG_AREA_INFO = 0u,
    REG_AREA_INDEX,
    REG_AREA_IFC,
    REG_AREA_FBTX,
    REG_AREA_FBRX
} FirmwareRegisterArea;

/* Register Names */

typedef enum FirmwareRegister {
    R_TAB_SEMANTICS = 0u,
    R_INDEX_ADDRESS,
    R_FW_MAJOR,
    R_FW_MINOR,
    R_FW_PATCH,
    R_FW_GIT_INC,
    R_FW_GIT_LEVEL,
    R_FW_GIT_META,
    R_ZEPHYR_MAJOR,
    R_ZEPHYR_MINOR,
    R_ZEPHYR_PATCH,
    R_UFW_MAJOR,
    R_UFW_MINOR,
    R_UFW_PATCH,
    R_IDX_SIZE,
#ifdef CR_WITH_SPI_0
    R_IDX_SPI0,
#endif /* CR_WITH_SPI_0 */
#ifdef CR_WITH_SPI_1
    R_IDX_SPI1,
#endif /* CR_WITH_SPI_1 */
#ifdef CR_WITH_I2C_0
    R_IDX_I2C0,
#endif /* CR_WITH_I2C_0 */
#ifdef CR_WITH_I2C_1
    R_IDX_I2C1,
#endif /* CR_WITH_I2C_1 */
#ifdef CR_WITH_SPI_0
    IFC_SPI_NAMES(0),
#endif /* CR_WITH_SPI_0 */
#ifdef CR_WITH_SPI_1
    IFC_SPI_NAMES(1),
#endif /* CR_WITH_SPI_1 */
#ifdef CR_WITH_I2C_0
    IFC_I2C_NAMES(0),
#endif /* CR_WITH_I2C_0 */
#ifdef CR_WITH_I2C_1
    IFC_I2C_NAMES(1)
#endif /* CR_WITH_I2C_1 */
} FirmwareRegister;

RPBlockAccess regwrite(uint32_t address, size_t n, const uint16_t *value);
RPBlockAccess regread(uint32_t address, size_t n, uint16_t *value);

#endif /* INC_REGISTERS_H_6a5bd7c3 */
