/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_REGISTERS_H_6a5bd7c3
#define INC_REGISTERS_H_6a5bd7c3

#include <ufw/register-table.h>

extern RegisterTable registers;

#define FIRMWARE_VERSION_MAJOR 0u
#define FIRMWARE_VERSION_MINOR 0u
#define FIRMWARE_PATCHLEVEL    1u

#define INFO_START      0x0000ul
#define INFO_SIZE         0x20ul
#define STATUS_START    0x0200ul
#define STATUS_SIZE       0x40ul
#define MEMORY_START    0x1000ul
#define MEMORY_SIZE      0x200ul

typedef enum FirmwareRegisterArea {
    REG_AREA_INFO = 0u,
    REG_AREA_STATUS,
    REG_AREA_MEMORY
} FirmwareRegisterArea;

typedef enum SensorRegister {
    R_FW_MAJOR,
    R_FW_MINOR,
    R_FW_PATCH,
    R_STATUS,
} SensorRegister;

#endif /* INC_REGISTERS_H_6a5bd7c3 */
