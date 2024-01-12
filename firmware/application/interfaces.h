/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_INTERFACES_H_a63e12ee
#define INC_INTERFACES_H_a63e12ee

#include <zephyr/kernel.h>

#if DT_NODE_EXISTS(DT_CHOSEN(chipremote_spi0))
#define CR_WITH_SPI_0
#endif /* cr_spi_0 */

#if DT_NODE_EXISTS(DT_CHOSEN(chipremote_spi1))
#define CR_WITH_SPI_1
#endif /* cr_spi_1 */

#if DT_NODE_EXISTS(DT_CHOSEN(chipremote_i2c0))
#define CR_WITH_I2C_0
#endif /* cr_i2c_0 */

#if DT_NODE_EXISTS(DT_CHOSEN(chipremote_i2c1))
#define CR_WITH_I2C_1
#endif /* cr_i2c_1 */

/* Common Interface Definitions */

#define R_DEFAULT_FBTX_SIZE 0x40u
#define R_DEFAULT_FBTX_ADDR 0x10000u
#define R_DEFAULT_FBRX_SIZE 0x40u
#define R_DEFAULT_FBRX_ADDR (0x10000u + R_DEFAULT_FBTX_SIZE)

/* Counting stuff */

#ifdef CR_WITH_SPI_0
#define CR__CNT_SPI0 1u
#else
#define CR__CNT_SPI0 0u
#endif /* CR_WITH_SPI_0 */

#ifdef CR_WITH_SPI_1
#define CR__CNT_SPI1 1u
#else
#define CR__CNT_SPI1 0u
#endif /* CR_WITH_SPI_1 */

#ifdef CR_WITH_I2C_0
#define CR__CNT_I2C0 1u
#else
#define CR__CNT_I2C0 0u
#endif /* CR_WITH_I2C_0 */

#ifdef CR_WITH_I2C_1
#define CR__CNT_I2C1 1u
#else
#define CR__CNT_I2C1 0u
#endif /* CR_WITH_I2C_1 */

#define FW_IFC_SPI_SIZE 16u
#define FW_IFC_I2C_SIZE 14u

#define FW_IFC_SPI0_OFFSET IFC_START
#define FW_IFC_SPI1_OFFSET (FW_IFC_SPI0_OFFSET + FW_IFC_SPI_SIZE * CR__CNT_SPI0)

#define FW_IFC_I2C0_OFFSET (FW_IFC_SPI1_OFFSET + FW_IFC_SPI_SIZE * CR__CNT_SPI1)
#define FW_IFC_I2C1_OFFSET (FW_IFC_I2C0_OFFSET + FW_IFC_I2C_SIZE * CR__CNT_I2C0)

#define ADDRREG_SIZE 2
#define FW_IDX_SPI0 1
#define FW_IDX_SPI1 (FW_IDX_SPI0 + ADDRREG_SIZE * CR__CNT_SPI0)
#define FW_IDX_I2C0 (FW_IDX_SPI1 + ADDRREG_SIZE * CR__CNT_SPI1)
#define FW_IDX_I2C1 (FW_IDX_I2C0 + ADDRREG_SIZE * CR__CNT_I2C0)

#define FW_INTERFACE_COUNT (CR__CNT_SPI0 + CR__CNT_SPI1 +       \
                            CR__CNT_I2C0 + CR__CNT_I2C1)

/* Interface Types */

#define R_TYPE_SPI 0u
#define R_TYPE_I2C 1u

/* SPI Interface */

#define R_SPI_FLAG_CS_ACTIVE_LOW     BIT(0)
#define R_SPI_FLAG_MSB_FIRST         BIT(1)
#define R_SPI_FLAG_CLOCK_IDLE_LOW    BIT(2)
#define R_SPI_FLAG_CLOCK_PHASE_DELAY BIT(3)

#define R_SPI_DEFAULT_FLEN 8u
#define R_SPI_DEFAULT_RATE 1000000ul
#define R_SPI_DEFAULT_FLAGS                     \
    (R_SPI_FLAG_MSB_FIRST                       \
    |R_SPI_FLAG_CS_ACTIVE_LOW                   \
    |R_SPI_FLAG_CLOCK_IDLE_LOW)

#define IFC_SPI(ID, OFFSET)                                                 \
    REG_U16FAIL(R_SPI##ID##_TYPE,     (OFFSET) +  0u, R_TYPE_SPI),          \
    REG_U16(R_SPI##ID##_FLEN,         (OFFSET) +  1u, R_SPI_DEFAULT_FLEN),  \
    REG_U32(R_SPI##ID##_RATE,         (OFFSET) +  2u, R_SPI_DEFAULT_RATE),  \
    REG_U16(R_SPI##ID##_FLAGS,        (OFFSET) +  4u, R_SPI_DEFAULT_FLAGS), \
    REG_U16FAIL(R_SPI##ID##_FBTXSIZE, (OFFSET) +  5u, R_DEFAULT_FBTX_SIZE), \
    REG_U32FAIL(R_SPI##ID##_FBTXADDR, (OFFSET) +  6u, R_DEFAULT_FBTX_ADDR), \
    REG_U16FAIL(R_SPI##ID##_FBRXSIZE, (OFFSET) +  8u, R_DEFAULT_FBRX_SIZE), \
    REG_U32FAIL(R_SPI##ID##_FBRXADDR, (OFFSET) +  9u, R_DEFAULT_FBRX_ADDR), \
    REG_U16(R_SPI##ID##_CMD,          (OFFSET) + 11u, 0u),                  \
    REG_U32(R_SPI##ID##_CMDARG,       (OFFSET) + 12u, 0u),                  \
    REG_U32FAIL(R_SPI##ID##_STATUS,   (OFFSET) + 14u, 0u)

#define IFC_SPI_NAMES(ID)     \
    R_SPI##ID##_TYPE,         \
    R_SPI##ID##_FLEN,         \
    R_SPI##ID##_RATE,         \
    R_SPI##ID##_FLAGS,        \
    R_SPI##ID##_FBTXSIZE,     \
    R_SPI##ID##_FBTXADDR,     \
    R_SPI##ID##_FBRXSIZE,     \
    R_SPI##ID##_FBRXADDR,     \
    R_SPI##ID##_CMD,          \
    R_SPI##ID##_CMDARG,       \
    R_SPI##ID##_STATUS

#define R_I2C_DEFAULT_CFG 0u

#define IFC_I2C(ID, OFFSET)                                                 \
    REG_U16FAIL(R_I2C##ID##_TYPE,     (OFFSET) +  0u, R_TYPE_I2C),          \
    REG_U16(R_I2C##ID##_CONFIG,       (OFFSET) +  1u, R_I2C_DEFAULT_CFG),   \
    REG_U16(R_I2C##ID##_CHIP_ADDRESS, (OFFSET) +  2u, 0u),                  \
    REG_U16FAIL(R_I2C##ID##_FBTXSIZE, (OFFSET) +  3u, R_DEFAULT_FBTX_SIZE), \
    REG_U32FAIL(R_I2C##ID##_FBTXADDR, (OFFSET) +  4u, R_DEFAULT_FBTX_ADDR), \
    REG_U16FAIL(R_I2C##ID##_FBRXSIZE, (OFFSET) +  6u, R_DEFAULT_FBRX_SIZE), \
    REG_U32FAIL(R_I2C##ID##_FBRXADDR, (OFFSET) +  7u, R_DEFAULT_FBRX_ADDR), \
    REG_U16(R_I2C##ID##_CMD,          (OFFSET) +  9u, 0u),                  \
    REG_U32(R_I2C##ID##_CMDARG,       (OFFSET) + 10u, 0u),                  \
    REG_U32FAIL(R_I2C##ID##_STATUS,   (OFFSET) + 12u, 0u)

#define IFC_I2C_NAMES(ID)     \
    R_I2C##ID##_TYPE,         \
    R_I2C##ID##_CONFIG,       \
    R_I2C##ID##_CHIP_ADDRESS, \
    R_I2C##ID##_FBTXSIZE,     \
    R_I2C##ID##_FBTXADDR,     \
    R_I2C##ID##_FBRXSIZE,     \
    R_I2C##ID##_FBRXADDR,     \
    R_I2C##ID##_CMD,          \
    R_I2C##ID##_CMDARG,       \
    R_I2C##ID##_STATUS

#endif /* INC_INTERFACES_H_a63e12ee */
