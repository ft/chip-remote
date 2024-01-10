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

/* Common Interface Definitions */

#define R_DEFAULT_FB_SIZE 0x40u
#define R_DEFAULT_FB_ADDR 0x10000u

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

#define FW_IFC_SPI_SIZE    13u
#define FW_IFC_SPI0_OFFSET IFC_START
#define FW_IFC_SPI1_OFFSET (FW_IFC_SPI0_OFFSET + FW_IFC_SPI_SIZE * CR__CNT_SPI0)

#define FW_IDX_SPI0 1
#define FW_IDX_SPI1 (FW_IDX_SPI0 + 2 * CR__CNT_SPI0)

#define FW_INTERFACE_COUNT (CR__CNT_SPI0 + CR__CNT_SPI1)

/* Interface Types */

#define R_TYPE_SPI 0u

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

#define IFC_SPI(ID, OFFSET)                                               \
    REG_U16FAIL(R_SPI##ID##_TYPE,   (OFFSET) +  0u, R_TYPE_SPI),          \
    REG_U16(R_SPI##ID##_FLEN,       (OFFSET) +  1u, R_SPI_DEFAULT_FLEN),  \
    REG_U32(R_SPI##ID##_RATE,       (OFFSET) +  2u, R_SPI_DEFAULT_RATE),  \
    REG_U16(R_SPI##ID##_FLAGS,      (OFFSET) +  4u, R_SPI_DEFAULT_FLAGS), \
    REG_U16FAIL(R_SPI##ID##_FBSIZE, (OFFSET) +  5u, R_DEFAULT_FB_SIZE),   \
    REG_U32FAIL(R_SPI##ID##_FBADDR, (OFFSET) +  6u, R_DEFAULT_FB_ADDR),   \
    REG_U16(R_SPI##ID##_CMD,        (OFFSET) +  8u, 0u),                  \
    REG_U32(R_SPI##ID##_CMDARG,     (OFFSET) +  9u, 0u),                  \
    REG_U32FAIL(R_SPI##ID##_STATUS, (OFFSET) + 11u, 0u)

#define IFC_SPI_NAMES(ID)     \
    R_SPI##ID##_TYPE,         \
    R_SPI##ID##_FLEN,         \
    R_SPI##ID##_RATE,         \
    R_SPI##ID##_FLAGS,        \
    R_SPI##ID##_FBSIZE,       \
    R_SPI##ID##_FBADDR,       \
    R_SPI##ID##_CMD,          \
    R_SPI##ID##_CMDARG,       \
    R_SPI##ID##_STATUS

#endif /* INC_INTERFACES_H_a63e12ee */
