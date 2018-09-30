#ifndef INC_USBD_CONF_H
#define INC_USBD_CONF_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <stm32f7xx.h>
#include <stm32f7xx_hal.h>

#define USBD_MAX_NUM_INTERFACES 1
#define USBD_MAX_NUM_CONFIGURATION 1
#define USBD_MAX_STR_DESC_SIZ 512
#define USBD_SUPPORT_USER_STRING 0
#define USBD_DEBUG_LEVEL 0
#define USBD_LPM_ENABLED 1
#define USBD_SELF_POWERED 1

#define DEVICE_FS 0
#define DEVICE_HS 1

#define USBD_malloc malloc
#define USBD_free free
#define USBD_memset memset
#define USBD_memcpy memcpy
#define USBD_Delay HAL_Delay

#if (USBD_DEBUG_LEVEL > 0)
#define USBD_UsrLog(...)         \
    do {                         \
        printf(__VA_ARGS__);     \
        printf("\n");            \
    } while (/* CONSTCOND */ 0)
#else
#define USBD_UsrLog(...)
#endif

#if (USBD_DEBUG_LEVEL > 1)

#define USBD_ErrLog(...)         \
    do {                         \
        printf("ERROR: ");       \
        printf(__VA_ARGS__);     \
        printf("\n");            \
    } while (/* CONSTCOND */ 0)
#else
#define USBD_ErrLog(...)
#endif

#if (USBD_DEBUG_LEVEL > 2)
#define USBD_DbgLog(...)         \
    do {                         \
        printf("DEBUG : ");      \
        printf(__VA_ARGS__);     \
        printf("\n");            \
    } while (/* CONSTCOND */ 0)
#else
#define USBD_DbgLog(...)
#endif

#endif /* INC_USBD_CONF_H */
