#include <usbd_core.h>

#include "usbd_conf.h"
#include "usb-tty-description.h"

#define USB_LANG_CHINESE_PRC 0x0804u
#define USB_LANG_CHINESE_TAIWAN 0x0404u
#define USB_LANG_EN_US 0x0409u
#define USB_LANG_EN_UK 0x0809u
#define USB_LANG_EN_AUS 0x0c09u
#define USB_LANG_EN_CA 0x1009u
#define USB_LANG_EN_NZ 0x1409u
#define USB_LANG_FRENCH 0x040cu
#define USB_LANG_GERMAN 0x0407u
#define USB_LANG_HINDI 0x0439u
#define USB_LANG_ITALIAN 0x0410u
#define USB_LANG_JAPANESE 0x0411u
#define USB_LANG_KOREAN 0x0412u
#define USB_LANG_ES_TRAD 0x040Au
#define USB_LANG_ES_MODERN 0x0c0au
#define USB_LANG_SWAHILI 0x0441u
#define USB_LANG_URDU_IN 0x0820u
#define USB_LANG_URDU_PK 0x0420u

#define USB_TTY_VENDOR_ID 0x2342u
#define USB_TTY_LANGUAGE_ID USB_LANG_EN_UK
#define USB_TTY_MANUFACTURER "ChipRemote Project"
#define USB_TTY_PRODUCT_ID 0xf64fu
#define USB_TTY_PRODUCT_STRING "ChipRemote Controller"
#define USB_TTY_SERIAL_STRING "23deadbeef42"
#define USB_TTY_CFG_STRING "CDC Config"
#define USB_TTY_IF_STRING "CDC Interface"

uint8_t *tty_device_desc(USBD_SpeedTypeDef, uint16_t *);
uint8_t *tty_lang_desc(USBD_SpeedTypeDef, uint16_t *);
uint8_t *tty_manufacturer_desc(USBD_SpeedTypeDef, uint16_t *);
uint8_t *tty_product_desc(USBD_SpeedTypeDef, uint16_t *);
uint8_t *tty_serial_desc(USBD_SpeedTypeDef, uint16_t *);
uint8_t *tty_config_desc(USBD_SpeedTypeDef, uint16_t *);
uint8_t *tty_interface_desc(USBD_SpeedTypeDef, uint16_t *);

#if (USBD_LPM_ENABLED == 1)
#define USB_BOS_DESC_SIZE 0x0C
uint8_t *tty_bos_desc(USBD_SpeedTypeDef, uint16_t *);
#endif /* (USBD_LPM_ENABLED == 1) */

USBD_DescriptorsTypeDef tty_descriptors = {
    .GetDeviceDescriptor = tty_device_desc,
    .GetLangIDStrDescriptor = tty_lang_desc,
    .GetManufacturerStrDescriptor = tty_manufacturer_desc,
    .GetProductStrDescriptor = tty_product_desc,
    .GetSerialStrDescriptor = tty_serial_desc,
    .GetConfigurationStrDescriptor = tty_config_desc,
    .GetInterfaceStrDescriptor = tty_interface_desc
#if (USBD_LPM_ENABLED == 1)
    ,
    .GetBOSDescriptor = tty_bos_desc
#endif /* (USBD_LPM_ENABLED == 1) */
};

/** USB standard device descriptor. */
__ALIGN_BEGIN uint8_t tty_device_data[USB_LEN_DEV_DESC] __ALIGN_END = {
    0x12,
    USB_DESC_TYPE_DEVICE,
#if (USBD_LPM_ENABLED == 1)
    0x01,
#else
    0x00, /*bcdUSB */
#endif /* (USBD_LPM_ENABLED == 1) */
    0x02,
    0x02,
    0x02,
    0x00,
    USB_MAX_EP0_SIZE,
    LOBYTE(USB_TTY_VENDOR_ID),
    HIBYTE(USB_TTY_VENDOR_ID),
    LOBYTE(USB_TTY_PRODUCT_ID),
    HIBYTE(USB_TTY_PRODUCT_ID),
    0x00,
    0x02,
    USBD_IDX_MFC_STR,
    USBD_IDX_PRODUCT_STR,
    USBD_IDX_SERIAL_STR,
    USBD_MAX_NUM_CONFIGURATION
};

#if (USBD_LPM_ENABLED == 1)
#if defined(__ICCARM__) /* IAR Compiler */
#pragma data_alignment = 4
#endif /* defined ( __ICCARM__ ) */
__ALIGN_BEGIN uint8_t tty_bos_data[USB_BOS_DESC_SIZE] __ALIGN_END = {
    0x5,
    USB_DESC_TYPE_BOS,
    0xC,
    0x0,
    0x1,
    0x7,
    USB_DEVICE_CAPABITY_TYPE,
    0x2,
    0x2,
    0x0,
    0x0,
    0x0
};
#endif /* (USBD_LPM_ENABLED == 1) */

#if defined(__ICCARM__) /* IAR Compiler */
#pragma data_alignment = 4
#endif /* defined ( __ICCARM__ ) */
__ALIGN_BEGIN uint8_t tty_lang_data[USB_LEN_LANGID_STR_DESC] __ALIGN_END = {
    USB_LEN_LANGID_STR_DESC,
    USB_DESC_TYPE_STRING,
    LOBYTE(USB_TTY_LANGUAGE_ID),
    HIBYTE(USB_TTY_LANGUAGE_ID)
};

#if defined(__ICCARM__) /* IAR Compiler */
#pragma data_alignment = 4
#endif /* defined ( __ICCARM__ ) */
__ALIGN_BEGIN uint8_t tty_string_data[USBD_MAX_STR_DESC_SIZ] __ALIGN_END;

uint8_t *
tty_device_desc(USBD_SpeedTypeDef speed, uint16_t *length)
{
    (void)speed;
    *length = sizeof(tty_device_data);
    return tty_device_data;
}

uint8_t *
tty_lang_desc(USBD_SpeedTypeDef speed, uint16_t *length)
{
    (void)speed;
    *length = sizeof(tty_lang_data);
    return tty_lang_data;
}

uint8_t *
tty_product_desc(USBD_SpeedTypeDef speed, uint16_t *length)
{
    (void)speed;
    USBD_GetString((uint8_t *)USB_TTY_PRODUCT_STRING, tty_string_data, length);
    return tty_string_data;
}

uint8_t *
tty_manufacturer_desc(USBD_SpeedTypeDef speed, uint16_t *length)
{
    (void)speed;
    USBD_GetString((uint8_t *)USB_TTY_MANUFACTURER, tty_string_data, length);
    return tty_string_data;
}

uint8_t *
tty_serial_desc(USBD_SpeedTypeDef speed, uint16_t *length)
{
    (void)speed;
    USBD_GetString((uint8_t *)USB_TTY_SERIAL_STRING, tty_string_data, length);
    return tty_string_data;
}

uint8_t *
tty_config_desc(USBD_SpeedTypeDef speed, uint16_t *length)
{
    (void)speed;
    USBD_GetString((uint8_t *)USB_TTY_CFG_STRING, tty_string_data, length);
    return tty_string_data;
}

uint8_t *
tty_interface_desc(USBD_SpeedTypeDef speed, uint16_t *length)
{
    (void)speed;
    USBD_GetString((uint8_t *)USB_TTY_IF_STRING, tty_string_data, length);
    return tty_string_data;
}

#if (USBD_LPM_ENABLED == 1)
uint8_t *
tty_bos_desc(USBD_SpeedTypeDef speed, uint16_t *length)
{
    (void)speed;
    *length = sizeof(tty_bos_data);
    return (uint8_t *)tty_bos_data;
}
#endif /* (USBD_LPM_ENABLED == 1) */
