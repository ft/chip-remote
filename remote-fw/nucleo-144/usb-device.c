#include <usbd_core.h>
#include <usbd_cdc.h>

#include "usb-device.h"
#include "usb-tty-interface.h"
#include "usb-tty-description.h"

USBD_HandleTypeDef tty_handle;

void
board_usb_init(void) {
    USBD_Init(&tty_handle, &tty_descriptors, DEVICE_FS);
    USBD_RegisterClass(&tty_handle, &USBD_CDC);
    USBD_CDC_RegisterInterface(&tty_handle, &tty_if_opts);
    USBD_Start(&tty_handle);
}
