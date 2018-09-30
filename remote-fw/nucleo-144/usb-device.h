#ifndef INC_USB_DEVICE_H
#define INC_USB_DEVICE_H

#include <usbd_def.h>

extern USBD_HandleTypeDef tty_handle;
void board_usb_init(void);

#endif /* INC_USB_DEVICE_H */
