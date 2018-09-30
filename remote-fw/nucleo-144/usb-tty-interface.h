#ifndef INC_USBD_CDC_IF_H
#define INC_USBD_CDC_IF_H

#include <usbd_cdc.h>

#define TTY_RX_BUFFER_SIZE 512u
#define TTY_TX_BUFFER_SIZE 512u

extern USBD_CDC_ItfTypeDef tty_if_opts;

uint8_t tty_send(uint8_t* Buf, uint16_t Len);

#endif /* INC_USBD_CDC_IF_H */
