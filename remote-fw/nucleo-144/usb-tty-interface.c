#include <stdint.h>

#include "usb-tty-interface.h"

uint8_t tty_tx_buffer[TTY_RX_BUFFER_SIZE];
uint8_t tty_rx_buffer[TTY_TX_BUFFER_SIZE];

extern USBD_HandleTypeDef tty_handle;

static int8_t tty_init(void);
static int8_t tty_deinit(void);
static int8_t tty_ctrl(uint8_t, uint8_t *, uint16_t);
static int8_t tty_recv(uint8_t *, uint32_t *);

USBD_CDC_ItfTypeDef tty_if_opts = { .Init = tty_init,
                                    .DeInit = tty_deinit,
                                    .Control = tty_ctrl,
                                    .Receive = tty_recv };

static int8_t
tty_init(void)
{
    USBD_CDC_SetTxBuffer(&tty_handle, tty_tx_buffer, 0);
    USBD_CDC_SetRxBuffer(&tty_handle, tty_rx_buffer);
    return USBD_OK;
}

static int8_t
tty_deinit(void)
{
    return USBD_OK;
}

static int8_t
tty_ctrl(uint8_t cmd, uint8_t *pbuf, uint16_t length)
{
    (void)pbuf;
    (void)length;

    switch (cmd) {
    case CDC_SEND_ENCAPSULATED_COMMAND:
    case CDC_GET_ENCAPSULATED_RESPONSE:
    case CDC_SET_COMM_FEATURE:
    case CDC_GET_COMM_FEATURE:
    case CDC_CLEAR_COMM_FEATURE:
    case CDC_SET_LINE_CODING:
    case CDC_GET_LINE_CODING:
    case CDC_SET_CONTROL_LINE_STATE:
    case CDC_SEND_BREAK:
    default:
        break;
    }

    return USBD_OK;
}

SerialRecvCallback tty_recv_cb = NULL;

void
set_tty_recv_cb(SerialRecvCallback cb)
{
    tty_recv_cb = cb;
}

static int8_t
tty_recv(uint8_t *buf, uint32_t *n)
{
    (void)n;

    USBD_CDC_SetRxBuffer(&tty_handle, buf);
    USBD_CDC_ReceivePacket(&tty_handle);
    if (tty_recv_cb != NULL)
        tty_recv_cb(buf, *n);

    return USBD_OK;
}

uint8_t
tty_send(uint8_t *buf, uint16_t n)
{
    USBD_CDC_HandleTypeDef *h;
    uint8_t result = USBD_OK;

    h = (USBD_CDC_HandleTypeDef *)tty_handle.pClassData;

    if (h->TxState != 0)
        return USBD_BUSY;

    USBD_CDC_SetTxBuffer(&tty_handle, buf, n);
    result = USBD_CDC_TransmitPacket(&tty_handle);

    return result;
}
