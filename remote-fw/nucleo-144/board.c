#include <stm32f7xx_hal.h>

#include "board.h"
#include "clock.h"
#include "eth.h"
#include "gpio.h"
#include "usart.h"
#include "usb-device.h"

void
board_init(void)
{
    HAL_Init();
    board_clock_init();
    board_gpio_init();
#ifdef WITH_ETHERNET
    board_eth_init();
#endif /* WITH_ETHERNET */
    board_usart3_init();
    board_usb_init();
}
