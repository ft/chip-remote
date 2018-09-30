/**
 * @file board.c
 * @brief Nucleo-144 Board Initialisation
 *
 * This file implements the board_init() function, that calls subsystem
 * initialisers in order to bring the nucleo-144 board up and running.
 */

#include <stm32f7xx_hal.h>

#include "board.h"
#include "clock.h"
#include "eth.h"
#include "gpio.h"
#include "usart.h"
#include "usb-device.h"

/**
 * Main board initialisation dispatcher
 *
 * This procedure initialises STM's hardware abstraction layer and then calls
 * the subsystem initialisers that are defined in separate files.
 *
 * @return void
 * @sideeffects Initialises the main board and controller
 */
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
