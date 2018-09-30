#ifndef INC_USART_H
#define INC_USART_H

#include <stm32f7xx_hal.h>

#include "board.h"

extern UART_HandleTypeDef huart3;
void board_usart3_init(void);

#endif /* INC_USART_H */
