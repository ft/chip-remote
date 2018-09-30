#ifndef INC_ETH_H
#define INC_ETH_H

#include <stm32f7xx_hal.h>

extern ETH_HandleTypeDef heth;

void board_eth_init(void);

#endif /* INC_ETH_H */
