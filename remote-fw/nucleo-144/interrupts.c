#include <stm32f7xx_hal.h>

#include "interrupts.h"

extern PCD_HandleTypeDef hpcd_USB_OTG_FS;

#define NULLHANDLER                             \
    char *f = __FILE__;                         \
    int l = __LINE__;                           \
                                                \
    (void)f;                                    \
    (void)l;                                    \
    for (;;) {                                  \
        __asm__(" nop");                        \
    }

void NMI_Handler(void) {}
void HardFault_Handler(void) { NULLHANDLER }
void MemManage_Handler(void) { NULLHANDLER }
void BusFault_Handler(void) { NULLHANDLER }
void UsageFault_Handler(void) { NULLHANDLER }
void SVC_Handler(void) {}
void DebugMon_Handler(void) {}
void PendSV_Handler(void) {}

void
SysTick_Handler(void)
{
    HAL_IncTick();
    HAL_SYSTICK_IRQHandler();
}

void
OTG_FS_IRQHandler(void)
{
    HAL_PCD_IRQHandler(&hpcd_USB_OTG_FS);
}

void
_Error_Handler(char *file, int line)
{
    (void)file;
    (void)line;
    for (;;) {
        __asm__(" nop");
    }
}
