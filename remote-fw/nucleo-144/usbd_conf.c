#include <stm32f7xx.h>
#include <stm32f7xx_hal.h>
#include <usbd_core.h>
#include <usbd_def.h>

#include "board.h"
#include "clock.h"
#include "interrupts.h"

PCD_HandleTypeDef hpcd_USB_OTG_FS;

void
HAL_PCD_MspInit(PCD_HandleTypeDef *pcdHandle)
{
    GPIO_InitTypeDef GPIO_InitStruct;
    if (pcdHandle->Instance == USB_OTG_FS) {
        GPIO_InitStruct.Pin = (USB_SOF_Pin
                             | USB_ID_Pin
                             | USB_DM_Pin
                             | USB_DP_Pin);
        GPIO_InitStruct.Mode = GPIO_MODE_AF_PP;
        GPIO_InitStruct.Pull = GPIO_NOPULL;
        GPIO_InitStruct.Speed = GPIO_SPEED_FREQ_VERY_HIGH;
        GPIO_InitStruct.Alternate = GPIO_AF10_OTG_FS;
        HAL_GPIO_Init(GPIOA, &GPIO_InitStruct);

        GPIO_InitStruct.Pin = USB_VBUS_Pin;
        GPIO_InitStruct.Mode = GPIO_MODE_INPUT;
        GPIO_InitStruct.Pull = GPIO_NOPULL;
        HAL_GPIO_Init(USB_VBUS_GPIO_Port, &GPIO_InitStruct);
        __HAL_RCC_USB_OTG_FS_CLK_ENABLE();
        HAL_NVIC_SetPriority(OTG_FS_IRQn, 0, 0);
        HAL_NVIC_EnableIRQ(OTG_FS_IRQn);
    }
}

void
HAL_PCD_MspDeInit(PCD_HandleTypeDef *pcdHandle)
{
    if (pcdHandle->Instance == USB_OTG_FS) {
        __HAL_RCC_USB_OTG_FS_CLK_DISABLE();
        HAL_GPIO_DeInit(GPIOA, (USB_SOF_Pin
                              | USB_VBUS_Pin
                              | USB_ID_Pin
                              | USB_DM_Pin
                              | USB_DP_Pin));
        HAL_NVIC_DisableIRQ(OTG_FS_IRQn);
    }
}

void
HAL_PCD_SetupStageCallback(PCD_HandleTypeDef *hpcd)
{
    USBD_LL_SetupStage((USBD_HandleTypeDef *)hpcd->pData,
                       (uint8_t *)hpcd->Setup);
}

void
HAL_PCD_DataOutStageCallback(PCD_HandleTypeDef *hpcd, uint8_t epnum)
{
    USBD_LL_DataOutStage((USBD_HandleTypeDef *)hpcd->pData,
                         epnum,
                         hpcd->OUT_ep[epnum].xfer_buff);
}

void
HAL_PCD_DataInStageCallback(PCD_HandleTypeDef *hpcd, uint8_t epnum)
{
    USBD_LL_DataInStage((USBD_HandleTypeDef *)hpcd->pData,
                        epnum,
                        hpcd->IN_ep[epnum].xfer_buff);
}

void
HAL_PCD_SOFCallback(PCD_HandleTypeDef *hpcd)
{
    USBD_LL_SOF((USBD_HandleTypeDef *)hpcd->pData);
}

void
HAL_PCD_ResetCallback(PCD_HandleTypeDef *hpcd)
{
    USBD_SpeedTypeDef speed = USBD_SPEED_FULL;

    switch (hpcd->Init.speed) {
    case PCD_SPEED_HIGH:
        speed = USBD_SPEED_HIGH;
        break;
    case PCD_SPEED_FULL:
        speed = USBD_SPEED_FULL;
        break;

    default:
        speed = USBD_SPEED_FULL;
        break;
    }
    USBD_LL_SetSpeed((USBD_HandleTypeDef *)hpcd->pData, speed);
    USBD_LL_Reset((USBD_HandleTypeDef *)hpcd->pData);
}

void
HAL_PCD_SuspendCallback(PCD_HandleTypeDef *hpcd)
{
    USBD_LL_Suspend((USBD_HandleTypeDef *)hpcd->pData);
    __HAL_PCD_GATE_PHYCLOCK(hpcd);
    if (hpcd->Init.low_power_enable) {
        SCB->SCR |= (uint32_t)((uint32_t)(SCB_SCR_SLEEPDEEP_Msk
                                        | SCB_SCR_SLEEPONEXIT_Msk));
    }
}

void
HAL_PCD_ResumeCallback(PCD_HandleTypeDef *hpcd)
{
    USBD_LL_Resume((USBD_HandleTypeDef *)hpcd->pData);
}

void
HAL_PCD_ISOOUTIncompleteCallback(PCD_HandleTypeDef *hpcd, uint8_t epnum)
{
    USBD_LL_IsoOUTIncomplete((USBD_HandleTypeDef *)hpcd->pData, epnum);
}

void
HAL_PCD_ISOINIncompleteCallback(PCD_HandleTypeDef *hpcd, uint8_t epnum)
{
    USBD_LL_IsoINIncomplete((USBD_HandleTypeDef *)hpcd->pData, epnum);
}

void
HAL_PCD_ConnectCallback(PCD_HandleTypeDef *hpcd)
{
    USBD_LL_DevConnected((USBD_HandleTypeDef *)hpcd->pData);
}

void
HAL_PCD_DisconnectCallback(PCD_HandleTypeDef *hpcd)
{
    USBD_LL_DevDisconnected((USBD_HandleTypeDef *)hpcd->pData);
}

USBD_StatusTypeDef
USBD_LL_Init(USBD_HandleTypeDef *pdev)
{
    if (pdev->id == DEVICE_FS) {
        hpcd_USB_OTG_FS.pData = pdev;
        pdev->pData = &hpcd_USB_OTG_FS;
        hpcd_USB_OTG_FS.Instance = USB_OTG_FS;
        hpcd_USB_OTG_FS.Init.dev_endpoints = 6;
        hpcd_USB_OTG_FS.Init.speed = PCD_SPEED_FULL;
        hpcd_USB_OTG_FS.Init.dma_enable = DISABLE;
        hpcd_USB_OTG_FS.Init.ep0_mps = DEP0CTL_MPS_64;
        hpcd_USB_OTG_FS.Init.phy_itface = PCD_PHY_EMBEDDED;
        hpcd_USB_OTG_FS.Init.Sof_enable = ENABLE;
        hpcd_USB_OTG_FS.Init.low_power_enable = DISABLE;
        hpcd_USB_OTG_FS.Init.lpm_enable = DISABLE;
        hpcd_USB_OTG_FS.Init.vbus_sensing_enable = ENABLE;
        hpcd_USB_OTG_FS.Init.use_dedicated_ep1 = DISABLE;

        if (HAL_PCD_Init(&hpcd_USB_OTG_FS) != HAL_OK) {
            Error_Handler();
        }

        HAL_PCDEx_SetRxFiFo(&hpcd_USB_OTG_FS, 0x80);
        HAL_PCDEx_SetTxFiFo(&hpcd_USB_OTG_FS, 0, 0x40);
        HAL_PCDEx_SetTxFiFo(&hpcd_USB_OTG_FS, 1, 0x80);
    }

    return USBD_OK;
}

static inline USBD_StatusTypeDef
hal2usb_status(HAL_StatusTypeDef hs)
{
    switch (hs) {
    case HAL_OK: return USBD_OK;
    case HAL_ERROR: return USBD_FAIL;
    case HAL_BUSY: return USBD_BUSY;
    case HAL_TIMEOUT: return USBD_FAIL;
    default: return USBD_FAIL;
    }
}

USBD_StatusTypeDef
USBD_LL_DeInit(USBD_HandleTypeDef *pdev)
{
    return hal2usb_status(HAL_PCD_DeInit(pdev->pData));
}

USBD_StatusTypeDef
USBD_LL_Start(USBD_HandleTypeDef *pdev)
{
    return hal2usb_status(HAL_PCD_Start(pdev->pData));
}

USBD_StatusTypeDef
USBD_LL_Stop(USBD_HandleTypeDef *pdev)
{
    return hal2usb_status(HAL_PCD_Stop(pdev->pData));
}

USBD_StatusTypeDef
USBD_LL_OpenEP(USBD_HandleTypeDef *pdev,
               uint8_t ep_addr,
               uint8_t ep_type,
               uint16_t ep_mps)
{
    return hal2usb_status(
        HAL_PCD_EP_Open(pdev->pData, ep_addr, ep_mps, ep_type));
}

USBD_StatusTypeDef
USBD_LL_CloseEP(USBD_HandleTypeDef *pdev, uint8_t ep_addr)
{
    return hal2usb_status(HAL_PCD_EP_Close(pdev->pData, ep_addr));
}

USBD_StatusTypeDef
USBD_LL_FlushEP(USBD_HandleTypeDef *pdev, uint8_t ep_addr)
{
    return hal2usb_status(HAL_PCD_EP_Flush(pdev->pData, ep_addr));
}

USBD_StatusTypeDef
USBD_LL_StallEP(USBD_HandleTypeDef *pdev, uint8_t ep_addr)
{
    return hal2usb_status(HAL_PCD_EP_SetStall(pdev->pData, ep_addr));
}

USBD_StatusTypeDef
USBD_LL_ClearStallEP(USBD_HandleTypeDef *pdev, uint8_t ep_addr)
{
    return hal2usb_status(HAL_PCD_EP_ClrStall(pdev->pData, ep_addr));
}

uint8_t
USBD_LL_IsStallEP(USBD_HandleTypeDef *pdev, uint8_t ep_addr)
{
    PCD_HandleTypeDef *hpcd = (PCD_HandleTypeDef *)pdev->pData;

    if ((ep_addr & 0x80) == 0x80) {
        return hpcd->IN_ep[ep_addr & 0x7F].is_stall;
    } else {
        return hpcd->OUT_ep[ep_addr & 0x7F].is_stall;
    }
}

USBD_StatusTypeDef
USBD_LL_SetUSBAddress(USBD_HandleTypeDef *pdev, uint8_t dev_addr)
{
    return hal2usb_status(HAL_PCD_SetAddress(pdev->pData, dev_addr));
}

USBD_StatusTypeDef
USBD_LL_Transmit(USBD_HandleTypeDef *pdev,
                 uint8_t ep_addr,
                 uint8_t *pbuf,
                 uint16_t size)
{
    return hal2usb_status(
        HAL_PCD_EP_Transmit(pdev->pData, ep_addr, pbuf, size));
}

USBD_StatusTypeDef
USBD_LL_PrepareReceive(USBD_HandleTypeDef *pdev,
                       uint8_t ep_addr,
                       uint8_t *pbuf,
                       uint16_t size)
{
    return hal2usb_status(HAL_PCD_EP_Receive(pdev->pData, ep_addr, pbuf, size));

}

uint32_t
USBD_LL_GetRxDataSize(USBD_HandleTypeDef *pdev, uint8_t ep_addr)
{
    return HAL_PCD_EP_GetRxCount((PCD_HandleTypeDef *)pdev->pData, ep_addr);
}

#if (USBD_LPM_ENABLED == 1)
void
HAL_PCDEx_LPM_Callback(PCD_HandleTypeDef *hpcd, PCD_LPM_MsgTypeDef msg)
{
    switch (msg) {
    case PCD_LPM_L0_ACTIVE:
        if (hpcd->Init.low_power_enable) {
            board_clock_init();
            SCB->SCR &= (uint32_t) ~((uint32_t)(SCB_SCR_SLEEPDEEP_Msk
                                              | SCB_SCR_SLEEPONEXIT_Msk));
        }
        __HAL_PCD_UNGATE_PHYCLOCK(hpcd);
        USBD_LL_Resume(hpcd->pData);
        break;

    case PCD_LPM_L1_ACTIVE:
        __HAL_PCD_GATE_PHYCLOCK(hpcd);
        USBD_LL_Suspend(hpcd->pData);

        if (hpcd->Init.low_power_enable) {
            SCB->SCR |= (uint32_t)((uint32_t)(SCB_SCR_SLEEPDEEP_Msk
                                            | SCB_SCR_SLEEPONEXIT_Msk));
        }
        break;
    }
}
#endif /* (USBD_LPM_ENABLED == 1) */
