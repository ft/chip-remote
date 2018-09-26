MEMORY
{
    rom (rx) : ORIGIN = 0x08000000, LENGTH = 2048K
    ram (rwx) : ORIGIN = 0x20000000, LENGTH = 512K
}

INCLUDE libopencm3_stm32f7.ld
