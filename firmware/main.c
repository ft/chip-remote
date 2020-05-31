#include <zephyr.h>
#include <sys/printk.h>

#include <string.h>
#include <c/compat.h>

#define MAX_BOARD_NAME_LENGTH 32

void
main(void)
{
    char board[MAX_BOARD_NAME_LENGTH];
    strlcpy(board, CONFIG_BOARD, sizeof(board));
    printk("ChipRemoteFirmware running on %s\n", board);
}
