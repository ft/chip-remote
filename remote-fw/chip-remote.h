/**
 * @file chip-remote.h
 * @brief Header for main chip-remote operation
 */

#ifndef INC_CHIP_REMOTE_H
#define INC_CHIP_REMOTE_H

struct cr_state {
    int cr_active;
    int line_pending;
};

enum cr_pin_role {
    CR_ROLE_NONE = 0,
    CR_ROLE_SPI_CLK,
    CR_ROLE_SPI_CS,
    CR_ROLE_SPI_MISO,
    CR_ROLE_SPI_MOSI
};

enum cr_access_mode {
    CR_ACCESS_READ = 0,
    CR_ACCESS_WRITE
};

void cr_set_active(int);
void cr_set_line_pending(int);
void cr_set_line_pending(int);
void cr_set_active(int);
void cr_init(int);
void cr_top_level(void);

#endif /* INC_CHIP_REMOTE_H */
