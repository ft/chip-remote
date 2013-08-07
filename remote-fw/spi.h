#ifndef INC_SPI_H
#define INC_SPI_H

#define SPI_MSB_FIRST 0x0f
#define SPI_LSB_FIRST 0xf0

struct spi_cfg {
    /** currently ignored */
    int clk_rate;
    /** integer: number of chip-select-lines */
    int cs_lines_num;
    /** [0|1] Chip Select Polarity */
    char cs_pol;
    /** [0|1] Clock Polarity */
    char clk_pol;
    /** [0|1] Clock Phase Delay */
    char clk_phase_delay;
    int frame_length;
    /** [SPI_MSB_FIRST|SPI_LSB_FIRST] */
    char bit_order;
};

#endif /* INC_SPI_H */
