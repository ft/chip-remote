#ifndef INC_LINE_MAPS_H
#define INC_LINE_MAPS_H

struct cr_spi_map {
    int cs_n;
    int cs_focused;
    struct cr_line *clk;
    struct cr_line *mosi;
    struct cr_line *miso;
    struct cr_line **cs;
    /** currently ignored */
    int clk_rate;
    /** integer: number of chip-select-lines */
    int cs_lines_num;
    /** [0|1] Chip Select Polarity */
    char cs_polarity;
    /** [0|1] Clock Polarity */
    char clk_polarity;
    /** [0|1] Clock Phase Delay */
    char clk_phase_delay;
    int frame_length;
    /** [SPI_MSB_FIRST|SPI_LSB_FIRST] */
    int bit_order;
    /** indicate whether the focused chip-select line changed */
    int focus_changed;
};

#endif /* INC_LINE_MAPS_H */
