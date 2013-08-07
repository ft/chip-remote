/**
 * @file spi.c
 * @brief Portable code for SPI operation
 *
 * The Serial Peripheral Interface Bus (SPI) is a synchronous serial interface,
 * that is very commonly used for chip configuration purposes. It features
 * exactly one master and - depending on the setup - one or more slave devices
 * that share the bus.
 *
 * In its most generic form it uses four lines:
 *
 * - CLK
 * - CS
 * - MOSI
 * - MISO
 *
 * CLK is the general clock line, that the master controls and all slaves
 * synchronise to.
 *
 * CS is a chip-select line, that the master uses to tell which of the slaves
 * should listen (sometimes this line is also called LE [latch-enable] and
 * sometimes SS [slave-select], but we'll stick with CS).
 *
 * MOSI and MISO are the data lines. MOSI is Master-Out-Slave-In and MISO is
 * Master-In-Slave-Out (the former is sometimes called Data-Out and the latter
 * Data-In, but again we'll stick with MOSI and MISO).
 *
 * As a variation of this scheme, in a point-to-point setup between one master
 * and exactly one slave, the CS line is sometimes omitted. This is sometimes
 * called 3-pin SPI.
 *
 * Data exchange works like this: CLK runs and CS is enabled. Now the master
 * transmits on MOSI and listens on MISO while the slave does the same thing
 * just the other way around.
 *
 * That is about all there is to know about the basic operation of the bus.
 *
 * There are a number of details, that need further discussion:
 *
 * - Clock Rate: While this seems like an obvious parameter that one should be
 *   able to tweak, in reality this is not very critical: Since the operation
 *   is synchronous, the master dictates the rate of operation in any case.
 *   While there is a maximum there usually isn't a minimum rate. Tweaking
 *   clock-rate to an exact value is not very easy and might complicate the
 *   code. So it is currently not on the agenda.
 *
 * - Frame Length: Unit of a fixed number of bits that is exchanged in one
 *   transaction. This is obviously important and depends on the specific
 *   slave.
 *
 * - Number of CS lines: This may range from zero (which implies 3-wire SPI
 *   operation) to any number the port in question can support, in order to
 *   address more than one SPI slave on the same bus. If more than one
 *   chip-select line is configured, the client needs to switch the chip-select
 *   focus before sending data.
 *
 * - CS Polarity: Determines whether CS is active high or active low.
 *
 * - CLK Polarity: Does CLK idle high or low?
 *
 * - CLK Phase Delay: When set, CLK phase is delayed by one half CLK cycle and
 *   data is latched at the leading edge (is CLK Polarity is 0 - falling edge
 *   otherwise) of CLK. Otherwise data is latched at the other CLK edge.
 *
 * - Bit Order: In a frame transmit data LSB-first or MSB-first?
 *
 * The reference implementation uses GPIO Pins to implement SPI in a
 * configurable, portable, bit-banged fashion (the controller-specific code
 * only needs to expose a way to manipulate a pin's polarity). Other
 * implementations may obviously leverage hardware SPI interfaces should their
 * target controller support that.
 */
