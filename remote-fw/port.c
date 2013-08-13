/**
 * @file port.c
 * @brief Portable code for defining chip-ports.
 *
 * In `chip-remote', a port defines one physical connection on the remote micro
 * controller, that can be used to talk to a configurable chip. The chip-remote
 * protocol allows for multiple ports on a controller, of which each is
 * configurable with respect to the type of bus it implements.
 *
 * While the reference implementation supports this kind of configurability,
 * the protocol defines it as optional. So other implementations may just
 * decide to have on fixed port that talks - for example - SPI of a fixed
 * variety to a single chip. This reduces complexity and the need to initialise
 * the remote controller with the right set of parameters required for
 * successful operation.
 *
 * A configurable port has to be explicitly initialised before it is put to use
 * and after any configuration changes.
 *
 * Sample ports query:
 *
 * >>> PORTS
 * <<< 2
 *
 * Sample port queries:
 *
 * >>> PORT 0
 * <<< PROPERTIES CONFIGURABLE
 * >>> MORE
 * <<< LINES 8 FIXED
 * >>> MORE
 * <<< MODE SPI
 * >>> MORE
 * <<< RATE DEFAULT FIXED
 * >>> MORE
 * <<< FRAME-LENGTH 32
 * >>> MORE
 * <<< CS-LINES 5
 * >>> MORE
 * <<< CLK-POLARITY 0
 * >>> MORE
 * <<< CLK-PHASE-DELAY 0
 * >>> MORE
 * <<< BIT-ORDER LSB-FIRST
 * >>> MORE
 * <<< DONE
 *
 * >>> PORT 1
 * <<< PROPERTIES FIXED
 * >>> MORE
 * <<< LINES 2
 * >>> MORE
 * <<< MODE I2C
 * >>> MORE
 * <<< RATE DEFAULT
 * >>> MORE
 * <<< FRAME-LENGTH 8
 * >>> MORE
 * <<< BIT-ORDER MSB-FIRST
 * >>> MORE
 * <<< DONE
 *
 * Sample property change:
 *
 * <<< SET 0 CS-LINES 5
 *
 * Sample lines query:
 *
 * >>> LINES 0
 * <<< 0 CLK
 * >>> MORE
 * <<< 1 MOSI
 * >>> MORE
 * <<< 2 MISO
 * >>> MORE
 * <<< 3 CS:0
 * >>> MORE
 * <<< 4 CS:1
 * >>> MORE
 * <<< 5 CS:2
 * >>> MORE
 * <<< 6 CS:3
 * >>> MORE
 * <<< 7 CS:4
 * >>> MORE
 * <<< DONE
 *
 * Sample line assignment (switch MISO and MOSI from previous configuration):
 *
 * >>> LINE 0 1 MISO
 * <<< OK
 * >>> LINE 0 1 MOSI
 * <<< OK
 *
 * Sample port initilisation:
 *
 * >>> INIT 0
 * <<< OK
 */

#include "port.h"

size_t
cr_numofports(struct cr_port *ports)
{
    size_t i;
    for (i = 0; ports[i].lines.value != 0; ++i)
        /* NOP */;
    return i;
}
