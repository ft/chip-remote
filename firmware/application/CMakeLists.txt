cmake_minimum_required(VERSION 3.13.1)
project(chip-remote-firmware)

include(InitialiseToolchain)
initialise_toolchain()

include(GitIntegration)

set(common_sources
  registers.c)

set(board_common_sources
  # ifc/bb/spi.c
  # ifc/os/i2c.c
  # ifc/os/spi.c
)

set(native_common_sources
  # ifc/text/spi.c
)

if (${BOARD} STREQUAL nucleo_f767zi)
  target_sources(app PRIVATE
    ${common_sources}
    ${board_common_sources}
    init-usb-cdc-acm.c)
elseif (${BOARD} STREQUAL nucleo_l476rg)
  target_sources(app PRIVATE
    ${common_sources}
    ${board_common_sources}
    init-uart.c)
elseif (${BOARD} STREQUAL native_posix_64)
  target_sources(app PRIVATE
    ${common_sources}
    ${native_common_sources}
    init-posix.c)
else()
  message(WARNING "Unsupported board: ${BOARD}")
endif()

target_link_libraries(app PRIVATE ufw)
if (${BOARD} MATCHES "^native")
  target_link_libraries(app PRIVATE sx-parser)
endif()

if (${BOARD} MATCHES "^native")
  gitint_install(
    LATEST_ARTIFACTS
    FILES
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.exe)
else()
  gitint_install(
    LATEST_ARTIFACTS
    FILES
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.elf
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.bin
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.hex
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.dts
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.map)
endif()
