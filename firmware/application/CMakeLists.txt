cmake_minimum_required(VERSION 3.13.1)
project(chip-remote-firmware)

include(InitialiseToolchain)
initialise_toolchain()

include(GitIntegration)

target_sources(app PRIVATE common.c)

if (${BOARD} STREQUAL nucleo_f767zi)
  target_sources(app PRIVATE
    ifc/bb/spi.c
    init-usb-cdc-acm.c)
elseif (${BOARD} STREQUAL native_posix_64)
  target_sources(app PRIVATE
    ifc/text/spi.c
    init-posix.c)
else()
  message(WARNING "Unsupported board: ${BOARD}")
endif()

target_link_libraries(app PRIVATE ufw chip-remote-protocol)

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
