cmake_minimum_required(VERSION 3.13.1)
project(chip-remote-firmware)

include(GitIntegration)

target_sources(app PRIVATE main.c)

if (${BOARD} STREQUAL nucleo_f767zi)
  target_sources(app PRIVATE init-usb-cdc-acm.c)
elseif (${BOARD} STREQUAL native_posix_64)
  target_sources(app PRIVATE init-posix.c)
else()
  message(WARNING "Unsupported board: ${BOARD}")
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/ufw/cmake/modules)
include(InitialiseToolchain)
initialise_toolchain()
target_link_libraries(app PRIVATE ufw chip-remote-protocol)

if (${BOARD} MATCHES "^native")
  gitint_install(
    BUILD_VARIANT generic
    LATEST_ARTIFACTS chip-remote
    FILES
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.exe)
else()
  gitint_install(
    BUILD_VARIANT generic
    LATEST_ARTIFACTS chip-remote
    FILES
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.elf
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.bin
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.hex
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.dts
    ${CMAKE_BINARY_DIR}/zephyr/zephyr.map)
endif()
