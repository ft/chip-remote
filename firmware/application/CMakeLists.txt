cmake_minimum_required(VERSION 3.13.1)
project(chip-remote-firmware)

include(InitialiseToolchain)
initialise_toolchain()

include(GitIntegration)

set(native_boards native_posix_64)

set(common_sources
  chip-remote.c
  peripherals.c
  registers.c)

set(board_common_sources
  heartbeat.c)

set(native_common_sources
  native-instrumentation.c
  resizable-buffer.c)

if (${BOARD} IN_LIST native_boards)
  target_sources(app PRIVATE ${common_sources} ${native_common_sources} main.c)
else()
  target_sources(app PRIVATE ${common_sources} ${board_common_sources} main.c)
endif()

target_link_libraries(app PRIVATE ufw ufw-zephyr)
if (${BOARD} MATCHES "^native")
  target_link_libraries(app PRIVATE ufw-sx)
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
