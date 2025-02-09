cmake_minimum_required(VERSION 3.13.1)
project(chip-remote-firmware)

include(InitialiseToolchain)
initialise_toolchain()

include(GitIntegration)

set(sources main.c chip-remote.c peripherals.c registers.c)

dt_has_chosen(with_heartbeat PROPERTY "chipremote,heartbeat")
if ("${with_heartbeat}" STREQUAL TRUE)
  message(STATUS "Heartbeat LED found in devicetree")
  list(APPEND sources heartbeat.c)
endif()

dt_has_chosen(with_instrumentation PROPERTY "chipremote,instr-ifc")
if ("${with_instrumentation}" STREQUAL TRUE)
  message(STATUS "Instrumentation interface found in devicetree")
  list(APPEND sources native-instrumentation.c server.c)
endif()

target_sources(app PRIVATE ${sources})
target_link_libraries(app PRIVATE ufw ufw-zephyr)

if ("${with_instrumentation}" STREQUAL TRUE)
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
