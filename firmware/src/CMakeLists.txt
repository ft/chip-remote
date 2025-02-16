cmake_minimum_required(VERSION 3.13.1)
project(chip-remote-firmware)

include(InitialiseToolchain)
initialise_toolchain()

include(GitIntegration)

set(sources main.c chip-remote.c peripherals.c registers.c)

dt_has_chosen(with_heartbeat PROPERTY "chipremote,heartbeat")

if ("${with_heartbeat}" STREQUAL TRUE)
  message(STATUS "Enabling Heartbeat LED")
  list(APPEND sources heartbeat.c)
else()
  message(STATUS "No Heartbeat LED in hardware description!")
endif()

if (CONFIG_CR_INTERFACE_TCPIP OR CONFIG_INSTRUMENTATION_INTERFACE)
  message(STATUS "Enabling TCP/IP Server Implementation")
  list(APPEND sources server.c)
endif()

if (CONFIG_INSTRUMENTATION_INTERFACE)
  message(STATUS "Enabling Instrumentation Interface")
  list(APPEND sources native-instrumentation.c)
  target_link_libraries(app PRIVATE ufw-sx)
endif()

target_sources(app PRIVATE ${sources})
target_link_libraries(app PRIVATE ufw ufw-zephyr)

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
