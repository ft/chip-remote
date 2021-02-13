cmake_minimum_required(VERSION 3.13.1)

find_package(Zephyr HINTS $ENV{ZEPHYR_BASE})
project(chip-remote-firmware)

target_sources(app PRIVATE
  src/main.c)

if (${BOARD} STREQUAL nucleo_f767zi)
  target_sources(app PRIVATE src/init-usb-cdc-acm.c)
elseif (${BOARD} STREQUAL native_posix_64)
  target_sources(app PRIVATE src/init-posix.c)
else()
  message(WARNING "Unsupported board: ${BOARD}")
endif()

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/ufw/cmake/modules)
include(InitialiseToolchain)
initialise_toolchain()

if (${ARCH} STREQUAL posix)
  set(PROJECT_TARGET_CPU native)
  include(Libtap)
  add_libtap(libtap)
endif()

add_subdirectory(ufw)
add_subdirectory(protocol)
add_subdirectory(tests)

target_link_libraries(app PRIVATE ufw chip-remote-protocol)
