cmake_minimum_required(VERSION 3.13.1)

find_package(Zephyr HINTS $ENV{ZEPHYR_BASE})
project(chip-remote-firmware)

target_sources(app PRIVATE
  src/main.c)

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
