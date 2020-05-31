cmake_minimum_required(VERSION 3.13.1)

find_package(Zephyr HINTS $ENV{ZEPHYR_BASE})
project(chip-remote-firmware)

target_sources(app PRIVATE main.c)

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/ufw/cmake/modules)
include(Libtap)

set(PROJECT_TARGET_CPU native)
add_libtap(libtap)
add_subdirectory(ufw)
add_subdirectory(protocol)
add_subdirectory(tests)

target_link_libraries(app PRIVATE ufw)
