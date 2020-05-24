cmake_minimum_required (VERSION 3.9)
project(chip-remote-firmware C)

if ((NOT CMAKE_BUILD_TYPE) OR ("${CMAKE_BUILD_TYPE}" STREQUAL ""))
  set(CMAKE_BUILD_TYPE "debug")
endif()

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/ufw/cmake/modules)
include(Libtap)

add_libtap(libtap)
add_subdirectory(ufw)
add_subdirectory(protocol)
add_subdirectory(tests)
