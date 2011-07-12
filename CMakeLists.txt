project(cdce-remote)
cmake_minimum_required(VERSION 2.6.0)

file(GLOB SOURCES *.c)
add_executable(cdce-remote ${SOURCES})

set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

find_package(Guile 2.0 REQUIRED)
INCLUDE_DIRECTORIES(${GUILE_INCLUDE_DIR})
TARGET_LINK_LIBRARIES(cdce-remote ${GUILE_LIBRARIES})
