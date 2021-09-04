cmake_minimum_required (VERSION 3.9)

include(InitialiseToolchain)
initialise_toolchain()

add_library(sx-parser STATIC
  sx-parser.c
  sx-utils.c)

target_include_directories(sx-parser
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

target_link_libraries(sx-parser PUBLIC ufw)

set_target_cpu(sx-parser)
MakeStrictCompilerC(sx-parser)
