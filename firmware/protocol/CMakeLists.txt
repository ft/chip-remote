cmake_minimum_required (VERSION 3.9)

add_library(chip-remote-protocol STATIC
  commands.c
  command-bye.c
  command-hi.c
  parse-string.c)

target_include_directories(chip-remote-protocol
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

target_link_libraries(chip-remote-protocol PUBLIC ufw)

set_target_cpu(chip-remote-protocol)
MakeStrictCompilerC(chip-remote-protocol)
