cmake_minimum_required (VERSION 3.9)

include(InitialiseToolchain)
initialise_toolchain()

add_library(chip-remote-protocol STATIC
  commands.c
  command-address.c
  command-bye.c
  command-features.c
  command-hi.c
  command-init.c
  command-ports.c
  command-set.c
  command-transmit.c
  command-version.c
  command-fw-version.c
  cr-process.c
  cr-port.c
  cr-utilities.c
  parse-string.c)

target_include_directories(chip-remote-protocol
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

target_link_libraries(chip-remote-protocol PUBLIC ufw)

set_target_cpu(chip-remote-protocol)
MakeStrictCompilerC(chip-remote-protocol)
