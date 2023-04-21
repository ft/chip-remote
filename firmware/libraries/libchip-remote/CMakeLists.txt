cmake_minimum_required (VERSION 3.9)

include(InitialiseToolchain)
initialise_toolchain()

set(CHIPREMOTE_PROTO_DIR "${CMAKE_SOURCE_DIR}/../protocol")
set(GENERATED ${CMAKE_CURRENT_BINARY_DIR}/generated)
set(chipremote_pb ${GENERATED}/chip-remote.pb.c)

message(STATUS "ChipRemote Protocol Directory: ${CHIPREMOTE_PROTO_DIR}")
message(STATUS "NanoPB Generator: ${NANOPB_PROTOC}")
message(STATUS "NanoPB Generated Code: ${chipremote_pb}")

# Why are we doing this here? Cmake has this limitation about custom rules: Any
# rule that needs to be used with a target, cannot be specified in a sub-direc-
# tory of the project, entered with “add_subdirectory()”. And since the “app”
# target was created here, by zephyr's package code, this needs to be done
# here, if we want this as a dependency for the “app” target, even if we name
# the generated file as a source file of that target in another directory.
#
# Maybe the generated protobuf code should really be part of “libchip-remote”,
# in which case we could move this to its subdirectory.
add_custom_command(
  COMMAND ${NANOPB_PROTOC} -I${CHIPREMOTE_PROTO_DIR}
                           --nanopb_out=${GENERATED}
                           chip-remote.proto
  DEPENDS ${CHIPREMOTE_PROTO_DIR}/chip-remote.proto
  OUTPUT ${chipremote_pb})

add_library(chip-remote-protocol STATIC
  commands.c
  command-address.c
  command-bye.c
  command-capabilities.c
  command-focus.c
  command-hi.c
  command-init.c
  command-ports.c
  command-set.c
  command-transmit.c
  command-version.c
  command-fw-version.c
  cr-process.c
  cr-protocol.c
  cr-port.c
  cr-utilities.c
  parse-string.c
  ${chipremote_pb})

target_include_directories(chip-remote-protocol
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

target_compile_definitions(chip-remote-protocol PUBLIC
  CONFIG_MAX_PROTOCOL_TOKENS=${CONFIG_MAX_PROTOCOL_TOKENS})

target_link_libraries(chip-remote-protocol
  PUBLIC ufw protobuf-nanopb-static)

set_target_cpu(chip-remote-protocol)
MakeStrictCompilerC(chip-remote-protocol)
