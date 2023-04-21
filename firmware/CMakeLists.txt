# Top-level chip-remote firmware cmake configuration. This firmware is based on
# the Zephyr real time operating system and the “ufw” (microframework) embedded
# support library. It uses that libary's sister tool “mmh” for build orchestra-
# tion. With mmh available in your execution path, building chip-remote's firm-
# ware for all available targets in all specified configurations only requires
# executing this one command: “mmh”. The build results will be organised into
# the “build/artifacts” directory afterwards. In mmh terms, this is a zephyr
# based system build, of which the details a specified in the “system.yaml”
# file.
#
# The comments in this file outline a couple of details about the interaction
# of the zephyr's cmake based build system and ufw's cmake extensions.

cmake_minimum_required(VERSION 3.13.1)
# Not initialising languages right away. We'll do some setup and then kick off
# the toolchain setup manually. Note, that this build is driven by ufw and mmh,
# as zephyr-based system builds. Zephyr doesn't use cmake's toolchain file ap-
# proach to configure toolchains, but has its own implementation.
project(chip-remote NONE)

# Initialising ufw's cmake extensions.
list(APPEND CMAKE_MODULE_PATH
  ${CMAKE_CURRENT_SOURCE_DIR}/libraries/ufw/cmake/modules)
include(SetupUFW)

# This sets up some internal state for ufw. In the case of zephyr-based builds,
# it also sets up the application source directory and the kernel directory.
# These are specified in “system.yaml” which is used by mmh to assemble a com-
# plicated cmake command line to configure the build. When everything is set
# up, this also triggers the zephyr package cmake code. This will already take
# care of assembling and processing Kconfig and devicetree data, based on the
# choice of zephyr-board-definition.
#
# Zephyr's cmake package code also sets up the generic “app” target, that will
# eventually become the application. This will be configured in “application”
# subdirectory, which — again — was specified in “system.yaml”.
ufw_toplevel(
  ROOT      ${CMAKE_SOURCE_DIR}/libraries/ufw
  ARTIFACTS ${CMAKE_BINARY_DIR}/artifacts)

# Now initialise the selected toolchain. The initialise_toolchain() step is not
# strictly required, since we could just go with zephyr's default setup, but I
# like to enable a couple more warnings than zephyr does be default. Also, if
# code wants to use any of the compiler feature discovery parts from ufw, this
# needs to run to populate the detection information.
enable_language(C)
enable_language(ASM)
include(InitialiseToolchain)
initialise_toolchain()

# Set up nanopb and specify a target that generated nanopb code for the proto-
# col spec in the “protocol” top-level subdirectory.
include(AddNanoPB)
define_nanopb_protoc(zephyr/modules/nanopb)

# This is ufw's way of saying “add_subdirectory()”. It takes care of some de-
# tails and then, indeed, calls that function. We're doing this fairly early
# on, so all the libraries and tests can depend on ufw.
setup_ufw(libraries/ufw)
add_subdirectory(libraries/libchip-remote)

# This dispatches to the source directories specified in “system.yaml”. In the
# case of zephyr builds (like here), this basically calls “add_subdirectory()”
# for the specified application source dir. This also takes care of some de-
# qtails.
ufw_subtree_build()

# If we're on a native build, also build the s-expression module and the tests.
# The former is used to implement native-build firmware instrumentation, and
# the latter implements unit tests.
message(STATUS "BOARD: ${BOARD}")
if ("${BOARD}" MATCHES "^native")
  add_subdirectory(libraries/libsx-parser)
  add_subdirectory(tests)
endif()
