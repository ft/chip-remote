cmake_minimum_required(VERSION 3.13.1)
project(chip-remote NONE)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/libraries/ufw/cmake/modules)
include(SetupUFW)

ufw_toplevel(
  ROOT      ${CMAKE_SOURCE_DIR}/libraries/ufw
  ARTIFACTS ${CMAKE_BINARY_DIR}/artifacts)

enable_language(C)
include(InitialiseToolchain)
initialise_toolchain()

setup_ufw(libraries/ufw)

ufw_subtree_build()
message(STATUS "BOARD: ${BOARD}")
