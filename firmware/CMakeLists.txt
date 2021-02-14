cmake_minimum_required(VERSION 3.13.1)
project(generic-h7-system NONE)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/libraries/ufw/cmake/modules)
include(SetupUFW)

ufw_toplevel(
  ROOT      ${CMAKE_SOURCE_DIR}/libraries/ufw
  ARTIFACTS ${CMAKE_BINARY_DIR}/artifacts)

macro(add_chip_remote variant)
  ufw_add_zephyr(chip-remote-${variant}
    KERNEL      ${CMAKE_SOURCE_DIR}/zephyr/kernel
    APPLICATION ${CMAKE_SOURCE_DIR}/application
    MODULE_ROOT ${CMAKE_SOURCE_DIR}/zephyr/modules
    BUILDTYPES  Debug Release
    ${ARGN})
endmacro()

add_chip_remote(arm
  TOOLCHAINS  gnuarmemb:/usr
  MODULES     arm-cmsis hal-stm32
  BOARDS      #stm32h747i_disco_m7
              nucleo_f767zi)

add_chip_remote(native
  TOOLCHAINS  host
  BOARDS      native_posix_64)

ufw_recursive_dispatch()
enable_language(C)
enable_language(CXX)
enable_language(ASM)
include(InitialiseToolchain)
initialise_toolchain()
ufw_subtree_build()

setup_ufw(libraries/ufw)
add_subdirectory(libraries/libchip-remote)
add_subdirectory(application)
