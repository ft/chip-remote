project(cdce-remote)
cmake_minimum_required(VERSION 2.8.0)

file(GLOB SOURCES *.c)
add_executable(cdce-remote ${SOURCES})

set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wall -Wextra -Wdeclaration-after-statement")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wformat -Wformat-nonliteral -Wformat-security")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wformat-y2k -Wmissing-braces -Wmissing-declarations")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wmissing-field-initializers -Wmissing-prototypes")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-multichar -Wold-style-definition -Wunused")

find_package(Guile 2.0 REQUIRED)
INCLUDE_DIRECTORIES(${GUILE_INCLUDE_DIR})
TARGET_LINK_LIBRARIES(cdce-remote ${GUILE_LIBRARIES})

install(TARGETS cdce-remote
  DESTINATION bin)
install(DIRECTORY scheme/ti
  DESTINATION share/cdce-remote
  FILES_MATCHING PATTERN "*.scm")

file(GLOB scm scheme/*.scm)
install(FILES ${scm}
  DESTINATION share/cdce-remote)

enable_testing()
add_subdirectory(tests)

add_custom_target(
  inbuildexecscript
  sh ${CMAKE_SOURCE_DIR}/mibes ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
