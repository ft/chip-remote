cmake_minimum_required(VERSION 2.8.0)

file(
  GLOB_RECURSE TESTS
  RELATIVE ${CMAKE_SOURCE_DIR}/tests/scm
  scm/*.t)
list(SORT TESTS ${TESTS})
foreach (test ${TESTS})
  get_filename_component(test_path ${test} PATH)
  if ("${test_path}" STREQUAL "")
    set(test_path "/${test_path}")
  else()
    set(test_path "/${test_path}/")
  endif()
  get_filename_component(test_name ${test} NAME_WE)
  add_test(
    NAME "scm${test_path}${test_name}"
    COMMAND ${CMAKE_BINARY_DIR}/chip-remote --no-auto-compile
                                -s ${CMAKE_SOURCE_DIR}/tests/scm/${test})
  set_property(
    TEST "scm${test_path}${test_name}"
    PROPERTY ENVIRONMENT GUILE_LOAD_PATH=${CMAKE_SOURCE_DIR}/scheme)
endforeach()

file(
  GLOB_RECURSE TESTS
  RELATIVE ${CMAKE_SOURCE_DIR}/tests/sim
  sim/*.scm)
list(SORT TESTS ${TESTS})
foreach (test ${TESTS})
  get_filename_component(test_path ${test} PATH)
  if ("${test_path}" STREQUAL "")
    set(test_path "/${test_path}")
  else()
    set(test_path "/${test_path}/")
  endif()
  get_filename_component(test_name ${test} NAME_WE)
  add_test(
    NAME "sim${test_path}${test_name}"
    COMMAND perl ${CMAKE_SOURCE_DIR}/devsimu/boardsimu -f
            ${CMAKE_BINARY_DIR}/chip-remote --no-auto-compile
                                -s ${CMAKE_SOURCE_DIR}/tests/sim/${test})
  set_property(
    TEST "sim${test_path}${test_name}"
    PROPERTY ENVIRONMENT GUILE_LOAD_PATH=${CMAKE_SOURCE_DIR}/scheme)
endforeach()
