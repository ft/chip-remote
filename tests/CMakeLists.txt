cmake_minimum_required(VERSION 2.8.0)

file(GLOB TESTS scm/*.t)
foreach (test ${TESTS})
  get_filename_component(test_name ${test} NAME_WE)
  add_test(
    NAME ${test_name}
    COMMAND ${CMAKE_BINARY_DIR}/cdce-remote --no-auto-compile -s ${test})
  set_property(
    TEST ${test_name}
    PROPERTY ENVIRONMENT GUILE_LOAD_PATH=${CMAKE_SOURCE_DIR}/scheme)
endforeach()
