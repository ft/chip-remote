cmake_minimum_required(VERSION 3.9)

list(APPEND test_names
  t-parse-string)

set(test_prgs)
list(APPEND test_prgs)
foreach (tst ${test_names})
  add_executable(${tst} ${tst}.c)
  if ("${CMAKE_BUILD_TYPE}" STREQUAL Debug)
    ufw_toolchain(${tst} FEATURES sanitize-address sanitize-undefined-behaviour)
  endif()
  target_link_libraries(${tst} PRIVATE chip-remote-protocol ufw-tap)
  MakeStrictCompilerC(${tst})
  MakeFatalCompilerC(${tst})
  set_target_cpu(${tst})
  list(APPEND test_prgs $<TARGET_FILE:${tst}>)
endforeach()

add_test(
  NAME chip-remote-firmware-tests
  COMMAND prove --verbose --merge --color --exec "" ${test_prgs})
