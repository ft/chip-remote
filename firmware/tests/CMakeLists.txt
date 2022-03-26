cmake_minimum_required(VERSION 3.9)

list(APPEND test_names
  t-utilities
  t-parse-string
  t-sx-parser)

set(test_prgs)
list(APPEND test_prgs)
foreach (tst ${test_names})
  add_executable(${tst} ${tst}.c)
  if ("${CMAKE_BUILD_TYPE}" STREQUAL Debug)
    ufw_toolchain(${tst} FEATURES sanitize-address sanitize-undefined-behaviour)
  endif()
  ufw_toolchain(${tst} FEATURES sanitize-address
                                sanitize-integer
                                sanitize-undefined-behaviour)
  target_link_libraries(${tst} PRIVATE ufw-tap)
  MakeStrictCompilerC(${tst})
  set_target_cpu(${tst})
  list(APPEND test_prgs $<TARGET_FILE:${tst}>)
endforeach()

target_link_libraries(t-parse-string PRIVATE chip-remote-protocol)
target_link_libraries(t-sx-parser PRIVATE sx-parser)
target_link_libraries(t-utilities PRIVATE chip-remote-protocol)

add_test(
  NAME chip-remote-firmware-tests
  COMMAND prove --verbose --merge --color --exec "" ${test_prgs})
