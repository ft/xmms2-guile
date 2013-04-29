project(xmms2-guile)
cmake_minimum_required(VERSION 2.8.0)

# Check what the system's compiler can do and take notes.
include(${CMAKE_SOURCE_DIR}/b/cmake/macros-compiler-knows.cmake)
include(${CMAKE_SOURCE_DIR}/b/cmake/compiler-setup.cmake)

# Check for build-dependencies.
include(${CMAKE_SOURCE_DIR}/b/cmake/build-deps.cmake)

# Since we know about our build-deps now, we can setup the compilers include
# directories now.
include(${CMAKE_SOURCE_DIR}/b/cmake/compiler-includes.cmake)
include(${CMAKE_SOURCE_DIR}/b/cmake/link-directories.cmake)

configure_file(
  ${CMAKE_SOURCE_DIR}/c/config.h.in
  ${CMAKE_BINARY_DIR}/c/config.h)

add_subdirectory(c)

add_custom_target(
  uninstall
  COMMAND ${PERL} ${CMAKE_SOURCE_DIR}/b/bin/uninstall.pl)
