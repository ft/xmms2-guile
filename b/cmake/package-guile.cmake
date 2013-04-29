cmake_minimum_required(VERSION 2.8.0)

if (NOT GUILE_LIBRARY)

  message(STATUS "Setting up GNU Guile...")

  find_program(GUILE guile)
  if (GUILE STREQUAL "GUILE-NOTFOUND")
    message(FATAL_ERROR "\nGuile binary not found; Giving up.\n")
  else()
    message(STATUS "  Guile binary found: ${GUILE}")
    set(GOT_GUILE "YES")
  endif()

  find_program(GUILD guild)
  if (GUILD STREQUAL "GUILD-NOTFOUND")
    message(FATAL_ERROR "\nGuild binary not found; Giving up.\n")
  else()
    message(STATUS "  Guild binary found: ${GUILD}")
    set(GOT_GUILD "YES")
  endif()

  execute_process(
    COMMAND guile -c "(format #t \"~a\" (major-version))"
    OUTPUT_VARIABLE GUILE_MAJOR_VERSION)
  message(STATUS "  Major version: ${GUILE_MAJOR_VERSION}")

  execute_process(
    COMMAND guile -c "(format #t \"~a\" (minor-version))"
    OUTPUT_VARIABLE GUILE_MINOR_VERSION)
  message(STATUS "  Minor version: ${GUILE_MINOR_VERSION}")

  execute_process(
    COMMAND guile -c "(format #t \"~a\" (micro-version))"
    OUTPUT_VARIABLE GUILE_MICRO_VERSION)
  message(STATUS "  Micro version: ${GUILE_MICRO_VERSION}")

  if ((${GUILE_MAJOR_VERSION} LESS 2) OR
      ((${GUILE_MAJOR_VERSION} EQUAL 2) AND
       (${GUILE_MINOR_VERSION} EQUAL 0) AND
       (${GUILE_MICRO_VERSION} LESS 5)))
    message(FATAL_ERROR "\nRequiring at least Guile version 2.0.5\n")
  endif()

  execute_process(
    COMMAND guile -c "(format #t \"~a\" (%global-site-dir))"
    OUTPUT_VARIABLE GUILE_GLOBAL_SITE_DIR)
  message(STATUS "  Site dir            : ${GUILE_GLOBAL_SITE_DIR}")

  execute_process(
    COMMAND guile -c "(format #t \"~a\" (%site-dir))"
    OUTPUT_VARIABLE GUILE_SITE_DIR)
  message(STATUS "  Site dir (versioned): ${GUILE_SITE_DIR}")

  execute_process(
    COMMAND guile -c "(format #t \"~a\"
                              (assq-ref %guile-build-info 'pkgincludedir))"
    OUTPUT_VARIABLE GUILE_PKG_INCLUDE_DIR)

  if (EXISTS "${GUILE_PKG_INCLUDE_DIR}/libguile.h")
    set(GUILE_INCLUDE_DIR "${GUILE_PKG_INCLUDE_DIR}")
  elseif (EXISTS "${GUILE_PKG_INCLUDE_DIR}/${GUILE_MAJOR_VERSION}.${GUILE_MINOR_VERSION}/libguile.h")
    set(GUILE_INCLUDE_DIR "${GUILE_PKG_INCLUDE_DIR}/${GUILE_MAJOR_VERSION}.${GUILE_MINOR_VERSION}")
  else ()
    message(FATAL_ERROR "\nCould not find Guile include directory.\n")
  endif()
  set(GUILE_INCLUDE_DIR "${GUILE_INCLUDE_DIR}"
      CACHE FILEPATH "Guile include directory")
  message(STATUS "  Include directory   : ${GUILE_INCLUDE_DIR}")

  execute_process(
    COMMAND guile -c "(format #t \"~a\"
                              (assq-ref %guile-build-info 'libdir))"
    OUTPUT_VARIABLE GUILE_LIB_DIR)

  execute_process(
    COMMAND guile -c "(format #t \"~a\"
                              (assq-ref %guile-build-info 'pkglibdir))"
    OUTPUT_VARIABLE GUILE_PKG_LIB_DIR)

  set(GUILE_LIBRARY_DIRECTORIES
    "${GUILE_PKG_LIB_DIR}"
    "${GUILE_LIB_DIR}"
    "${GUILE_PKG_LIB_DIR}/${GUILE_MAJOR_VERSION}.${GUILE_MINOR_VERSION}"
    "${GUILE_LIB_DIR}/${GUILE_MAJOR_VERSION}.${GUILE_MINOR_VERSION}")

  find_library(
    GUILE_LIBRARY
    NAMES guile
    PATHS ${GUILE_LIBRARY_DIRECTORIES})

  if (NOT GUILE_LIBRARY)
    find_library(
      GUILE_LIBRARY
      NAMES "guile-${GUILE_MAJOR_VERSION}.${GUILE_MINOR_VERSION}"
      PATHS ${GUILE_LIBRARY_DIRECTORIES})
    if (NOT GUILE_LIBRARY)
      message(FATAL_ERROR "\nCould not find Guile library.\n")
    endif()
    set(GUILE_INTERNAL_LIB "guile-${GUILE_MAJOR_VERSION}.${GUILE_MINOR_VERSION}")
  else()
    set(GUILE_INTERNAL_LIB "guile")
  endif()
  message(STATUS "  Library found       : ${GUILE_LIBRARY}")

  execute_process(
    COMMAND guile -c "(format #t \"~a\"
                              (assq-ref %guile-build-info 'CFLAGS))"
    OUTPUT_VARIABLE GUILE_CFLAGS)
  string(STRIP "${GUILE_CFLAGS}" GUILE_CFLAGS)
  set(GUILE_CFLAGS "${GUILE_CFLAGS}" CACHE STRING "Guile CFLAGS")
  message(STATUS "  CFLAGS: ${GUILE_CFLAGS}")

  execute_process(
    COMMAND guile -c "(format #t \"~a\"
                              (assq-ref %guile-build-info 'LIBS))"
    OUTPUT_VARIABLE GUILE_LIBS)
  string(STRIP "${GUILE_LIBS}" GUILE_LIBS)
  set(GUILE_LIBS "${GUILE_LIBS} -l${GUILE_INTERNAL_LIB}"
      CACHE STRING "Guile linker options")
  message(STATUS "  LIBS: ${GUILE_LIBS}")

endif()
