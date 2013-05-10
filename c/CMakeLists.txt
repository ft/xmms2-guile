cmake_minimum_required(VERSION 2.8.0)

include_directories(${GUILE_INCLUDE_DIR})

if (NOT ("${GUILE_CFLAGS}" STREQUAL ""))
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${GUILE_CFLAGS}")
endif()

set(SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/init.c
            ${CMAKE_CURRENT_SOURCE_DIR}/primitive-connect.c
            ${CMAKE_CURRENT_SOURCE_DIR}/primitive-playback.c
            ${CMAKE_CURRENT_SOURCE_DIR}/primitive-synchronous.c
            ${CMAKE_CURRENT_SOURCE_DIR}/primitive-value.c
            ${CMAKE_CURRENT_SOURCE_DIR}/type-connection.c
            ${CMAKE_CURRENT_SOURCE_DIR}/type-result.c
            ${CMAKE_CURRENT_SOURCE_DIR}/type-value.c
            ${CMAKE_CURRENT_SOURCE_DIR}/utils.c)

add_library(xmms2-guile SHARED ${SOURCES})
target_link_libraries(xmms2-guile ${XMMS2_CLIENT_LDFLAGS})
