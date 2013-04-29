include(FindPkgConfig)

pkg_check_modules(XMMS2_CLIENT xmms2-client REQUIRED)
include(${CMAKE_SOURCE_DIR}/b/cmake/package-guile.cmake)
