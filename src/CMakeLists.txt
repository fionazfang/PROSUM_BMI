cmake_minimum_required(VERSION 3.15)
project(prosum_core Fortran)

include(GNUInstallDirs)

set(PROSUM_SOURCES
  PROSUM_module_v0.7.F90
  PROSUM_sub_v0.8.f90
  PROSUM_main_v0.5.f90
)

add_library(prosum_core SHARED ${PROSUM_SOURCES})

install(TARGETS prosum_core
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE   DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

file(GLOB PROSUM_MODULE_FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/*.mod"
)

install(
  FILES ${PROSUM_MODULE_FILES}
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)


