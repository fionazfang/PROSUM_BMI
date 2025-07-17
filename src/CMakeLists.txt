cmake_minimum_required(VERSION 3.15)
project(prosum_core Fortran)

include(GNUInstallDirs)

set(PROSUM_SOURCES
  PROSUM_module_new.F90
  PROSUM_sub_v0.8.f90
  PROSUM_main_v0.5.f90
)

add_library(prosum_core SHARED ${PROSUM_SOURCES})

install(TARGETS prosum_core
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE   DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
install(FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/*.mod
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
