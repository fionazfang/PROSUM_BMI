cmake_minimum_required(VERSION 3.15)
project(bmi_prosum Fortran)

include(GNUInstallDirs)

# 1. Find the Fortran BMI binding
find_package(PkgConfig REQUIRED)
pkg_check_modules(BMIF REQUIRED bmif)

# 2. Generate a pkg-config file for bmi_prosum
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/bmi_prosum.pc.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/bmi_prosum.pc
  @ONLY
)

# 3. Build the shared BMI wrapper library
add_library(bmi_prosum SHARED
  bmi_prosumf.f90    
)
target_include_directories(bmi_prosum PRIVATE
  ${BMIF_INCLUDE_DIRS}                         # bmif_2_0.mod
  ${CMAKE_CURRENT_SOURCE_DIR}/../src/_build    # prosum_module.mod
)
target_compile_options(bmi_prosum PRIVATE ${BMIF_CFLAGS_OTHER})
target_link_libraries(bmi_prosum PRIVATE
  prosum_core                   
  ${BMIF_LIBRARIES}
)

# 4. Build the CLI driver
add_executable(run_bmi_prosum bmi_main.f90)
target_link_libraries(run_bmi_prosum PRIVATE bmi_prosum)

# 5. Install everything
install(TARGETS bmi_prosum run_bmi_prosum
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
install(FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/bmi_prosum.mod
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/bmi_prosum.pc
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
)
