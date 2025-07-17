prefix=@CMAKE_INSTALL_PREFIX@
exec_prefix=${prefix}
libdir=${exec_prefix}/${CMAKE_INSTALL_LIBDIR}
includedir=${exec_prefix}/${CMAKE_INSTALL_INCLUDEDIR}

Name: bmi_prosum
Description: BMI wrapper for the PROSUM model
Version: @PROJECT_VERSION@
Libs: -L${libdir} -lbmi_prosum
Cflags: -I${includedir}
