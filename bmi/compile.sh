gfortran -g -O0 \
../src/PROSUM_module_new.o \
../src/PROSUM_sub_v0.8.o \
bmi_prosumf.o \
bmi_main.o \
-L ../../bmi-fortan/lib/libbmif.a \
-o run_bmiprosumf.exe
