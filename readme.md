# PROSUM BMI Wrapper

This project provides a [Basic Model Interface (BMI)](https://bmi.readthedocs.io/) wrapper around Dr Ed Rowe’s PROSUM (Plant PROduction and SUccession Model). It allows BMI-aware tools to drive PROSUM as a shared library.

**Platform:** Linux (not working on WSL)  
**Credit:** PROSUM core by Dr Ed Rowe (`src/PROSUM_module_v0.7.F90`, `src/PROSUM_main_v0.5.f90`, etc.); bmi wrapper by Ziyan (Fiona) Fang

---

## Prerequisites

- GNU Fortran (`gfortran`)  
- CMake ≥ 3.15  
- pkg-config  
- A writable `$HOME/local-bmi` (or change `CMAKE_INSTALL_PREFIX`)

---

## 1. Build & Install BMI Fortran Binding

BMI Fortran is needed to compile the wrapper’s `bmi_prosumf.f90`.

```bash
git clone https://github.com/csdms/bmi-fortran.git
cd bmi-fortran
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=$HOME/local-bmi ..
cmake --build .
cmake --install .

export PKG_CONFIG_PATH=$HOME/local-bmi/lib/pkgconfig:$PKG_CONFIG_PATH
```

---

## 2. Build & Install PROSUM Core Library

Compile PROSUM as `libprosum_core.so`. Note that we use `-ffree-line-length-none` to avoid line-truncation error.

```bash
cd src
rm -rf _build
mkdir _build && cd _build

cmake \
  -DCMAKE_INSTALL_PREFIX=$HOME/local-bmi \
  -DCMAKE_Fortran_FLAGS="-ffree-line-length-none" \
  ..

cmake --build .
cmake --install .
```

This installs:
- `libprosum_core.so` → `$HOME/local-bmi/lib`  
- Module files (`*.mod`) → `$HOME/local-bmi/include`

---

## 3. Build & Run the BMI Wrapper

Under `bmi/`, the CMakeLists builds:

- `bmi_prosumf.f90` → `libbmi_prosum.so`  
- `bmi_main.f90`  → `run_bmi_prosum`

```bash
cd ../bmi
rm -rf _build
mkdir _build && cd _build

export PKG_CONFIG_PATH=$HOME/local-bmi/lib/pkgconfig:$PKG_CONFIG_PATH
cmake -DCMAKE_INSTALL_PREFIX=$HOME/local-bmi ..
cmake --build .
```

### Run a Test

1. Create a dummy config and copy inputs:
   ```bash
   echo "dummy config" > test_config.txt
   cp ../../src/*.csv .
   ```
2. Execute:
   ```bash
   ./run_bmi_prosum test_config.txt
   ```
3. Inspect `PROSUM_outputs.csv` in `_build/` to see exposed variables (e.g., `vegetation__nitrogen_availability`).

---

Enjoy driving PROSUM through BMI-enabled workflows!