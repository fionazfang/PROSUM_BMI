# PROSUM Core Shared Library Build Instructions

---

## Contents

* `build_prosum.sh` – Shell script to automate the CMake build and install steps.
* `CMakeLists.txt` – CMake configuration for the PROSUM core.
* `PROSUM_module*.F90`, `PROSUM_sub*.F90`, `PROSUM_main*.F90` – Fortran source files.

---

## Prerequisites 

* **Fortran compiler** (recommending `gfortran`)
* **CMake** (version ≥ 3.15)
* **pkg-config** (optional, only for BMI wrapper)

---

## Quick build & install

1. **Make the script executable** (if not already):

   ```bash
   cd $(dirname "$0")
   chmod +x build_prosum.sh
   ```

2. **Run the build script**:

   ```bash
   ./build_prosum.sh
   ```

   By default, this will:

   1. Clean any previous `_build/` directory.
   2. Create a fresh `_build/` folder and enter it.
   3. Configure the project with CMake, installing into `$HOME/local-bmi`.
   4. Compile the shared library `libprosum_core.so`.
   5. Install the library and module files into `$HOME/local-bmi`.

3. **Verify the installation**:

   ```bash
   ls $HOME/local-bmi/lib/libprosum_core.*
   ```

---

## Customizing the install prefix

To install into a different directory, set the `PROSUM_INSTALL_PREFIX` environment variable before running the script:

```bash
PROSUM_INSTALL_PREFIX=/opt/prosum ./build_prosum.sh
```

This will install the library and module files under `/opt/prosum` instead of `$HOME/local-bmi`.

---

## Next steps

* After installing the core, navigate to the **BMI wrapper** directory (`prosum/bmi`) and rebuild the wrapper so it picks up the newly installed module:

  ```bash
  cd ../bmi
  ./build_bmi_prosum.sh    # or follow that wrapper’s README
  ```
