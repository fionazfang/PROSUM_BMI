set -euo pipefail

# Default installation prefix (can be overridden by PROSUM_INSTALL_PREFIX)
INSTALL_PREFIX=${PROSUM_INSTALL_PREFIX:-"$HOME/local-bmi"}

# Build directory name
BUILD_DIR="_build"

echo "Cleaning old build artifacts (if any)..."
rm -rf "${BUILD_DIR}" CMakeCache.txt CMakeFiles

echo "Creating build directory: ${BUILD_DIR}"
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

echo "Configuring with CMake (install prefix: ${INSTALL_PREFIX})..."
cmake -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" ..

echo "Building PROSUM core..."
cmake --build .

echo "Installing into ${INSTALL_PREFIX}..."
cmake --install .

echo "Done! Installed PROSUM core to: ${INSTALL_PREFIX}"
