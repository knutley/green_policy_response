#!/bin/bash

# run.sh
# Portable R pipeline runner with renv support.
# Works on Linux, WSL, and macOS.
# Ensures R environment is isolated and project dependencies are installed.

set -e  # exit on error

# ==== CONFIGURATION ====
# Determine R version to construct user lib path
R_MAJOR=$(Rscript -e 'cat(R.version$major)')
R_MINOR=$(Rscript -e 'cat(strsplit(R.version$minor, "[.]")[[1]][1])')
R_VERSION="${R_MAJOR}.${R_MINOR}"
USER_LIB="${HOME}/R/x86_64-pc-linux-gnu-library/${R_VERSION}"

# ==== STEP 0: OS-specific dependency guidance ====
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    echo "Note: If you encounter build errors, install system libraries with:"
    echo "  sudo apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev \\"
    echo "       libharfbuzz-dev libfribidi-dev build-essential pkg-config"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Note: If you encounter build errors, install system libraries with:"
    echo "  brew install curl openssl libxml2 libgit2 harfbuzz fribidi pkg-config"
    echo "  And ensure Xcode CLI tools are installed: xcode-select --install"
fi

# ==== STEP 1: Ensure Rscript is available ====
if ! command -v Rscript &> /dev/null; then
    echo "Error: Rscript is not installed or not in your PATH."
    exit 1
fi

# ==== STEP 2: Ensure user R library exists and is writable ====
if [ ! -d "$USER_LIB" ]; then
    echo "Creating local R library at $USER_LIB"
    mkdir -p "$USER_LIB"
fi

if [ ! -w "$USER_LIB" ]; then
    echo "Error: R user library at $USER_LIB is not writable."
    exit 1
fi

export R_LIBS_USER="$USER_LIB"

# ==== STEP 3: Install renv if not available ====
echo "Checking for renv..."
Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org", lib = Sys.getenv("R_LIBS_USER"))'

# ==== STEP 4: Initialise or activate renv ====
if [ ! -f "renv/activate.R" ]; then
    echo "Setting up renv virtual environment (activate.R not found)..."
    Rscript -e 'renv::init(bare = TRUE, force = TRUE)'
fi

echo "Using renv environment..."


# ==== STEP 5: Install dependencies using renv ====
echo "Installing required R packages with renv..."
Rscript -e 'renv::activate(); source("requirements.R"); renv::snapshot(confirm = FALSE)'

# ==== STEP 6: Run the requested pipeline steps ====
if [ "$#" -eq 0 ]; then
    echo "Usage: $0 [filter] [translate] [topic]"
    echo "Runs the selected parts of the green_policy_response pipeline."
    exit 1
fi

for step in "$@"; do
    case "$step" in
        filter)
            echo "Running filtering step..."
            Rscript -e 'renv::activate(); source("src/main.R")' filter
            ;;
        translate)
            echo "Running translation step..."
            Rscript -e 'renv::activate(); source("src/main.R")' translate
            ;;
        topic)
            echo "Running topic modelling step..."
            Rscript -e 'renv::activate(); source("src/main.R")' topic
            ;;
        *)
            echo "Warning: Unknown step '$step'. Valid options are: filter, translate, topic."
            ;;
    esac
done
