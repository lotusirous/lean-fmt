set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

default: build

# Build all targets
build:
  cabal build

# Run unit + property tests
test:
  cabal test

# Print the path to the built executable
print-bin:
  cabal list-bin exe:lean-fmt

compile-all: compile

# Build and copy a deployable binary to ./bin/lean-fmt
compile:
  mkdir -p bin
  cabal build exe:lean-fmt
  BIN="$(cabal list-bin exe:lean-fmt)"; install -m 755 "$BIN" "bin/lean-fmt"
  echo "Wrote bin/lean-fmt"


# Convenience: run the formatter (stdin by default)
run *args:
  cabal run lean-fmt -- {{args}}

# Clean build artifacts
clean:
  cabal clean

