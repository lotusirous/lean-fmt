#!/usr/bin/env bash
set -euo pipefail

# Simple local releaser for lean-fmt, using GitHub CLI.
# - Creates a release for the given tag (or updates it if it already exists)
# - Uploads macOS and Linux binaries, similar to a tiny goreleaser.
#
# Requirements:
#   - gh (GitHub CLI) is installed and authenticated: gh auth login
#   - You are in a git repo with a GitHub remote
#   - Binaries already built, e.g.:
#       just compile          # macOS -> bin/lean-fmt
#       just compile-linux    # Linux -> bin/lean-fmt-linux-amd64
#
# Usage:
#   TAG=v0.1.0 scripts/release.sh
#   TAG=v0.1.0 scripts/release.sh path/to/extra-asset
#
# If TAG is not set, the script tries to infer it from the current commit:
#   git describe --tags --exact-match

TAG="${TAG:-}"

if [[ -z "$TAG" ]]; then
  if git describe --tags --exact-match >/dev/null 2>&1; then
    TAG="$(git describe --tags --exact-match)"
  else
    echo "ERROR: TAG is not set and the current commit has no exact tag."
    echo "Usage: TAG=v0.1.0 scripts/release.sh"
    exit 1
  fi
fi

if ! command -v gh >/dev/null 2>&1; then
  echo "ERROR: GitHub CLI 'gh' is required. Install it and run 'gh auth login'."
  exit 1
fi

# Default assets if none are provided.
if [[ "$#" -eq 0 ]]; then
  assets=(bin/lean-fmt bin/lean-fmt-linux-amd64)
else
  assets=("$@")
fi

# Check that all assets exist.
for f in "${assets[@]}"; do
  if [[ ! -f "$f" ]]; then
    echo "ERROR: asset not found: $f"
    exit 1
  fi
done

echo "Releasing tag: $TAG"
echo "Assets:"
for f in "${assets[@]}"; do
  echo "  - $f"
done

# If the release exists, upload/replace assets.
if gh release view "$TAG" >/dev/null 2>&1; then
  echo "Release '$TAG' already exists, uploading assets (with --clobber)..."
  gh release upload "$TAG" "${assets[@]}" --clobber
else
  echo "Creating new release '$TAG'..."
  NOTES="${NOTES:-"Release $TAG"}"
  gh release create "$TAG" "${assets[@]}" -t "$TAG" -n "$NOTES"
fi

echo "Done."

