#!/usr/bin/env bash

# update-marmalade.sh: Download a copy of https://github.com/kadena-io/marmalade
# from the latest master, then extract the contents to
# pact-tests/pact-tests/marmalade and commit them.

set -eu
[ ! -z "${DEBUG:-}" ] && set -x

ROOT=$(git rev-parse --show-toplevel)
REPO=${REPO:-kadena-io/marmalade}
BRANCH=${BRANCH:-main}

# First, make sure the repo and working copy is completely clean
if ! git diff --quiet --exit-code; then
  echo "Working copy is dirty; aborting"
  exit 1
fi

# And the index, too
if ! git diff --quiet --exit-code --cached; then
  echo "Index is dirty; aborting"
  exit 1
fi

# Next, get the latest commit hash. We want to put it in the commit message.
# There's an easy one-liner
HASH=$(curl -s -H "Accept: application/vnd.github.VERSION.sha" "https://api.github.com/repos/${REPO}/commits/${BRANCH}")
URL="https://github.com/${REPO}/archive/${HASH}.tar.gz"
echo "Git repo is clean; will download latest ${REPO}@${BRANCH} (${HASH})"

# Delete the old marmalade dir, if needed
if [ -d "$ROOT/pact-tests/pact-tests/marmalade" ]; then
  echo "Removing current marmalade directory"
  rm -rf "$ROOT/pact-tests/pact-tests/marmalade"
fi

# Then re-create it, so 'tar' can move to it and extract
mkdir -p "$ROOT/pact-tests/pact-tests/marmalade"

# Download the latest version of marmalade tarball into the dir
echo "Downloading and extracting..."
curl -s -L "$URL" | tar -xvz --strip-components=1 -C "$ROOT/pact-tests/pact-tests/marmalade"

# Commit the changes
echo "Committing changes..."
git add "$ROOT/pact-tests/pact-tests/marmalade"
git commit -m "tests: update marmalade snapshot to ${REPO}@${HASH}"
