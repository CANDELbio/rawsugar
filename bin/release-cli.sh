#!/bin/bash

# Push a release to Github. Put release title and notes in the file release-notes.md
# You probably want to run ./bin/package.sh before running this.
# Requires gh (brew install gh) and works better running from Terminal than Emacs.
echo "Running $BASH_SOURCE"

export TIER=$1
export VERSION=$(cat version)

# Add git log since last release
# TODO probably wants code formatting and should be in separate file to avoid problems
# This is having problems
# git log last-release..HEAD --oneline >> release-notes.md

# -p means prerelease
gh release create  v$VERSION rawsugar-${TIER}-${VERSION}.zip -F release-notes.md
git tag -f last-release

# Note: if the above fails for authentication reasons, generate a new token at
# https://github.com/settings/tokens and export it as GITHUB_TOKEN

