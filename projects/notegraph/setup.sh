#!/bin/bash

# exit script with nonzero exit code if any command fails
set -e

# echo each command
set -x

# Runs in this script rather than with a Hakyll preprocessing step 
# (e.g. `preprocess (callCommand "cd projects/notegraph && npm install")`)
# because the preprocessing rule and the compilation rule which copies
# the mathjax files do not seem to run in the same compilation round.
# The copy step only runs _after_ the preprocessing rule has run and
# some other change triggers recompilation.
# Running 'npm ci' here ensures that the mathjax files are present
# the first time the site is built.
# Running 'npm ci' rather than 'npm install' ensures that the existing 
# package-lock.json file is used, if present, and that a new one is not
# generated.
npm ci