#!/bin/sh

TEST_STATUS=0

npx stylelint css/*.scss || TEST_STATUS=$?

# Must `export PERCY_TOKEN` on the command-line before running
# this command. The PERCY_TOKEN is located at
# https://percy.io/justinmanley/www.justinmanley.work/settings.
percy snapshot _site || TEST_STATUS=$?

stack test || TEST_STATUS=$?

# Return the exit code of the last failing command.
exit $TEST_STATUS
