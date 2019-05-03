#!/bin/sh

TEST_STATUS=0

npx stylelint css/*.scss || TEST_STATUS=$?

stack test || TEST_STATUS=$?

# Return the exit code of the last failing command.
exit $TEST_STATUS
