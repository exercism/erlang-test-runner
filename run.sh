#!/usr/bin/env sh

printf "Exercise: %s\nSolution: %s\nOutput:   %s\n" "$1" "$2" "$3"

exec /opt/test-runner/bin/erlang_test_runner "$@"
