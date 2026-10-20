#!/bin/sh
set -eu

test_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
source_dir=${HORSE_TEST_SOURCE:-"$test_dir/../../src"}
output_dir=$(mktemp -d /tmp/horse-header-hash.XXXXXX)

cd "$test_dir"
# Rebuild Horse too: checks on the test program alone do not exercise the bug.
fpc -B -Mdelphi -Sh -gh -gl "$@" -Cr -Co \
  -Fu"$source_dir" -Fu../../src -Fi../../src \
  -FU"$output_dir" -FE"$output_dir" HeaderHashCheck.dpr \
  >"$output_dir/build.log" 2>&1 || {
    cat "$output_dir/build.log"
    exit 1
  }

set +e
run_output=$("$output_dir/HeaderHashCheck" 2>&1)
run_status=$?
set -e
printf '%s\n' "$run_output"
if [ "$run_status" -ne 0 ]; then
  exit "$run_status"
fi
if printf '%s\n' "$run_output" | grep -Eq '[1-9][0-9]* unfreed memory blocks'; then
  echo 'Header hash regression: FPC reported unfreed memory.' >&2
  exit 1
fi
