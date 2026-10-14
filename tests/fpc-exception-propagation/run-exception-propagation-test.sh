#!/bin/sh
set -eu

test_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
output_dir=$(mktemp -d /tmp/horse-exception-propagation.XXXXXX)

cd "$test_dir"
# Override only for validating this regression against an earlier source tree.
source_dir=${HORSE_TEST_SOURCE:-../../src}
fpc -B -Mdelphi -Sh -gh -gl \
  -Fu"$source_dir" -Fu../../src -FU"$output_dir" -FE"$output_dir" "$@" \
  ExceptionPropagationCheck.dpr

set +e
run_output=$("$output_dir/ExceptionPropagationCheck" 2>&1)
run_status=$?
set -e
printf '%s\n' "$run_output"
if [ "$run_status" -ne 0 ]; then
  exit "$run_status"
fi
if printf '%s\n' "$run_output" | grep -Eq '[1-9][0-9]* unfreed memory blocks'; then
  echo 'Exception propagation regression: FPC reported unfreed memory.' >&2
  exit 1
fi
