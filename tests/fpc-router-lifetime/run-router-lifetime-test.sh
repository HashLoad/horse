#!/bin/sh
set -eu

test_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
output_dir=$(mktemp -d /tmp/horse-router-lifetime.XXXXXX)

cd "$test_dir"
fpc -B -Mdelphi -Sh -gh -gl \
  -Fu../../src \
  -FU"$output_dir" -FE"$output_dir" \
  RouterLifetimeCheck.dpr

set +e
run_output=$("$output_dir/RouterLifetimeCheck" 2>&1)
run_status=$?
set -e
printf '%s\n' "$run_output"

if [ "$run_status" -ne 0 ]; then
  exit "$run_status"
fi

if printf '%s\n' "$run_output" | grep -Eq '[1-9][0-9]* unfreed memory blocks'; then
  echo 'Router lifetime regression: FPC reported unfreed memory.' >&2
  exit 1
fi

echo 'Router lifetime regression passed: no FPC heap leak report.'
