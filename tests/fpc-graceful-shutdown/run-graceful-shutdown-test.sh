#!/bin/sh
set -eu

test_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
output_dir=$(mktemp -d /tmp/horse-graceful-shutdown.XXXXXX)
port=${HORSE_GRACEFUL_PORT:-9911}

cd "$test_dir"
fpc -B -Mdelphi -Sh -gl \
  -Fu../../src \
  -FU"$output_dir" -FE"$output_dir" \
  GracefulShutdownCheck.dpr

# Without this change, StopListen raised "not implemented" and Listen never
# returned: the timeout turns that hang into a failure.
for mode in drain in-onlisten; do
  set +e
  timeout 30 "$output_dir/GracefulShutdownCheck" "$port" "$mode"
  status=$?
  set -e
  if [ "$status" -ne 0 ]; then
    echo "Graceful shutdown regression ($mode): exit status $status." >&2
    exit 1
  fi
done

echo 'Graceful shutdown regression passed: drain, idle keep-alive, Listen order and stop inside OnListen.'
