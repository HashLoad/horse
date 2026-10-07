#!/usr/bin/env bash
set -euo pipefail
root=${HORSE_ROOT:-/repo}
out=${HORSE_TEST_OUTPUT:-/out}
mkdir -p "$out/keepalive"
test "$(fpc -iV)" = '3.3.1'
fpc -B -Mdelphi -Sh -dHORSE_CONSOLE -Fu"$root/src" \
  -FU"$out/keepalive" -FE"$out/keepalive" \
  "$root/tests/src/FPCHttpKeepAliveServer.dpr" > "$out/keepalive/build.log" 2>&1 || {
  tail -30 "$out/keepalive/build.log"; exit 1;
}
"$out/keepalive/FPCHttpKeepAliveServer" > "$out/keepalive/server.log" 2>&1 &
server=$!
trap 'kill "$server" 2>/dev/null || true; wait "$server" 2>/dev/null || true' EXIT
ready=false
for attempt in {1..50}; do
  if curl -fsS http://127.0.0.1:9901/ping > /dev/null 2>&1; then ready=true; break; fi
  sleep .1
done
if [[ "$ready" != true ]]; then cat "$out/keepalive/server.log"; exit 1; fi
python3 "$root/tests/fpc_keepalive_regression.py" | tee "$out/keepalive/results.log"
