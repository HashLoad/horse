#!/usr/bin/env bash
set -euo pipefail
work=/out/baseline
mkdir -p "$work/units"
flags=()
if [[ "${BASELINE_CMEM:-0}" = 1 ]]; then flags+=(-dBASELINE_CMEM); fi
fpc -B -gl -gw3 -Cg "${flags[@]}" -Fu/repo/src -FU"$work/units" -o"$work/baseline.so" \
  /repo/tests/hosted-providers/FpcApacheBaseline.lpr > "$work/build.log" 2>&1
sed -e "s|@OUT@|$work|g" \
  -e "s|@PROVIDER_MODULE@|LoadModule horse_hosted_module $work/baseline.so|g" \
  -e 's|@PROVIDER_HANDLER@|SetHandler horse-hosted-handler|g' \
  /repo/tests/hosted-providers/apache.conf > "$work/httpd.conf"
apache2 -f "$work/httpd.conf" -DFOREGROUND > "$work/host.log" 2>&1 &
server=$!
trap 'kill -TERM "$server" 2>/dev/null || true; wait "$server" 2>/dev/null || true' EXIT
sleep 2
curl --fail --max-time 5 http://127.0.0.1:19280/ping > "$work/response.log"
kill -0 "$server"
echo 'PASS: FPC-only Apache baseline (no Horse units)'
