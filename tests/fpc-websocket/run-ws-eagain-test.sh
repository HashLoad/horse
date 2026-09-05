#!/usr/bin/env bash
# =============================================================================
# FIX-WS-NONBLOCK / FIX-WS-EINTR regression — build + run on FPC + epoll.
#
# Requested in review of HashLoad/horse PR #549. Horse's own suite is DUnitX
# and Delphi-only, so the FPC+epoll path needs a standalone program; this
# builds and runs it.
#
#   HORSE_SRC=/path/to/horse/src ./run-ws-eagain-test.sh
#
# Exit code = number of failed checks, so it can gate CI directly.
# =============================================================================
set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
HORSE_SRC=${HORSE_SRC:-$(cd "$HERE/../../src" 2>/dev/null && pwd)}
FPC=${FPC:-fpc}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

echo "FPC       : $($FPC -iV 2>/dev/null || echo 'NOT FOUND')"
echo "Horse src : $HORSE_SRC"

if ! command -v "$FPC" >/dev/null 2>&1; then
  echo "FAIL: fpc not on PATH. Set FPC=/path/to/fpc." >&2
  exit 1
fi

if [[ ! -f "$HORSE_SRC/Horse.pas" ]]; then
  echo "FAIL: Horse.pas not found under $HORSE_SRC" >&2
  echo "      Set HORSE_SRC to the horse/src directory." >&2
  exit 1
fi

# The guard this test exists to protect. Without it the run is meaningless —
# every check would fail for the same reason and prove nothing about the loop.
if ! grep -q 'WS_SOCKET_READ_TICK_MS' "$HORSE_SRC/Horse.Provider.Socket.WebSocket.pas" 2>/dev/null; then
  echo "FAIL: $HORSE_SRC/Horse.Provider.Socket.WebSocket.pas is missing FIX-WS-NONBLOCK." >&2
  echo "      This tree predates the fix; the test would fail for the wrong reason." >&2
  exit 1
fi

echo
echo "── compiling ──────────────────────────────────────────────────────────"
# -B forces a full rebuild: a cached .ppu of the transport would silently test
# the previous version of the very unit under test.
if ! "$FPC" -MDelphi -B -O1 -gl \
      -dHORSE_PROVIDER_EPOLL \
      -Fu"$HORSE_SRC" \
      -Fu"$HORSE_SRC/Providers" \
      -FU"$WORK" -FE"$WORK" \
      "$HERE/WebSocketEagainRegression.lpr" > "$WORK/build.log" 2>&1; then
  echo "FAIL: compilation failed"
  tail -30 "$WORK/build.log" | sed 's/^/    | /'
  exit 1
fi
echo "  compiled OK"

echo
echo "── running ────────────────────────────────────────────────────────────"
"$WORK/WebSocketEagainRegression"
RC=$?

echo
if [[ $RC -eq 0 ]]; then
  echo "RESULT: all checks passed"
else
  echo "RESULT: $RC check(s) failed"
fi
exit $RC
