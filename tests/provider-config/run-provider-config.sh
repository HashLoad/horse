#!/usr/bin/env bash
set -euo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
horse_src=${HORSE_SRC:-$(cd "$here/../../src" && pwd)}
work=$(mktemp -d /tmp/horse-provider-config.XXXXXX)
trap 'rm -rf "$work"' EXIT
for provider in default epoll daemon; do
  for router in tree radix; do
    flags=()
    case "$provider" in
      epoll) flags+=(-dHORSE_PROVIDER_EPOLL) ;;
      daemon) flags+=(-dHORSE_DAEMON) ;;
    esac
    if [[ "$router" = radix ]]; then flags+=(-dHORSE_RADIX_ROUTER); fi
    fpc -B -Mdelphi -Sh -gh -gl -Fu"$horse_src" -FU"$work" -FE"$work" \
      "${flags[@]}" "$here/ProviderConfigCheck.dpr" > "$work/build.log" 2>&1 || {
        tail -30 "$work/build.log"; exit 1;
      }
    timeout 20s "$work/ProviderConfigCheck" > "$work/run.log" 2>&1 || {
      cat "$work/run.log"; exit 1;
    }
    if grep -Eq '[1-9][0-9]* unfreed memory blocks' "$work/run.log"; then
      cat "$work/run.log"; exit 1;
    fi
    echo "PASS: $provider / $router (default + 10 TLS rejection checks; no heap leak report)"
  done
done
