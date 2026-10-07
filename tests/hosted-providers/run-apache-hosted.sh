#!/usr/bin/env bash
set -euo pipefail
root=${HORSE_ROOT:-/repo}
out=${HORSE_TEST_OUTPUT:-/out}
server=''
backend=''
cleanup() {
  if [[ -n "$server" ]]; then
    kill -TERM "$server" 2>/dev/null || true
    wait "$server" 2>/dev/null || true
    server=''
  fi
  if [[ -n "$backend" ]]; then
    kill -TERM "$backend" 2>/dev/null || true
    wait "$backend" 2>/dev/null || true
    backend=''
  fi
}
trap cleanup EXIT
read -r -a providers <<< "${HORSE_TEST_PROVIDERS:-Apache CGI FastCGI}"
for provider in "${providers[@]}"; do
  case "$provider" in Apache|CGI|FastCGI) ;; *) echo "Unsupported provider: $provider" >&2; exit 1;; esac
  for router in Tree Radix; do
    work="$out/$provider-$router"
    mkdir -p "$work/cgi-bin" "$work/units"
    flags=()
    if [[ "$router" = Radix ]]; then flags+=(-dHORSE_RADIX_ROUTER); fi
    module=''
    handler=''
    base='http://127.0.0.1:19280'
    if [[ "$provider" = Apache ]]; then
      flags+=(-dHORSE_APACHE -Cg)
      binary="$work/horse.so"
      module="LoadModule horse_hosted_module $binary"
      handler='SetHandler horse-hosted-handler'
    elif [[ "$provider" = CGI ]]; then
      flags+=(-dHORSE_CGI)
      binary="$work/cgi-bin/horse"
      base+='/cgi/horse'
    else
      flags+=(-dHORSE_FCGI)
      binary="$work/horse-fcgi"
      backend_port=19281
      if [[ "$router" = Radix ]]; then backend_port=19282; fi
      module=$'LoadModule proxy_module /usr/lib/apache2/modules/mod_proxy.so\nLoadModule proxy_fcgi_module /usr/lib/apache2/modules/mod_proxy_fcgi.so'
      # ProxyPass does not supply PATH_INFO by default; Horse routes on it.
      handler=$'ProxyPass / fcgi://127.0.0.1:19281/\nProxyFCGIBackendType GENERIC\nProxyFCGISetEnvIf "true" PATH_INFO "%{reqenv:SCRIPT_NAME}"'
      handler=${handler//19281/$backend_port}
    fi
    fpc -B -Mdelphi -Sh -Fu"$root/src" -FU"$work/units" -o"$binary" \
      "${flags[@]}" "$root/tests/hosted-providers/HostedProviderCheck.dpr" > "$work/build.log" 2>&1 || {
      tail -30 "$work/build.log"; exit 1;
    }
    module=${module//$'\n'/\\n}
    handler=${handler//$'\n'/\\n}
    sed -e "s|@OUT@|$work|g" -e "s|@PROVIDER_MODULE@|$module|g" \
      -e "s|@PROVIDER_HANDLER@|$handler|g" \
      "$root/tests/hosted-providers/apache.conf" > "$work/httpd.conf"
    apache2 -t -f "$work/httpd.conf"
    if [[ "$provider" = FastCGI ]]; then
      "$binary" > "$work/backend.log" 2>&1 &
      backend=$!
    fi
    apache2 -f "$work/httpd.conf" -DFOREGROUND > "$work/host.log" 2>&1 &
    server=$!
    ready=false
    for attempt in {1..50}; do
      if curl -fsS "$base/ping" > /dev/null 2>&1; then ready=true; break; fi
      sleep .1
    done
    if [[ "$ready" != true ]]; then
      curl -sS --max-time 5 -i "$base/ping" > "$work/readiness-response.log" 2>&1 || true
      tail -n 20 "$work/error.log" "$work/host.log"
      if [[ -f "$work/backend.log" ]]; then cat "$work/backend.log"; fi
      exit 1
    fi
    python3 "$root/tests/hosted-providers/check-http.py" "$base" | tee "$work/results.log"
    cleanup
    echo "PASS $provider / $router (real Apache host)"
  done
done
