#!/usr/bin/env bash
set -euo pipefail
while IFS= read -r line; do
  line=${line//$'\r'/}
  line=${line//\"/}
  path=${line//\\//}
  name=${path##*/}
  [[ -n "$line" ]] || continue
  if [[ -f "/out/$name" ]]; then printf '/out/%s\n' "$name"
  elif [[ -f "/delphi/$name" ]]; then printf '/delphi/%s\n' "$name"
  else echo "Missing linker input: $line" >&2; exit 1
  fi
done < /out/HostedProviderCheck.lnk > /tmp/hosted.rsp
# Apache resolves its own exported ap_* and apr_* symbols when loading the DSO.
ld -o /out/horse.so -e _ZN19Hostedprovidercheck14initializationEv \
  --gc-sections --version-script /out/HostedProviderCheck.vsr -shared \
  --export-dynamic -z noexecstack -z relro --build-id --eh-frame-hdr -m elf_x86_64 \
  -L/delphi @/tmp/hosted.rsp -l:libgcc_s.so.1 -lrtlhelper_PIC -lc -ldl \
  -lpthread -lm -lrtlhelper -lpcre_PIC -lz -rpath '$ORIGIN'
sed -e 's|@OUT@|/out|g' \
  -e 's|@PROVIDER_MODULE@|LoadModule horse_hosted_module /out/horse.so|g' \
  -e 's|@PROVIDER_HANDLER@|SetHandler horse-hosted-handler|g' \
  /repo/tests/hosted-providers/apache.conf > /out/httpd.conf
apache2 -t -f /out/httpd.conf -c 'Alias /horse /out/horse.so'
apache2 -f /out/httpd.conf -c 'Alias /horse /out/horse.so' -DFOREGROUND > /out/host.log 2>&1 &
server=$!
trap 'kill -TERM "$server" 2>/dev/null || true; wait "$server" 2>/dev/null || true' EXIT
ready=false
for attempt in {1..50}; do
  if curl -fsS http://127.0.0.1:19280/horse/ping >/dev/null 2>&1; then ready=true; break; fi
  sleep .1
done
if [[ "$ready" != true ]]; then cat /out/error.log /out/host.log; exit 1; fi
python3 /repo/tests/hosted-providers/check-http.py http://127.0.0.1:19280/horse | tee /out/results.log
