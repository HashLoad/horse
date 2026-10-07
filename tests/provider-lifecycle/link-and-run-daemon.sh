#!/usr/bin/env bash
# Run inside a Linux development container with the generated Linux objects
# mounted at /out and Delphi's lib/linux64/release mounted read-only at /delphi.
# Native linking avoids requiring an installed Windows-side Linux SDK.
set -euo pipefail
while IFS= read -r line; do
  line=${line//$'\r'/}
  line=${line//\"/}
  path=${line//\\//}
  name=${path##*/}
  if [[ -z "$line" ]]; then continue; fi
  if [[ -f "/out/$name" ]]; then
    printf '/out/%s\n' "$name"
  elif [[ -f "/delphi/$name" ]]; then
    printf '/delphi/%s\n' "$name"
  else
    echo "Missing linker input: $line" >&2; exit 1
  fi
done < /out/ProviderLifecycle.lnk > /tmp/provider-lifecycle.rsp
ld -o /out/ProviderLifecycle -e _ZN17Providerlifecycle14initializationEv \
  --gc-sections --dynamic-list /out/ProviderLifecycle.exp -z relro --build-id \
  --eh-frame-hdr -m elf_x86_64 --dynamic-linker /lib64/ld-linux-x86-64.so.2 \
  -L/delphi @/tmp/provider-lifecycle.rsp -l:libgcc_s.so.1 -lrtlhelper_PIC -lc -ldl \
  -lpthread -lm -lrtlhelper -lpcre_PIC -lz -rpath '$ORIGIN'
# PID 1 (or a direct child of PID 1) avoids Daemon's intentional double fork,
# so the runner can supervise the real listener and retain its test report.
exec /out/ProviderLifecycle --xml:/out/results.xml
