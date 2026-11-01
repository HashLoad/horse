#!/bin/bash
set -euo pipefail

name=$1
shift
mkdir -p /tmp/horse-keepalive-inputs
while IFS= read -r line; do
  line=${line//$'\r'/}
  line=${line//\"/}
  path=${line//\\//}
  file=${path##*/}
  [[ -n "$line" ]] || continue
  if [[ -f "/out/$file" ]]; then
    cp "/out/$file" /tmp/horse-keepalive-inputs/
  elif [[ -f "/delphi/$file" ]]; then
    cp "/delphi/$file" /tmp/horse-keepalive-inputs/
  else
    echo "Missing linker input: $line" >&2
    exit 1
  fi
  printf '/tmp/horse-keepalive-inputs/%s\n' "$file"
done < "/out/$name.lnk" > /tmp/horse-keepalive.rsp

cp /delphi/librtlhelper*.a /delphi/libpcre_PIC.a /tmp/horse-keepalive-inputs/
entry=$(nm "/out/$name.o" | awk '/ T .*initializationEv/ {print $3}')
[[ -n "$entry" ]] || { echo 'Missing program entry point' >&2; exit 1; }
ld -o "/tmp/$name" -e "$entry" \
  --gc-sections --dynamic-list "/out/$name.exp" -z relro --build-id \
  --eh-frame-hdr -m elf_x86_64 --dynamic-linker /lib64/ld-linux-x86-64.so.2 \
  -L/tmp/horse-keepalive-inputs @/tmp/horse-keepalive.rsp \
  -l:libgcc_s.so.1 -lrtlhelper_PIC -lc -ldl -lpthread -lm \
  -lrtlhelper -lpcre_PIC -l:libz.so.1

# Daemon intentionally forks when parented by a normal shell. The timeout is
# PID 1 and its direct child owns the listener without daemonizing.
exec timeout 40s "/tmp/$name" "$@"
