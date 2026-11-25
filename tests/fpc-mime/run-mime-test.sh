#!/bin/sh
set -eu

test_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
output_dir=$(mktemp -d /tmp/horse-mime.XXXXXX)
trap 'rm -rf "$output_dir"' EXIT

cd "$test_dir"
fpc -B -Mdelphi -Sh \
  -Fu../../src \
  -FU"$output_dir" -FE"$output_dir" \
  MimeTypeCheck.dpr

"$output_dir/MimeTypeCheck"
echo 'FPC MIME type regression passed.'
