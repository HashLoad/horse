#!/bin/sh
set -eu

test_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
output_dir=$(mktemp -d /tmp/horse-url-decoding.XXXXXX)
trap 'rm -rf "$output_dir"' EXIT

cd "$test_dir"
fpc -B -Mdelphi -Sh \
  -Fu../../src \
  -FU"$output_dir" -FE"$output_dir" \
  UrlDecodingCheck.dpr

"$output_dir/UrlDecodingCheck"
echo 'FPC URL decoding regression passed.'
