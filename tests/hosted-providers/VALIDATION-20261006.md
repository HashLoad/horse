# Hosted validation — 2026-10-06

Local validation of the existing uncommitted provider fixes. This run added
test fixtures/runners/documentation; it did not change production provider
code, external libraries, system IIS settings or any remote repository.

| Execution | Scenarios | HTTP assertions | Result |
| --- | ---: | ---: | --- |
| IIS Express x86, Delphi 10/11/12/13, CGI/ISAPI, Tree/Radix | 16 | 96 | PASS |
| Apache 2.4.68 Linux, Delphi 12/13, Tree/Radix | 4 | 24 | PASS |
| Apache CGI, FPC 3.2.2/3.3.1, Tree/Radix | 4 | 24 | PASS |
| Apache proxy FastCGI, FPC 3.2.2/3.3.1, Tree/Radix | 4 | 24 | PASS |

Total: **28 hosted scenarios / 168 HTTP assertions**, plus the FPC 3.3.1
keep-alive regression: 30 HTTP 200 responses on the same socket; median
0.055 ms, p95 0.111 ms (test median threshold: 25 ms).

FPC 3.3.1 source revision:
`7a7ff6e7315db58f451c3a81a091a2fb6e0b0088`.
This development compiler was built in Docker, not installed over Lazarus.

## Authoritative reports under `benchmarks/results/`

- `iis-hosted-20261006-205844/`: 16 `check.log` files, all PASS; runner exit 0.
- `apache-delphi-20261006-210819/`: four `results.log` files, all PASS; runner exit 0.
- `hosted-fpc322-final/CGI-{Tree,Radix}/results.log` and
  `hosted-fpc331-final/CGI-{Tree,Radix}/results.log`: all PASS. The combined
  runs subsequently failed in FastCGI setup; these logs establish only CGI
  coverage, not a passing combined run.
- `fastcgi-fpc322-complete/` and `fastcgi-fpc331-complete/`: Tree/Radix
  all PASS, both final runners exit 0.
- `hosted-20261006-204635/keepalive/results.log`: PASS, runner exit 0.

Reports are local ignored artifacts; these paths are not distributed test data.

## Remaining external limitation: FPC Apache module

Direct Apache-module execution fails on this Debian/Apache environment with
both FPC versions. `FpcApacheBaseline.lpr` reproduced failure **without importing
any Horse unit**. FPC 3.3.1 also failed with the optional `BASELINE_CMEM` control.
The recorded backtrace includes `THREADSTATE.ORPHAN`, `FINALIZEHEAP`,
`DONETHREAD` and `CTHREADCLEANUP`; the Apache FPC runtime dummy thread is active.

Diagnostic reports: `hosted-fpc322/baseline/`,
`hosted-20261006-204635/baseline/`,
`hosted-20261006-204635/apache-debug-backtrace.log`,
and `apache-baseline-cmem/baseline/`.

This does not establish a Horse defect, nor does it establish Apache/FPC runtime
compatibility. Keep that cell **unvalidated/failed in this environment**; do not
claim an all-provider pass. No external FPC/Apache patches were applied.

## Test-environment issues resolved

- Portable IIS Express uses an explicit isolated `/userhome` and config.
- Delphi CGI fixtures are console applications.
- JSON echo explicitly declares UTF-8 rather than relying on RTL defaults.
- Delphi Apache uses an application Alias, preserving route `PATH_INFO`.
- FastCGI uses a container-only wildcard bind, a GENERIC Apache backend and
  explicit `PATH_INFO` mapping; Tree/Radix use distinct backend ports to avoid
  TIME_WAIT collisions.

Earlier failed logs were retained. Process termination is test cleanup, not
proof of graceful hosted shutdown or production worker leak freedom.
