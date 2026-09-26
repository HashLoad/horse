# Console provider stability regression

This Windows/Delphi stress test models the workload reported in issue #581:
50 persistent HTTP/1.1 clients perform 1,500 keep-alive requests each, for a
total of 75,000 transactions. It fails on connection errors, timeouts, HTTP
errors, or invalid response bodies and reports the highest observed latency.

Run from the repository root:

```powershell
powershell -ExecutionPolicy Bypass -File tests/console-stability/run-console-stability-test.ps1
```

Set `RADSTUDIO_RSVARS` when RAD Studio is installed outside the default Delphi
12 path.

The default run has no delay so that it can be used as a quick regression
check. The workload can be paced with environment variables. The issue's
reported rate (75,000 transactions from 50 clients over approximately eight
hours) is equivalent to a delay of about 19.2 seconds between requests from
each client:

```powershell
$env:HORSE_STABILITY_CLIENTS = '50'
$env:HORSE_STABILITY_REQUESTS_PER_CLIENT = '1500'
$env:HORSE_STABILITY_REQUEST_DELAY_MS = '19200'
powershell -ExecutionPolicy Bypass -File tests/console-stability/run-console-stability-test.ps1
```

Unset `HORSE_STABILITY_REQUEST_DELAY_MS` (or set it to `0`) for the quick run.
