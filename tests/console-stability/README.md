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
