# HTTP.sys lifetime regression

`HttpSysLifetimeCheck.lpr` starts and stops the built-in HTTP.sys provider with
the FPC heap tracer enabled. The process must finish with zero unfreed blocks.

The test uses `http://localhost:9095/`. HTTP.sys requires a URL reservation for
non-administrator processes. From an elevated terminal, configure it once:

```powershell
netsh http add urlacl url=http://localhost:9095/ user="$env:USERDOMAIN\$env:USERNAME"
```

Run the regression from a normal PowerShell terminal:

```powershell
powershell -ExecutionPolicy Bypass -File tests/httpsys-lifetime/run-httpsys-lifetime-test.ps1
```

Set the `FPC` environment variable if FPC 3.2.2 is installed elsewhere.
