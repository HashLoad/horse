# Router lifetime regression

`RouterLifetimeCheck.dpr` creates nested RouterTree routes, switches to Radix
and back, and destroys an independent `THorseInstance`. Compile it with the
FPC heap tracer (`-gh`); the process must finish with zero unfreed blocks.

On Linux, run `sh tests/fpc-router-lifetime/run-router-lifetime-test.sh` from
the repository root. The same command is registered in the tests workflow.

For Lazarus/FPC 3.2.2 on Windows, compile from this directory with an output
directory outside the source tree:

```powershell
& C:\lazarus\fpc\3.2.2\bin\i386-win32\fpc.exe -B -Mdelphi -Sh -gh -gl '-Fu../../src' '-FU<output-dir>' '-FE<output-dir>' RouterLifetimeCheck.dpr
& <output-dir>\RouterLifetimeCheck.exe
```

The Windows heap summary should contain `0 unfreed memory blocks : 0`. Replace
the compiler path with the path used by your Lazarus installation.
