# FPC HTTPApplication graceful shutdown regression

`GracefulShutdownCheck.dpr` drives `THorse.StopListenGraceful` on the FPC
HTTPApplication provider (the default provider for FPC console applications).
Before this change, the provider did not override `StopListen`: it inherited
the abstract one, which raises `StopListen not implemented`, although
`doc/deployment.md` stops this shape with `THorse.StopListen`.

Two modes, each must exit with code 0:

- `drain`: with a slow request in flight and an idle keep-alive connection
  open, `StopListenGraceful` stops accepting, lets the slow request answer
  200, makes the idle connection see EOF in under 1 s (keep-alive is switched
  off instead of waiting for its 15 s window) and returns before `Listen`
  does.
- `in-onlisten`: `StopListenGraceful` called inside the `Listen` callback, the
  earliest point; `Listen` returns without serving. The accept loop runs with
  an idle timeout, so a stop requested before `Run` starts accepting is still
  honoured.

On Linux, run `sh tests/fpc-graceful-shutdown/run-graceful-shutdown-test.sh`
from the repository root. The same command is registered in the tests
workflow. Against the provider without this change, `drain` hangs (the
script fails on its timeout) and `in-onlisten` exits with
`StopListen not implemented`.
