# HTTP over the IOCP provider

`IocpHttpCheck.dpr` runs the real IOCP provider on `127.0.0.1:19265` and talks
raw HTTP over TCP, so headers, framing and connection state are checked as they
go on the wire.

A blocking loopback socket almost never returns a partial send, so the check
swaps the provider's low-level sends (`TIocpRawResponse.SendFunction` and
`GatherSendFunction`) to force a partial count or a failure.
`THorseProviderIOCP.ConnectionCount` shows whether a closed connection left
the registry.

| Check | What passes |
|---|---|
| CA1 partial send | With one byte per send, 1 MiB by `Send(string)`, 1 MiB by stream and one 64 KiB SSE event arrive whole. |
| CA2 failed send | When a send fails mid-response, the server closes the connection, the registry count comes back, and the stream writer reports `IsConnected = False`. The next request on a new connection is then served. |
| CA3 `Connection: close` | 200 requests leave the registry count where it was. |
| CA4 exception after `SendStream` | The client gets the streamed response, no byte after it, and the connection is closed. |
| CA6 load | 32 keep-alive clients, 50 rounds each of 8 MiB by `Send(string)` and 8 MiB by stream, arrive whole. This is a measure, not a negative control. |

Before this fix:
- `WriteV` and the response sends ignored how many bytes went out;
- the stream writer's failed send left `IsConnected` true;
- a response without keep-alive closed the socket outside the registry, so its
  context stayed there;
- the error response built after `SendStream` was appended to a response that
  was already delimited.

## Windows

```powershell
./tests/iocp-http/run-iocp-http.ps1 -Version 19.0
```

`-Version` is the RAD Studio folder (`19.0` is Delphi 10.2 Tokyo). Reports go
to `benchmarks/results/iocp-http-*`. Every check prints `PASS` or `FAIL`, and
a final `PASS:` line means all of them passed.

## Negative control

The check needs the seam, so the previous source gets it first. From a
checkout of the commit before this fix:

```powershell
git worktree add ../horse-before <commit-before-the-fix>
git -C ../horse-before apply "$PWD/tests/iocp-http/seam-only.patch"
./tests/iocp-http/run-iocp-http.ps1 -Version 19.0 -HorseSource ../horse-before/src
```

`seam-only.patch` adds only the replaceable sends and the registry count, and
changes no behaviour. Against that source, CA1 fails because the response is
truncated. CA2 fails because the connection stays open. CA3 fails because the
registry count grows. CA4 fails because bytes arrive after the response.

These tests need a licensed Delphi, so the public FPC job does not run them.
