# Streaming over the Indy provider

`IndyStreamCheck.dpr` runs the real Console (or VCL) provider with
`KeepConnectionAlive` on and reads the wire with a raw TCP client:

- `/sse` streams three `text/event-stream` events, 200 ms apart;
- `/partes` streams three `text/plain` parts;
- `/comum` is an ordinary `Send`.

A streaming response must carry no `Content-Length` and a single
`Transfer-Encoding: chunked`, and its body must decode from the chunks. After
the SSE response, `/comum` on the same connection must answer 200, and on its
own it must keep its `Content-Length`. The program prints the Indy version
(`gsIdVersion`) and the compiler version.

Before this fix, the Indy branch of `THorseWebBrokerStreamWriter` sent
`Content-Length: 0`. The client ended the response at the header, and the
chunked mode sent `Content-Length` and `Transfer-Encoding` together. Indy writes
a `Content-Length` whenever the response declares no non-identity
`Transfer-Encoding` (it computes one from `ContentText` or `ContentStream`), so
on Indy both modes are now chunked, SSE included.

## Windows

```powershell
./tests/indy-stream/run-indy-stream.ps1 -Version 19.0 -Provider Console
```

`-Version` is the RAD Studio folder (`19.0` is Delphi 10.2 Tokyo). Reports go
to `benchmarks/results/indy-stream-*`.

## Negative control

`-HorseSource` points to a `src` folder from before the fix. The check must
fail on `/sse` with "must not carry Content-Length".

These tests need a licensed Delphi, so the public FPC job does not run them.
Streaming over Indy assumes an HTTP/1.1 client; HTTP/1.0 clients are out of
scope for this check.
