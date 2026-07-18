# Running the Epoll provider tests (Linux)

The Epoll provider (`Horse.Provider.Epoll`) is Linux-only. The test suite for
it reuses the shared pieces:

- **Server**: `HorseEpollTestServer.dpr` (+ `Horse.Indy.TestRoutes.pas`) —
  built and run on Linux
- **Clients**: the existing Windows binaries — no Linux client build needed
  when the server runs under WSL2 (see topology below)

## Compiler options for the server

The shared routes register handlers as anonymous procedures, which constrains
the compiler choice:

| Compiler | Works? | Notes |
|---|---|---|
| **FPC 3.3.1+ (trunk)** with `-dHORSE_FPC_FUNCTIONREFERENCES` | ✅ recommended | The define switches `THorseCallback` / `THorseChunkProducer` to `reference to` types (FUNCTIONREFERENCES modeswitch), letting the anonymous route handlers compile. The `/stream/*` routes use plain unit-scope producers + threadvar counters either way. |
| **FPC 3.2.2 (stable)** | ❌ | No FUNCTIONREFERENCES — every anonymous route handler in Horse.Indy.TestRoutes fails to compile. |
| **Delphi 10.4+ (Linux64 target)** | ✅ | Compiles the Delphi branches (anonymous producers included); dispatch runs on the TTask path of the provider. |

### Installing FPC trunk (3.3.1) in WSL2

Stable 3.2.2 from apt cannot compile the suite (no FUNCTIONREFERENCES).
Build trunk from source using 3.2.2 as the seed compiler:

```sh
sudo apt install fpc make git binutils        # 3.2.2 seed compiler
git clone --depth 1 https://gitlab.com/freepascal.org/fpc/source.git ~/fpcsrc
cd ~/fpcsrc
make clean all -j$(nproc)                     # ~5-10 min
sudo make install PREFIX=/usr/local/fpc-trunk
sudo /usr/local/fpc-trunk/lib/fpc/3.3.1/samplecfg /usr/local/fpc-trunk/lib/fpc/3.3.1 /etc
export PATH=/usr/local/fpc-trunk/bin:$PATH    # add to ~/.bashrc to persist
fpc -iV                                       # must print 3.3.1
```

(Alternative: `fpclazup-x86_64-linux --fpcVersion=trunk --installdir=$HOME/fpctrunk
--only=FPC --noconfirm` from the fpcupdeluxe GitHub releases.)

### Building the server

From `horse/samples/tests/` (the WSL view of the Windows repo works —
`/mnt/c/lang/Repo/horse/samples/tests`; keep compiler output in the WSL
filesystem for speed):

```sh
mkdir -p ~/epoll-test/bin
fpc -MDelphi -Sh -O2 \
    -dHORSE_PROVIDER_EPOLL \
    -dHORSE_FPC_FUNCTIONREFERENCES \
    -Fu/mnt/c/lang/Repo/horse/src -Fu. \
    -FE"$HOME/epoll-test/bin" -FU"$HOME/epoll-test/bin" \
    HorseEpollTestServer.dpr
~/epoll-test/bin/HorseEpollTestServer
```

**`-dHORSE_PROVIDER_EPOLL` on the command line is mandatory** — the
`{$DEFINE}` inside the .dpr only scopes to the program file; Horse.pas (a
unit) never sees it and silently selects the FPC default provider
(fphttpserver via Horse.Provider.FPC.HTTPApplication) instead.  Check the
compile log: it must list `Horse.Provider.Epoll.pas`.

**cthreads is mandatory too** — on FPC/Linux the program's uses clause must
start with `cthreads` ({$IF DEFINED(FPC) AND DEFINED(UNIX)} guarded; already
in the patched HorseEpollTestServer.dpr).  Without it the first TThread
creation aborts with runtime error 232 ("This binary has no thread support
compiled in") right after the startup banner.

**Changing -d defines requires a clean rebuild** — FPC reuses cached .ppu
units when the SOURCE is unchanged, even if the command-line defines differ,
so adding -dHORSE_PROVIDER_EPOLL after a previous build silently keeps the
old provider selection baked into Horse.ppu.  Clear the -FU directory (or
pass `-B`) whenever defines change.  Symptom of the stale build: fpWeb
"Module Error" 500 pages ("Default session not available outside
handlerequest") on bodied requests, a trailing newline on every response
body, and 501 on the /stream/* routes — all fphttpserver behaviors, not
Epoll's.

This is the first-ever FPC compile of this stack — the dual-compilation rule
has been maintained by review, not by a compiler.  Treat initial compile
errors as expected validation output, not surprises.

### FPC threading caveat ([EPOLL-STREAM-1])

FPC builds hardcode `HORSE_EPOLL_SYNCHRONOUS`: requests dispatch INLINE on
the epoll event-loop worker threads (no task pool).  A draining stream
therefore blocks that worker for its duration (~600 ms for test 33), which
can briefly stall other connections assigned to the same worker.  Acceptable
for Phase 1 validation (there is one worker per CPU core and the client
timeout is 8 s; test 36's two concurrent streams at worst serialize) — but a
production FPC deployment with long-lived streams should revisit this in
Phase 2.  The threadvar test producers remain correct in this mode because
handler and drain share the same thread.

## Building with Delphi (Linux64 target + PAServer in WSL2)

Requires Delphi 10.4+ **Enterprise or Architect** (the Linux 64-bit target is
not in Professional). One-time setup:

1. **Linux prerequisites** (inside WSL2 Ubuntu):
   ```sh
   sudo apt update
   sudo apt install joe wget p7zip-full curl openssh-server \
        build-essential zlib1g-dev libcurl4-gnutls-dev libncurses-dev
   ```
   (Package list per Embarcadero's DocWiki "Linux Application Development".)

2. **PAServer in WSL2** — copy the tarball shipped with Delphi
   (`C:\Program Files (x86)\Embarcadero\Studio\22.0\PAServer\LinuxPAServer22.0.tar.gz`)
   into WSL, extract, run:
   ```sh
   tar xzf LinuxPAServer22.0.tar.gz && cd PAServer-22.0 && ./paserver
   ```
   Set (or leave empty) the connection password; default port 64211.

3. **Connection profile + SDK** (Delphi IDE, once):
   - Tools → Options → Deployment → Connection Profile Manager → Add:
     platform *Linux 64-bit*, host `127.0.0.1`, port `64211` (WSL2 forwards
     Windows localhost to WSL listeners) → Test Connection.
   - Tools → Options → Deployment → SDK Manager → Add → Linux 64-bit +
     that profile (pulls the WSL toolchain/libs into the local SDK cache).

Per-build steps:

4. Copy the patched sources over the repo first
   (`Horse.Provider.Epoll.pas` → `horse\src\`,
   `Horse.Indy.TestRoutes.pas` → `horse\samples\tests\`).
5. Open `HorseEpollTestServer.dproj` → right-click *Target Platforms* →
   *Add Platform* → **Linux 64-bit** → assign the connection profile.
6. Select Linux64/Release → **Build All** → *Run → Run Without Debugging*.
   The IDE deploys the binary via PAServer and starts it; console output
   appears in the PAServer terminal in WSL. (To run it manually later:
   `~/PAServer/scratch-dir/<profile>/HorseEpollTestServer/HorseEpollTestServer`.)

On the Delphi-Linux build the provider dispatches on `TTask` and the
streaming engine uses its bounded-retry send path — the suite expectations
below are the same as for FPC.

## Test topology (WSL2 — no Linux client needed)

WSL2 forwards Windows `localhost` to the WSL VM, so the Windows test clients
reach a server running inside WSL directly:

```
1. In WSL:      ./bin/HorseEpollTestServer          (listens on 9010)
2. On Windows:  HorseIndyTestClient.exe             (TCrossHttpClient)
                HorseNetHttpTestClient.exe          (WinHTTP)
   — both target http://127.0.0.1:9010, which WSL2 forwards to the server.
```

Make sure no Windows server (HTTP.sys/IOCP/Indy) is holding port 9010 first —
the HTTP.sys kernel listener in particular would shadow the forwarded port.

## Expected results

- Core suite: same expectations as IOCP (custom parser — `/upload` and
  empty-body POST accept 200 or 400).
- Streaming tests 33–36: the Epoll provider has a streaming engine
  ([EPOLL-STREAM-1], synchronous pull pump with manual chunked framing —
  same wire contract as HTTP.sys [HTTPSYS-STREAM-1]), so on an
  FPC-with-FUNCTIONREFERENCES or Delphi-Linux build the routes stream for
  real: test 33 ≈ 600 ms with the 5-chunk body, 34 propagates the
  Content-Type, 35 answers an instant empty 200 (explicit Content-Length: 0),
  36 drains two concurrent streams. The dual-outcome client checks accept
  either a complete stream or a clean 501, so older builds without the
  engine still pass as graceful refusal.

## Incremental wire check (from WSL or Windows)

```sh
curl -N http://127.0.0.1:9010/stream/pull    # chunks appear ~120 ms apart, then exits
curl    http://127.0.0.1:9010/stream/empty   # instant, empty body
```

If `curl -N` prints everything but never exits, the chunked terminator is
missing — see the framing lessons recorded in `plans/streaming-sse.md`
(HTTP.sys engine history: transports never frame chunked bodies for you).
