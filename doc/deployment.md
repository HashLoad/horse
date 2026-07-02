# Deployment Cheatsheet

*Read this in [English](./deployment.md) or [Português (BR)](./deployment.pt-BR.md).*

Same Horse code, seven deployment shapes, two interchangeable async transports (CrossSocket and mORMot2). This page is the at-a-glance reference; for the rationale and longer code samples, see [Providers & Application types §8 (CrossSocket)](./providers.md#8-running-crosssocket-as-each-application-type) or [§9 (mORMot2)](./providers.md#9-running-mormot2-as-each-application-type).

---

## The four-step pattern

Every shape on this page follows the same four steps:

1. **Define exactly one transport** in Project Options → Conditional Defines:
   - `HORSE_PROVIDER_CROSSSOCKET` for the CrossSocket Provider, **or**
   - `HORSE_PROVIDER_MORMOT` for the mORMot2 Provider.

   Add the matching `HORSE_APPTYPE_*` if you want the cross-product convenience unit (since PATCH-HORSE-2). The legacy `HORSE_CROSSSOCKET` alias still works for backwards compatibility; there is no legacy alias for mORMot. The two Provider defines are mutually exclusive — `Horse.pas` rejects the combination at compile time.
2. Pick the **project type** for the desired shape.
3. Call `THorse.Listen(port)` from the **right lifecycle hook** for that shape.
4. Call `THorse.StopListen` from the **shutdown hook** so the Provider drains active requests.

The runtime check that drives shape behaviour is `IsConsole`:

- **`IsConsole = True`** (console binary, `{$APPTYPE CONSOLE}`) → `Listen` blocks the calling thread; the shutdown hook unblocks it via `StopListen`.
- **`IsConsole = False`** (VCL / LCL / TService) → `Listen` starts the IO threads and returns immediately; the calling thread is free for the GUI message loop or the service-control loop.

---

## At-a-glance

| Shape | `{$APPTYPE CONSOLE}` | Project type | `Listen` from | `StopListen` from |
|---|:---:|---|---|---|
| Console (Delphi) | ✔ | Console Application | `begin … end.` | `SetConsoleCtrlHandler` |
| VCL (Delphi) | ❌ | VCL Forms Application | `FormCreate` | `FormClose` |
| Linux daemon (Delphi) | ✔ | Console (Linux64 target) | `begin … end.` | POSIX `signal(SIGTERM, …)` |
| Windows Service (Delphi) | ❌ | Service Application | `ServiceStart` (worker thread) | `ServiceStop` |
| Linux daemon (FPC) | ✔ | Console (FPC) | `begin … end.` | `fpSignal(SIGTERM, …)` |
| LCL desktop (FPC) | ❌ | Lazarus Application | `FormCreate` | `FormClose` |
| FPC HTTPApplication | ✔ | Console (FPC) | `begin … end.` | `fpSignal(SIGTERM, …)` |

---

## Minimal code per shape

### Console (Delphi) — `SetConsoleCtrlHandler` for stop

```pascal
{$APPTYPE CONSOLE}
function CtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  if dwCtrlType in [CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT, CTRL_SHUTDOWN_EVENT] then
  begin 
    THorse.StopListen; 
    Result := True; 
  end
  else 
    Result := False;
end;
begin
  SetConsoleCtrlHandler(@CtrlHandler, True);
  THorse.Listen(9000);
end.
```

### VCL (Delphi) — `FormCreate` / `FormClose`

```pascal
procedure TfrmMain.FormCreate(Sender: TObject);  
begin 
  THorse.Listen(9000); 
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin 
  THorse.StopListen;   
end;
```

### Linux daemon (Delphi) — POSIX signal + systemd

```pascal
{$APPTYPE CONSOLE}
{$IFDEF LINUX}
procedure HandleSignal(ASignal: Integer); cdecl;
begin 
  THorse.StopListen; 
end;
{$ENDIF}
begin
  {$IFDEF LINUX} 
  signal(SIGTERM, @HandleSignal); 
  signal(SIGINT, @HandleSignal); 
  {$ENDIF}
  THorse.Listen(9000);
end.
```

systemd unit (`/etc/systemd/system/myhorse.service`):

```ini
[Unit]
After=network.target
[Service]
Type=simple
ExecStart=/opt/myhorse/MyDaemon
Restart=on-failure
[Install]
WantedBy=multi-user.target
```

### Windows Service (Delphi) — TService

```pascal
procedure TMyHorseService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  FListenerThread := TThread.CreateAnonymousThread(
    procedure 
    begin 
      THorse.Listen(9000); 
    end);
  FListenerThread.FreeOnTerminate := False;
  FListenerThread.Start;
  Started := True;
end;

procedure TMyHorseService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  THorse.StopListen;
  FListenerThread.WaitFor;
  FreeAndNil(FListenerThread);
  Stopped := True;
end;
```

Install / uninstall via the standard SCM verbs:

```bat
MyHorseServer.exe /install
sc start MyHorseService
sc stop  MyHorseService
MyHorseServer.exe /uninstall
```

Simpler alternative without writing a TService: build the Console binary above and wrap with [NSSM](https://nssm.cc/) — `nssm install MyHorseService C:\path\to\Console.exe`. NSSM sends `Ctrl+Break` on stop, which the `CtrlHandler` from the Console shape catches.

### Linux daemon (FPC) — `fpSignal` + systemd

```pascal
{$MODE DELPHI}{$H+}
{$APPTYPE CONSOLE}
{$IFDEF UNIX}
procedure HandleSignal(ASignal: cint); cdecl;
begin 
  THorse.StopListen; 
end;
{$ENDIF}
begin
  {$IFDEF UNIX} 
  fpSignal(SIGTERM, @HandleSignal); 
  fpSignal(SIGINT, @HandleSignal); 
  {$ENDIF}
  THorse.Listen(9000);
end.
```

systemd unit identical to the Delphi Linux daemon above.

### LCL (FPC / Lazarus) — `FormCreate` / `FormClose`

```pascal
procedure TfrmMain.FormCreate(Sender: TObject);
begin 
  THorse.Listen(9000); 
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin 
  THorse.StopListen; 
end;
```

### FPC HTTPApplication — Console-shape FPC binary

Same code as the FPC Linux daemon — `THorse.Listen` owns the loop; don't call `fphttpapp.Application.Run`. If a library expects `TFPHTTPApplication` to exist, instantiate it but leave its `Run` alone.

---

## Stop-signal mapping

The shutdown signal differs by OS and supervisor. All cases end in the same place: `THorse.StopListen` → the active Provider's SEC-30 active-request drain (implemented identically in `horse-provider-crosssocket` and `horse-provider-mormot`) → `Listen` returns → process exits cleanly.

| Supervisor | Signal sent | How your code catches it |
|---|---|---|
| Terminal (Ctrl-C) | `SIGINT` / `CTRL_C_EVENT` | `SetConsoleCtrlHandler` (Windows) / `signal` / `fpSignal` (POSIX) |
| systemd | `SIGTERM` | POSIX `signal` / `fpSignal` handler |
| Windows SCM | `SERVICE_CONTROL_STOP` | `TService.OnStop` event |
| NSSM | `Ctrl+Break` (via console) | `SetConsoleCtrlHandler` catching `CTRL_BREAK_EVENT` |
| VCL / LCL window close | `WM_CLOSE` | `TForm.OnClose` event |
| Docker `docker stop` | `SIGTERM` (then `SIGKILL` after grace period) | POSIX `signal` / `fpSignal` handler — give your handler < 10 s to return |

---

## Common gotchas

| Symptom | Cause | Fix |
|---|---|---|
| Process exits immediately after start | Console binary with no signal handler reaches `end.` after `Listen` returns | Add the `CtrlHandler` / `signal` setup *before* `Listen`. |
| VCL form freezes on startup | `{$APPTYPE CONSOLE}` accidentally left in the .dpr | Remove that directive; VCL/LCL apps must have `IsConsole = False`. |
| Windows service hangs in "Starting" state | `ServiceStart` blocks because `Listen` was called directly on the SCM thread | Wrap `Listen` in `TThread.CreateAnonymousThread` (see TService snippet). |
| systemd reports "main process exited, code=killed, status=15/TERM" | Process didn't catch `SIGTERM` — systemd had to escalate | Install the POSIX signal handler so the binary exits cleanly under `0`. |
| `Address already in use` after restart | Previous process held the socket and was force-killed (no clean drain) | Always call `StopListen`; for Docker, set `--stop-grace-period=30s`. |
| In-progress requests lost on shutdown | `Listen` returned immediately after `StopListen` without waiting for the active-request counter | SEC-30 already handles this — make sure you're on `horse-provider-crosssocket >= 1.0.4` against a recent `winddriver/Delphi-Cross-Socket` (or the [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3) fork), or any release of `horse-provider-mormot`, where SEC-30 has been built in from day one. |
| **~60 % HTTP 500 under load on Indy** (`EWebBrokerException: "Maximum number of concurrent connections exceeded"`), only with response-header middleware + keep-alive + concurrency ≥ ~40 | WebBroker's module-pool `MaxConnections` defaulted to **32** | **Fixed by default** — the Indy providers now raise the ceiling to `DEFAULT_MAX_CONNECTIONS` (1024) when `THorse.MaxConnections` is unset. Set `THorse.MaxConnections := N` to go higher. (Indy only; see [Providers §10.2](./providers.md#102-indy-provider-connection-defaults--maxconnections--listenqueue).) |
| Connections refused / dropped under bursts on Indy | Indy's `ListenQueue` defaulted to **15** | **Fixed by default** — Indy providers now use `DEFAULT_LISTEN_QUEUE` (511) when `THorse.ListenQueue` is unset; raise the OS `somaxconn` to match for very high concurrency. |

---

## Multi-OS deployment

Most teams ship the same Horse code as **Linux daemon** in production and a **Windows Service** or **Console** binary for dev. The Conditional Defines stay the same — only the project target changes (Win64 / Linux64). Build twice, once per OS.

Shared CI config:

```yaml
jobs:
  build-linux:
    runs-on: ubuntu-latest
    steps: [...]
    env:
      CONFIGURATION: Release
      PLATFORM: Linux64
      DEFINES: HORSE_PROVIDER_CROSSSOCKET

  build-windows:
    runs-on: windows-latest
    steps: [...]
    env:
      CONFIGURATION: Release
      PLATFORM: Win64
      DEFINES: HORSE_PROVIDER_CROSSSOCKET
```

The same `.dpr` compiles on both — only the deployment shell (systemd unit vs. SCM service registration) differs per OS.

---

## HTTPS / TLS runtime — what to ship per OS

All three self-hosted TLS providers — `HORSE_PROVIDER_CROSSSOCKET`, `HORSE_PROVIDER_MORMOT`, and `HORSE_PROVIDER_ICS` — use **OpenSSL** for HTTPS. CrossSocket and mORMot `dlopen` / `LoadLibrary` the system shared library at startup; ICS ships its own OpenSSL 3.x/4.x libraries with the distribution. The transport stack itself is in your binary; OpenSSL is **not** statically linked by default (except via mORMot's `mormot2static`). All three enable TLS the same way — `Config.SSLEnabled := True` plus a cert/key on the config record, passed via `ListenWithConfig` — and all three support mutual TLS via `SSLVerifyPeer` + a CA file. Plan your deployment accordingly.

### Linux

Install OpenSSL via the distro package manager so `libssl.so` and `libcrypto.so` are on the loader path:

```bash
# Debian / Ubuntu (22.04+, OpenSSL 3.x)
apt install libssl3 libcrypto3

# Debian / Ubuntu (20.04, OpenSSL 1.1.x)
apt install libssl1.1

# RHEL / Rocky / Alma 9.x  (OpenSSL 3.x)
dnf install openssl-libs

# Alpine
apk add openssl libcrypto3 libssl3
```

CrossSocket and mORMot accept either 1.1.x or 3.x — they probe at startup. If the loader can't find either, the binary still runs but `SSLEnabled := True` fails at `Listen`-time with a clear "no SSL backend available" error. ICS targets OpenSSL 3.x/4.x and bundles the libraries it needs.

For air-gapped or minimal container deployments where you cannot rely on the distro packages:
- **CrossSocket:** ship the matching `libssl.so` + `libcrypto.so` alongside your binary and declare them in the systemd unit's `[Service]` section: `Environment="LD_LIBRARY_PATH=/opt/yourapp"`.
- **mORMot2:** the `mormot2static` package includes a static-link variant for some platforms (`mormot2static/static/x86_64-linux` for FPC) — see [horse-provider-mormot's samples/tests/README](https://github.com/freitasjca/horse-provider-mormot/blob/master/samples/tests/README.md) for the full Search-path setup.
- **ICS:** Delphi only (Windows + Linux64). Ship the OpenSSL `.so` (Linux) / `.dll` (Windows) that ship with the ICS distribution next to your binary.

> **mutual TLS (mTLS).** All three providers verify client certificates when `Config.SSLVerifyPeer := True` and a CA file is set (`SSLCACertFile` on CrossSocket/mORMot, `SSLCAFile` on ICS). Each provider's `tests/TLS-TESTS.md` has a runnable one-way + mTLS integration test. CrossSocket server-side mTLS additionally needs the `Net.CrossSslSocket.*` patches or the fork release (see its README).

### Windows

Ship the OpenSSL DLLs **next to your `.exe`** (not into `C:\Windows\System32` and not into a globally-PATHed folder — co-locating them avoids hijacking by other apps' bundled OpenSSL):

| OpenSSL version | DLL names (per-arch) |
|---|---|
| 1.1.x | `libssl-1_1-x64.dll`, `libcrypto-1_1-x64.dll` (Win64); drop `-x64` for Win32 |
| 3.x | `libssl-3-x64.dll`, `libcrypto-3-x64.dll` (Win64); drop `-x64` for Win32 |

The standard source is [the official OpenSSL Windows builds](https://wiki.openssl.org/index.php/Binaries) or [SLProWeb's installers](https://slproweb.com/products/Win32OpenSSL.html). Pick **one** version and use it everywhere — code that dynamic-loads `libssl-1_1.dll` will not run on a host that only has `libcrypto-3.dll`, and the two cannot coexist within the same process.

For Windows Service deployments, the DLLs must be in the same folder as the service `.exe` — the SCM does **not** inherit the user's `PATH`.

### Common gotcha — version-mismatch crashes

| Symptom | Cause | Fix |
|---|---|---|
| `EOSError: failed to load libssl` on Linux | No OpenSSL package installed, or runtime is stripped to a minimal container with only `libc` | Install `libssl3` / `libssl1.1` (Linux) or copy the DLLs next to the binary (Windows). |
| HTTPS works in dev, fails in prod with "wrong version number" | Dev box has OpenSSL 3.x; prod box has 1.1.x (or vice versa) — TLS negotiation features differ | Pin to one version family across environments. If you must support both, ship the DLLs (Windows) or use the `mormot2static` static-link variant (mORMot2, Linux). |
| Random SIGSEGV during TLS handshake on Linux | Two copies of `libcrypto` loaded at once (system 3.x + a different bundled 1.1.x in `LD_LIBRARY_PATH`) | Ensure only one OpenSSL ABI is reachable. |

---

## See also

- [Providers & Application types](./providers.md) — the full architectural model, including §8 (CrossSocket) and §9 (mORMot2) with annotated code for every shape.
- [Getting Started](./getting-started.md) — your first Horse server before deciding on a deployment shape.
- [Compiler Support](./compiler-support.md) — Delphi / FPC version requirements and the platform matrix.
- [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket) — config (TLS, body-size limits, IO thread count) for the CrossSocket Provider.
- [`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot) — config (thread pool, max body bytes, drain timeout, server banner) for the mORMot2 Provider.
