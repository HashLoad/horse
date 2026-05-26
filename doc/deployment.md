# Deployment Cheatsheet

*Read this in [English](./deployment.md) or [Português (BR)](./deployment.pt-BR.md).*

Same Horse code, seven deployment shapes. This page is the at-a-glance reference; for the rationale and longer code samples, see [Providers & Application types §8](./providers.md#8-running-crosssocket-as-each-application-type).

---

## The four-step pattern

Every shape on this page follows the same four steps:

1. **Define `HORSE_PROVIDER_CROSSSOCKET`** in Project Options → Conditional Defines, plus the matching `HORSE_APPTYPE_*` if you want the cross-product convenience unit (since PATCH-HORSE-2). The legacy `HORSE_CROSSSOCKET` alias still works for backwards compatibility.
2. Pick the **project type** for the desired shape.
3. Call `THorse.Listen(port)` from the **right lifecycle hook** for that shape.
4. Call `THorse.StopListen` from the **shutdown hook** so CrossSocket drains in-flight requests.

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
  begin THorse.StopListen; Result := True; end
  else Result := False;
end;
begin
  SetConsoleCtrlHandler(@CtrlHandler, True);
  THorse.Listen(9000);
end.
```

### VCL (Delphi) — `FormCreate` / `FormClose`

```pascal
procedure TfrmMain.FormCreate(Sender: TObject);  begin THorse.Listen(9000); end;
procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
                                                begin THorse.StopListen;   end;
```

### Linux daemon (Delphi) — POSIX signal + systemd

```pascal
{$APPTYPE CONSOLE}
{$IFDEF LINUX}
procedure HandleSignal(ASignal: Integer); cdecl;
begin THorse.StopListen; end;
{$ENDIF}
begin
  {$IFDEF LINUX} signal(SIGTERM, @HandleSignal); signal(SIGINT, @HandleSignal); {$ENDIF}
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
    procedure begin THorse.Listen(9000); end);
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
begin THorse.StopListen; end;
{$ENDIF}
begin
  {$IFDEF UNIX} fpSignal(SIGTERM, @HandleSignal); fpSignal(SIGINT, @HandleSignal); {$ENDIF}
  THorse.Listen(9000);
end.
```

systemd unit identical to the Delphi Linux daemon above.

### LCL (FPC / Lazarus) — `FormCreate` / `FormClose`

```pascal
procedure TfrmMain.FormCreate(Sender: TObject);
begin THorse.Listen(9000); end;
procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin THorse.StopListen; end;
```

### FPC HTTPApplication — Console-shape FPC binary

Same code as the FPC Linux daemon — `THorse.Listen` owns the loop; don't call `fphttpapp.Application.Run`. If a library expects `TFPHTTPApplication` to exist, instantiate it but leave its `Run` alone.

---

## Stop-signal mapping

The shutdown signal differs by OS and supervisor. All cases end in the same place: `THorse.StopListen` → CrossSocket SEC-30 active-request drain → `Listen` returns → process exits cleanly.

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
| In-flight requests dropped on shutdown | `Listen` returned immediately after `StopListen` without waiting for the active-request counter | CrossSocket's SEC-30 already handles this — make sure you're on `horse-provider-crosssocket >= 1.0.4`. |

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
      DEFINES: HORSE_CROSSSOCKET

  build-windows:
    runs-on: windows-latest
    steps: [...]
    env:
      CONFIGURATION: Release
      PLATFORM: Win64
      DEFINES: HORSE_CROSSSOCKET
```

The same `.dpr` compiles on both — only the deployment shell (systemd unit vs. SCM service registration) differs per OS.

---

## See also

- [Providers & Application types](./providers.md) — the full architectural model, including §8 with annotated code for every shape.
- [Getting Started](./getting-started.md) — your first Horse server before deciding on a deployment shape.
- [Compiler Support](./compiler-support.md) — Delphi / FPC version requirements and the platform matrix.
- [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket) — config (TLS, body-size limits, IO thread count) for the CrossSocket Provider.
