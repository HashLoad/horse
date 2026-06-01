# Getting Started

*Read this in [English](./getting-started.md) or [Português (BR)](./getting-started.pt-BR.md).*

This page takes you from a blank project to a running Horse server in under five minutes. It assumes you have **Delphi 10.4 or later** (or **Lazarus 2.2 / FPC 3.2+**) and a working internet connection for Boss to fetch dependencies.

If anything below doesn't apply to your environment, check [Compiler Support](./compiler-support.md) first.

---

## 1. Install Boss

[Boss](https://github.com/HashLoad/boss) is the package manager Horse uses for distribution. One-time install:

```sh
# Windows (PowerShell, run as administrator)
iwr https://github.com/HashLoad/boss/releases/latest/download/boss-windows.exe -OutFile boss.exe
Move-Item boss.exe C:\Windows\System32\

# Linux / macOS
curl -L https://github.com/HashLoad/boss/releases/latest/download/boss-linux -o boss
chmod +x boss
sudo mv boss /usr/local/bin/
```

Verify:

```sh
boss --version
```

## 2. Create a project and add Horse

Create a fresh Delphi console application (or Lazarus project), save it, then from a terminal in the project directory:

```sh
boss init -q                  # writes a boss.json
boss install horse            # adds Horse to the project search path
```

Boss fetches the latest stable Horse, drops it into `modules/horse/`, and adds the source path to your `.dproj` / `.lpi` automatically. You can now `uses Horse;` from any source file.

## 3. Minimal server — Delphi

```delphi
program HelloHorse;

{$APPTYPE CONSOLE}

uses
  Horse;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  WriteLn('Listening on http://127.0.0.1:9000/ping');
  THorse.Listen(9000);
end.
```

Compile, run, then in another terminal:

```sh
curl http://127.0.0.1:9000/ping
# pong
```

That's it. Press `Ctrl-C` to stop.

## 4. Minimal server — Lazarus / FPC

```pascal
program HelloHorse;

{$MODE DELPHI}{$H+}

uses
  Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  THorse.Get('/ping', @GetPing);
  THorse.Listen(9000);
end.
```

Two FPC-specific notes:

- `{$MODE DELPHI}{$H+}` is required so anonymous procedures and `string = AnsiString` semantics match Delphi.
- Route callbacks take an `@` because Lazarus distinguishes procedure values from procedure references.

## 5. Project structure conventions

You don't have to follow these — Horse imposes no layout — but the official samples use them:

```
my-server/
├── boss.json                ← Boss manifest
├── boss-lock.json           ← Pinned versions (auto-generated)
├── modules/                 ← Boss-installed dependencies (gitignored)
├── src/
│   ├── HelloHorse.dpr       ← Entry point (registers routes, calls Listen)
│   ├── Controllers/         ← One unit per resource (Users, Products, ...)
│   ├── Services/            ← Business logic, no Horse dependency
│   └── Middlewares/         ← Cross-cutting concerns
└── tests/
    └── ...
```

A typical entry point composes the application:

```delphi
uses
  Horse, Horse.Jhonson, Horse.CORS,
  Controllers.Users, Controllers.Products;

begin
  THorse
    .Use(Jhonson)                 // JSON body parsing
    .Use(CORS);                    // permissive CORS

  Controllers.Users.RegisterRoutes;
  Controllers.Products.RegisterRoutes;

  THorse.Listen(9000);
end.
```

…and each controller exposes a `RegisterRoutes` procedure that calls `THorse.Get/Post/Put/…`.

## 6. Same code, seven deployment shapes

The Console hello-world above is the simplest deployment shape, but **the same `THorse.Get` / `THorse.Listen` code can ship as any of these**, by changing only the project type and Conditional Defines:

| You want to ship as… | Compile | Add a `HORSE_*` define? | One-line shutdown hook |
|---|---|---|---|
| Console binary | Delphi or FPC | (none) — what you just built | `SetConsoleCtrlHandler` (Win) / `signal SIGTERM` (Linux) |
| VCL desktop app embedding a server | Delphi | (none — write a Forms project) | `FormClose` |
| Linux daemon | Delphi or FPC, target Linux64 | (none — console binary + systemd) | `signal SIGTERM` / `fpSignal SIGTERM` |
| Windows Service | Delphi (Service Application) | (none — write a `TService` project) | `TService.OnStop` |
| Lazarus LCL desktop app | FPC | (none — write a Lazarus project) | `FormClose` |
| Apache module | Delphi | `HORSE_APACHE` | (Apache owns the lifecycle) |
| IIS ISAPI extension | Delphi | `HORSE_ISAPI` | (IIS owns the lifecycle) |
| CGI / FastCGI binary | Delphi or FPC | `HORSE_CGI` / `HORSE_FCGI` | (host owns the lifecycle) |

For the high-performance path, add **one** of the following Provider defines alongside (where the table says "none") to switch the transport from the default Indy / `fphttpserver` to an async Provider:

| Provider define | Backed by | When to pick it |
|---|---|---|
| `HORSE_PROVIDER_CROSSSOCKET` | [`winddriver/Delphi-Cross-Socket`](https://github.com/winddriver/Delphi-Cross-Socket) (upstream) + [`cnpack/cnvcl`](https://github.com/cnpack/cnvcl) for CnPack/Crypto units — IOCP / epoll / kqueue. Install both manually (mirroring the mORMot2 setup). For mTLS server mode (`SSLVerifyPeer = True`), use the supported alternative [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3) which bundles CnPack and adds `SetCACertificateFile` + `SetVerifyPeer` in one clone. | You prefer native async control or already depend on Delphi-Cross-Socket. Requires Delphi 10.2+. |
| `HORSE_PROVIDER_MORMOT` | [mORMot2](https://github.com/synopse/mORMot2) `THttpServer` — IOCP / epoll | You want Delphi 7+ compatibility, http.sys kernel-mode HTTP, or pure-Pascal HTTP without compiled C deps. |

The two Providers are mutually exclusive (one transport per build). The legacy alias `HORSE_CROSSSOCKET` keeps working forever (PATCH-HORSE-2 translates it to `HORSE_PROVIDER_CROSSSOCKET`); there is no legacy alias for mORMot — it's new.

Concrete recipes (project type, code skeleton, install commands) for each shape: [Deployment Cheatsheet](./deployment.md), or the longer-form [Providers & Application types §8 (CrossSocket)](./providers.md#8-running-crosssocket-as-each-application-type) / [§9 (mORMot2)](./providers.md#9-running-mormot2-as-each-application-type).

## 7. Where to next

- [Routing](./routing.md) — declare endpoints, path parameters, route groups.
- [Request & Response](./request-response.md) — read input, write output.
- [Middleware](./middleware.md) — JSON parsing, CORS, JWT, logging.
- [Providers & Application types](./providers.md) — when to switch off the default Indy transport. The CrossSocket and mORMot2 sections cover the two high-concurrency alternatives.
- [Deployment Cheatsheet](./deployment.md) — one-pager for shipping the binary as any of the seven shapes above.

## Troubleshooting

| Symptom | Likely cause |
|---|---|
| `Unit not found Horse` | `boss install horse` didn't run — re-run it from the project directory. |
| Server starts then exits immediately | The `Listen` call is non-blocking on some platforms; in a console app make sure the main thread doesn't fall through to `end.` Wrap it with a `ReadLn` or use a signal handler. |
| Port already in use | Another service holds `:9000`. Pick another port or stop the conflict. |
| `Address already in use` after a previous run crashed | The previous process didn't release the socket. Wait 30 s for `TIME_WAIT`, or change the port. |
| Compilation fails on FPC with "method-pointer expected" | Missing `@` before the procedure name in `THorse.Get(...)`. |
