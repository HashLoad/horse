<p align="center">
  <a href="https://github.com/HashLoad/horse/blob/master/img/horse.png">
    <img alt="Horse" height="150" src="https://github.com/HashLoad/horse/blob/master/img/horse.png">
  </a>
</p><br>
<p align="center">
  <b>Horse</b> is an <a href="https://github.com/expressjs/express">Express</a>-inspired <b>web framework</b> for Delphi and Lazarus.<br>
  Designed to <b>ease</b> things up for <b>fast</b> development in a <b>minimalist</b> way and with high <b>performance</b>.
</p><br>
<p align="center">
  <a href="https://t.me/hashload">
    <img src="https://img.shields.io/badge/telegram-join%20channel-7289DA?style=flat-square">
  </a>
</p>

<p align="center">
  <i>Read this in <a href="./README.md">English</a> or <a href="./README.pt-BR.md">Português (BR)</a>.</i>
</p>

## ⚙️ Installation

Installation is done using the [`boss install`](https://github.com/HashLoad/boss) command:

```sh
boss install horse
```

*(Optional)* Install the [**Horse Wizard**](https://github.com/HashLoad/horse-wizard) for IDE integration.

## ⚡️ Quickstart Delphi

```delphi
uses Horse;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

## ⚡️ Quickstart Lazarus

```delphi
{$MODE DELPHI}{$H+}

uses Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Pong');
end;

begin
  THorse.Get('/ping', GetPing);
  THorse.Listen(9000);
end.
```

## 📖 Documentation

The full guide lives in [`doc/`](./doc/index.md) — a small wiki that complements the quick reference below:

| Topic | Read |
|---|---|
| First server, install paths, Delphi/Lazarus setup | [Getting Started](./doc/getting-started.md) |
| Defining routes, route params, route groups, query strings | [Routing](./doc/routing.md) |
| `THorseRequest` / `THorseResponse` — body, headers, cookies, sessions, status, streaming | [Request & Response](./doc/request-response.md) |
| Using middleware, registration order, the `Next` proc | [Middleware](./doc/middleware.md) |
| **Writing & publishing your own middleware** — skeleton, thread safety, Provider neutrality, Boss packaging | [**Writing a Middleware**](./doc/writing-middleware.md) |
| **Choosing a transport provider** — Indy (default), CrossSocket, mORMot2, ICS, HttpSys, Apache, ISAPI, CGI, daemons | [**Providers**](./doc/providers.md) |
| **Deploy** as Console / VCL / Daemon / Windows Service / LCL / HTTPApplication — one-page recipe | [**Deployment Cheatsheet**](./doc/deployment.md) |
| Full middleware catalogue with extended descriptions | [Middleware Ecosystem](./doc/middleware-ecosystem.md) |
| Automated integration, resilience (Access Violation) and SO limit testing | [Integrity Testing](./doc/integrity-testing.md) |
| Supported Delphi / FPC versions and platforms | [Compiler Support](./doc/compiler-support.md) |

## 🔌 Providers (transport layer)

A _provider_ is the HTTP transport that owns the socket and hands requests to your route handlers. **The same handler code runs under any provider** — you select one at compile time via a Conditional Define. The default Provider depends on the compiler: **Indy** on Delphi (for Console / VCL / Daemon), **`fphttpserver`** on FPC (for Daemon / HTTPApplication / LCL). The optional **CrossSocket** and **mORMot2** Providers replace both with async **IOCP / epoll / kqueue** I/O; the optional **ICS** Provider (Delphi; Windows + Linux64/macOS) swaps in OverbyteICS's modern **OpenSSL 3.x / 4.x** stack — TLS 1.3, SNI, mTLS. The **HttpSys** Provider (Windows) is **built into Horse** — it drives the OS's **http.sys** kernel-mode HTTP stack (the same one IIS uses) with no external library.

| Provider | Compiler define | Delphi | Lazarus |
| ----------------------------------------------------------------------------------------------- | ----------------------- | :------------------: | :-------------------------: |
| **Indy** _(Delphi default for self-hosted)_                                                     | _(none)_                | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;n/a |
| **`fphttpserver`** _(FPC default for self-hosted)_                                              | _(none)_                | &nbsp;&nbsp;&nbsp;n/a | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[horse-provider-crosssocket](https://github.com/freitasjca/horse-provider-crosssocket)**    | `HORSE_CROSSSOCKET`     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[horse-provider-mormot](https://github.com/freitasjca/horse-provider-mormot)**               | `HORSE_PROVIDER_MORMOT` | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[HTTP.sys](./doc/httpsys.md)** _(Windows kernel-mode driver for ultra-low latency)_        | `HORSE_PROVIDER_HTTPSYS` | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[epoll](./doc/epoll.md)** _(Linux-native asynchronous event loop)_                         | `HORSE_PROVIDER_EPOLL`   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[horse-provider-ics](https://github.com/freitasjca/horse-provider-ics)** _(Delphi; Win + Linux/macOS)_     | `HORSE_PROVIDER_ICS`    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |

> **Note** — Apache / ISAPI / CGI / FastCGI Application types (below) do **not** use any of these Providers. The host process (Apache, IIS, the web server) owns the socket; Horse runs in-process. See [Providers & Application types](./doc/providers.md) for the full model.

> **Delphi-Cross-Socket installation** — clone [`winddriver/Delphi-Cross-Socket`](https://github.com/winddriver/Delphi-Cross-Socket) (upstream) **plus** [`cnpack/cnvcl/.../Crypto`](https://github.com/cnpack/cnvcl/tree/master/Source/Crypto) for the required CnPack/Crypto units, and add search paths to your project. Three previously-fork-only bug fixes have been merged into upstream as of 2026-Q2, so the upstream mainline is correct for general use. For server-side **mutual TLS** (`SSLVerifyPeer = True` + `SSLCACertFile = ...`) use the pre-built release [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3) — single clone, CnPack bundled, mTLS APIs (`SetCACertificateFile` + `SetVerifyPeer`) ready to use. See [horse-provider-crosssocket Installation](./doc/providers.md#crosssocket-optional) for the full two-path breakdown.

> **OverbyteICS installation** — the ICS Provider requires [OverbyteICS](https://wiki.overbyte.eu/wiki/index.php/ICS_Download) (v9.x). **Install ICS following the official ICS instructions** — download/clone ICS and add its `Source/` folder to your project search path (ICS is not Boss-installable). For TLS, the OpenSSL libraries ship with ICS (DLLs on Windows, `.so` on Linux). The ICS Provider is **Delphi only — Windows and POSIX (Linux64 / macOS)** via ICS's own `Ics.Posix.*` message pump (on Linux use `HORSE_APPTYPE_DAEMON` + `THorseICSLinuxDaemonApp.Run`); a **Lazarus/FPC** port is not viable — ICS's POSIX layer rides the Delphi POSIX RTL and ICS compiles out OpenSSL under FPC. Its distinctive value is ICS's OpenSSL 3.x / 4.x stack (TLS 1.3, SNI, mTLS). See [horse-provider-ics](https://github.com/freitasjca/horse-provider-ics) for setup, the A–K test suite, and known limitations.

> **HttpSys** — **no install**: the `Horse.Provider.HttpSys` unit ships with Horse and binds directly to Windows' `httpapi.dll` (http.sys), so there's no external library. Set `HORSE_PROVIDER_HTTPSYS` (Windows; Delphi or Lazarus). Because http.sys is a kernel-mode, machine-wide HTTP stack, binding a non-`localhost` host or a privileged port needs a one-time URL reservation (`netsh http add urlacl url=http://+:9000/ user=Everyone`) or Administrator rights; HTTPS uses the Windows certificate store via `netsh http add sslcert`. It is mutually exclusive with the CrossSocket / mORMot / ICS Providers (one transport per build).

## 🎯 Application types

How the binary is packaged and started. **Self-hosted** types run under the chosen Provider above; **host-managed** types delegate the socket to the web server, which then becomes the transport.

| Application type | Compiler define | Delphi | Lazarus |
| ---------------------------------------------------------------------------- | ----------------- | :------------------: | :-------------------------: |
| _**Self-hosted** (uses the selected Provider)_                                                                                                                            |
| [Console](./doc/providers.md) _(default)_                                    | _(none)_          | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| [VCL](./doc/providers.md)                                                    | `HORSE_VCL`       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
| [Daemon — Windows Service](./doc/providers.md)                               | `HORSE_DAEMON`    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;n/a |
| [Daemon — Linux daemon (systemd)](./doc/providers.md)                        | `HORSE_DAEMON`    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| [LCL](./doc/providers.md) (Lazarus GUI)                                      | `HORSE_LCL`       | &nbsp;&nbsp;&nbsp;❌ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| [HTTPApplication](./doc/providers.md) (FPC)                                  | _(FPC default)_   | &nbsp;&nbsp;&nbsp;❌ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| _**Host-managed** (the web server owns the socket; Provider above is unused)_                                                                                            |
| [Apache module](./doc/providers.md)                                          | `HORSE_APACHE`    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| [ISAPI](./doc/providers.md) (IIS)                                            | `HORSE_ISAPI`     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
| [CGI](./doc/providers.md)                                                    | `HORSE_CGI`       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| [FastCGI](./doc/providers.md)                                                | `HORSE_FCGI`      | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |

> **Note** – `HORSE_DAEMON` is a **platform‑adaptive** application type:  
> - On **Windows** → compiles as a Windows Service (`Vcl.SvcMgr.TService` + SCM)  
> - On **Linux** → compiles as a systemd daemon (uses `signal(SIGTERM)` + systemd)  
>  
> “Daemon” is the Unix‑native term; Windows has no exact equivalent, so the same define name is used across platforms.


See [Providers](./doc/providers.md) for the full compatibility matrix and how to combine Provider × Application type.

## 🧬 Official Middlewares

For a more _maintainable_ middleware _ecosystem_, we've put official middlewares into separate repositories:

| Middleware | Delphi | Lazarus |
| ------------------------------------------------------------------- | -------------------- | --------------------------- |
|  [horse/json](https://github.com/HashLoad/jhonson)                  | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/basic-auth](https://github.com/HashLoad/horse-basic-auth)   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/cors](https://github.com/HashLoad/horse-cors)               | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/stream](https://github.com/HashLoad/horse-octet-stream)     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/jwt](https://github.com/HashLoad/horse-jwt)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/exception](https://github.com/HashLoad/handle-exception)    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/logger](https://github.com/HashLoad/horse-logger)           | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/compression](https://github.com/HashLoad/horse-compression) | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |

## 🌱 Third Party Middlewares

This is a list of middlewares that are created by the Horse community, please create a PR if you want to see yours!

| Middleware | Delphi | Lazarus |
| ---------------------------------------------------------------------------------------------------------- | -------------------- | --------------------------- |
|  [bittencourtthulio/etag](https://github.com/bittencourtthulio/Horse-ETag)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [bittencourtthulio/paginate](https://github.com/bittencourtthulio/Horse-Paginate)                         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [bittencourtthulio/cachecontrol](https://github.com/bittencourtthulio/horse-cachecontrol)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [gabrielbaltazar/gbswagger](https://github.com/gabrielbaltazar/gbswagger)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [willhubner/socketIO](https://github.com/WillHubner/Horse-SocketIO)                                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/ratelimit](https://github.com/dliocode/horse-ratelimit)                                         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/slowdown](https://github.com/dliocode/horse-slowdown)                                           | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [giorgiobazzo/upload](https://github.com/giorgiobazzo/horse-upload)                                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/query](https://github.com/dliocode/horse-query)                                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [CarlosHe/healthcheck](https://github.com/CarlosHe/horse-healthcheck)                                     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [CarlosHe/staticfiles](https://github.com/CarlosHe/horse-staticfiles)                                     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [CachopaWeb/horse-server-static](https://github.com/CachopaWeb/horse-server-static)                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [arvanus/horse-exception-logger](https://github.com/arvanus/horse-exception-logger)                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [claudneysessa/Horse-CSResponsePagination](https://github.com/claudneysessa/Horse-CSResponsePagination)   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [claudneysessa/Horse-XSuperObjects](https://github.com/claudneysessa/Horse-XSuperObjects)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [andre-djsystem/horse-bearer-auth](https://github.com/andre-djsystem/horse-bearer-auth)                   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [andre-djsystem/horse-manipulate-request](https://github.com/andre-djsystem/horse-manipulate-request)     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [andre-djsystem/horse-manipulate-response](https://github.com/andre-djsystem/horse-manipulate-response)   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [antoniojmsjr/Horse-IPGeoLocation](https://github.com/antoniojmsjr/Horse-IPGeoLocation)                   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [antoniojmsjr/Horse-XMLDoc](https://github.com/antoniojmsjr/Horse-XMLDoc)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [isaquepinheiro/horse-jsonbr](https://github.com/HashLoad/JSONBr)                                         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [IagooCesaar/Horse-JsonInterceptor](https://github.com/IagooCesaar/Horse-JsonInterceptor)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/horse-datalogger](https://github.com/dliocode/horse-datalogger)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [marcobreveglieri/horse-prometheus-metrics](https://github.com/marcobreveglieri/horse-prometheus-metrics) | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [weslleycapelari/horse-documentation](https://github.com/weslleycapelari/horse-documentation)             | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [weslleycapelari/horse-validator](https://github.com/weslleycapelari/horse-validator)                     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |

## Delphi Versions

`Horse` works with Delphi 13 Florence, Delphi 12 Athens, Delphi 11 Alexandria, Delphi 10.4 Sydney, Delphi 10.3 Rio, Delphi 10.2 Tokyo, Delphi 10.1 Berlin, Delphi 10 Seattle, Delphi XE8 and Delphi XE7.

For the full platform matrix per provider, see [Compiler Support](./doc/compiler-support.md).

## 🤝 Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md) for how to report bugs, suggest features, and submit code or documentation changes. Bilingual EN / PT-BR docs are kept in sync — when editing one language, edit the other in the same PR.

## 💻 Code Contributors

<a href="https://github.com/Hashload/horse/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=Hashload/horse" />
</a>

## ⚠️ License

`Horse` is free and open-source software licensed under the [MIT License](https://github.com/HashLoad/horse/blob/master/LICENSE).

## 📐 Tests

![tests](https://github.com/GlerystonMatos/horse/workflows/tests/badge.svg) ![Console Coverage ](https://img.shields.io/badge/console%20coverage-45%25-blue) ![VCL Coverage ](https://img.shields.io/badge/vcl%20coverage-43%25-blue)
