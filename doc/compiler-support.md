# Compiler Support

*Read this in [English](./compiler-support.md) or [Português (BR)](./compiler-support.pt-BR.md).*

Horse targets a broad range of Delphi and Free Pascal versions. This page lists what's supported, what's tested, and the per-Provider / per-Application-type considerations.

For the two-axis model (Provider — Indy / fphttpserver / CrossSocket — × Application type — Console / VCL / Daemon / Apache / ISAPI / CGI / FCGI / LCL / HTTPApplication), see [Providers & Application types](./providers.md).

---

## Delphi

| Version | Compiler version | Status |
|---|---|---|
| Delphi 13 Florence | 38.0 | Supported |
| Delphi 12 Athens | 36.0 | **Recommended** |
| Delphi 11 Alexandria | 35.0 | Supported |
| Delphi 10.4 Sydney | 34.0 | Supported |
| Delphi 10.3 Rio | 33.0 | Supported |
| Delphi 10.2 Tokyo | 32.0 | Supported |
| Delphi 10.1 Berlin | 31.0 | Supported |
| Delphi 10 Seattle | 30.0 | Supported |
| Delphi XE8 | 29.0 | Supported |
| Delphi XE7 | 28.0 | Minimum |

CI runs on Delphi 11 and 12 against the official `tests/` suite. Anything below 10.4 receives source-compatibility consideration but isn't actively exercised.

## Free Pascal / Lazarus

| Version | Status |
|---|---|
| FPC 3.2.2 + Lazarus 2.2 | **Recommended** |
| FPC 3.2.0 + Lazarus 2.0 | Supported |
| FPC 3.3.1 (trunk) | Supported with `{$MODE DELPHI}{$H+}` |

## Target platforms

The platform set depends on which Provider is selected and which Application type is being built. The two tables below match the [two-axis model](./providers.md).

### Self-hosted Providers × Platform

The Provider that owns the socket. Indy is Delphi-only; `fphttpserver` is FPC-only; CrossSocket spans both.

| Platform | Indy _(Delphi default)_ | `fphttpserver` _(FPC default)_ | CrossSocket _(`HORSE_PROVIDER_CROSSSOCKET`; legacy alias `HORSE_CROSSSOCKET`)_ |
|---|:---:|:---:|:---:|
| Windows x86 / x64 | ✔ | ✔ | ✔ |
| Linux x64 | ✔ | ✔ | ✔ _(primary target)_ |
| macOS Intel / ARM64 | ✔ | ✔ | ✔ |
| FreeBSD | — | ✔ | ✔ _(via kqueue)_ |
| Android / iOS | — | — | — |

### Host-managed Application types × Platform

These don't use a self-hosted Provider — the host process owns the socket. Coverage depends on what the host runs on.

| Platform | Apache module | ISAPI _(IIS)_ | CGI | FastCGI |
|---|:---:|:---:|:---:|:---:|
| Windows x86 / x64 | ✔ | ✔ | ✔ | ✔ |
| Linux x64 | ✔ | — | ✔ | ✔ |
| macOS Intel / ARM64 | — | — | ✔ | — |
| FreeBSD | — | — | ✔ | — |
| Android / iOS | — | — | — | — |

(Server-side targets only — for HTTP **client** code, Delphi's HTTP libraries support broader mobile platforms.)

## Per-Provider notes (self-hosted)

### Indy — Delphi default

- Works on every Delphi version listed above. **Not available on FPC** — FPC builds use `fphttpserver` instead.
- Provider unit: `Horse.Provider.Console` / `Horse.Provider.VCL` / `Horse.Provider.Daemon`. Each `uses IdHTTPWebBrokerBridge, IdContext, IdSSLOpenSSL, …`.
- OpenSSL DLLs (`libeay32.dll` / `ssleay32.dll` or `libssl-1_1-x64.dll` / `libcrypto-1_1-x64.dll`) must accompany the binary for HTTPS.

### `fphttpserver` — FPC default

- Works on every FPC version listed above. **Not available on Delphi** — Delphi builds use Indy instead.
- Provider unit: `Horse.Provider.FPC.Daemon` / `Horse.Provider.FPC.HTTPApplication` / `Horse.Provider.FPC.LCL`. Each `uses fphttpserver, fpHTTP, httpdefs`.
- SSL via OpenSSL through FPC's standard HTTP SSL handlers — no Indy dependency.

### CrossSocket — optional, cross-compiler

- Requires **Delphi 10.2 Tokyo or later** (the `Delphi-Cross-Socket` library uses inline `var` and `System.Net` types not present in earlier versions).
- Requires **FPC 3.2.0 or later** for FPC builds.
- Tested platforms: Windows x64, Linux x64, macOS ARM64.
- Replaces Indy (Delphi) and `fphttpserver` (FPC) with a single async transport for both compilers.
- See [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket#readme) for the current per-version test matrix.

## Host-managed Application types

Apache / ISAPI / CGI / FastCGI **do not use a self-hosted Provider** — neither Indy, `fphttpserver`, nor CrossSocket is involved. The host process (Apache httpd, IIS, the web server) owns the socket and hands the request to Horse via `Web.HTTPApp` (Delphi) or `fpFCGI` / `fpHTTP` (FPC) subclasses.

- **Apache module** — Delphi (`Web.HTTPD24Impl`, `Web.ApacheApp`). Build the `.so` / `.dll` matching the Apache architecture.
- **ISAPI extension** — Delphi only (`Web.Win.ISAPIApp`). Windows + IIS. Match the architecture of the IIS application pool (32-bit pool → Win32 build).
- **CGI** — Delphi (`Web.CGIApp`) and FPC. Cross-platform; one process per request.
- **FastCGI** — FPC (`fpFCGI`). Persistent process; talks to the web server via Unix socket or TCP. Delphi FCGI requires a third-party library and is not part of the shipped Horse providers.

## Compiler-version guards in the source

Horse uses a few defensive guards. If you're contributing patches:

- `{$IF DEFINED(FPC)}` — branch FPC-only code (different RTL, different generics syntax).
- `{$IF CompilerVersion >= 32.0}` — Delphi 10.2 Tokyo introduced `Int64` return types on `TWebRequest.GetIntegerVariable` / `TWebResponse.SetIntegerVariable`. Anything that overrides those needs the guard.
- `{$IF CompilerVersion >= 33.0}` — Delphi 10.3 Rio introduced inline `var`. Horse core avoids inline `var` so it remains compilable on XE7.

For every change, mentally compile against both `dcc32` (Delphi) and `fpc` (FPC). Anonymous procedures, generics, and Web/HTTPApp types are the most common cross-compiler trip points.

## Reporting a version-specific bug

When opening an issue:

1. Include the **exact Delphi / FPC version** and the **target platform** (Win64, Linux64, …).
2. Include the **provider define(s)** active in your project.
3. State whether the bug is reproducible on the matching provider sample under `samples/`.

The triage is usually quick once the platform and provider are pinned.

## See also

- [Getting Started](./getting-started.md) — install paths per IDE.
- [Providers & Application types](./providers.md) — the two-axis model: which Provider × Application-type combinations exist, and which units each one `uses`.
