# Compiler Support

*Read this in [English](./compiler-support.md) or [Português (BR)](./compiler-support.pt-BR.md).*

Horse targets a broad range of Delphi and Free Pascal versions. This page lists what's supported, what's tested, and the per-Provider / per-Application-type considerations.

For the two-axis model (Provider — Indy / fphttpserver / CrossSocket / mORMot2 / ICS / HttpSys — × Application type — Console / VCL / Daemon / Apache / ISAPI / CGI / FCGI / LCL / HTTPApplication), see [Providers & Application types](./providers.md).

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

The Provider that owns the socket. Indy is Delphi-only; `fphttpserver` is FPC-only; CrossSocket and mORMot2 span both; ICS is Delphi-only (Windows + POSIX/Linux64); HttpSys is Windows only (Delphi + FPC), built into Horse.

| Platform | Indy _(Delphi default)_ | `fphttpserver` _(FPC default)_ | CrossSocket _(`HORSE_PROVIDER_CROSSSOCKET`)_ | mORMot2 _(`HORSE_PROVIDER_MORMOT`)_ | ICS _(`HORSE_PROVIDER_ICS`)_ | HttpSys _(`HORSE_PROVIDER_HTTPSYS`)_ |
|---|:---:|:---:|:---:|:---:|:---:|:---:|
| Windows x86 / x64 | ✔ | ✔ | ✔ _(IOCP)_ | ✔ _(IOCP; also http.sys via `THttpApiServer`)_ | ✔ _(OpenSSL 3.x/4.x; TLS 1.3, mTLS)_ | ✔ _(http.sys, kernel-mode; built-in)_ |
| Linux x64 | ✔ | ✔ | ✔ _(epoll, primary target)_ | ✔ _(epoll)_ | ✔ _(Delphi POSIX; `Ics.Posix.*` pump)_ | — |
| macOS Intel / ARM64 | ✔ | ✔ | ✔ _(kqueue)_ | ✔ | ✔ _(Delphi POSIX)_ | — |
| FreeBSD | — | ✔ | ✔ _(kqueue)_ | ✔ _(kqueue)_ | — | — |
| Android / iOS | — | — | — | — | — | — |

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
- **Installation is manual** (mirrors the mORMot2 install pattern, not Boss): clone [`winddriver/Delphi-Cross-Socket`](https://github.com/winddriver/Delphi-Cross-Socket) (upstream) and add its search paths. Upstream does **not** bundle CnPack — also clone [`cnpack/cnvcl`](https://github.com/cnpack/cnvcl) and add `Source/Common` + `Source/Crypto` to the search path (Delphi-Cross-Socket's `Utils.Hash.pas` requires `CnMD5`, `CnSHA1`, `CnSHA2`, `CnSM3`, and `CnPemUtils` plus their transitive deps). Three previously-fork-only bug fixes (`PATCH-IOCP-1`, the zero-body response parser hang, and the `_OnBodyEnd` nil-guard) have all been merged into upstream as of 2026-Q2 — no patches needed on upstream for HTTP and one-way HTTPS.
- **Server-side mutual TLS is still fork-only at the source level.** Upstream `Net.CrossSslSocket.Base.pas` and `Net.CrossSslSocket.OpenSSL.pas` do **not** yet expose the `SetCACertificate(File)` overload chain or the virtual abstract `SetVerifyPeer(Boolean)` that the provider calls when `THorseCrossSocketConfig.SSLVerifyPeer = True`. An upstream PR is in preparation; until it lands, mTLS users must either apply the two `Net.CrossSslSocket.*` patches manually on upstream **or** use the pre-built fork release (next bullet).
- **Supported alternative — fork release** [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3): single clone, bundles CnPack, **and ships the two `Net.CrossSslSocket.*` mTLS patches pre-applied** — `SetCACertificate(File)` + `SetVerifyPeer(Boolean)` are immediately available. Use this when `SSLVerifyPeer = True` or when you prefer the one-dependency convenience over tracking upstream directly. Trade-off: the fork lags upstream's commit history between syncs (typically <24h via the [automated sync workflow](https://github.com/freitasjca/Delphi-Cross-Socket/actions)).
- See [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket#readme) for the full three-path install runbook and the current per-version test matrix.

### mORMot2 — optional, cross-compiler

- Compatible with **Delphi 7 through 12.3 Athens** — the broadest compiler range of any Provider thanks to mORMot's own long-standing legacy support. Horse-side adapter units carry `{$IF CompilerVersion >= 32.0}` guards (Delphi 10.2+) on a small number of `Int64`/`Integer` boundaries; XE7+ otherwise.
- Requires **FPC 3.2.0 or later** for FPC builds.
- Tested platforms: Windows x86 / x64, Linux x64, macOS ARM64.
- Replaces Indy (Delphi) and `fphttpserver` (FPC) with [mORMot2](https://github.com/synopse/mORMot2)'s `THttpServer` (IOCP / epoll) — pure Pascal, no compiled C library dependencies for standard HTTP. On Windows, swap `THttpServer` for `THttpApiServer` to get **kernel-mode http.sys HTTP** with zero code change.
- mORMot2 is **not** available via `boss install` — clone [synopse/mORMot2](https://github.com/synopse/mORMot2) directly and add the search-path entries documented in [`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot#readme).
- On Delphi the build also requires the **precompiled `.obj` blobs** from `mormot2static.7z` ([latest mORMot2 release](https://github.com/synopse/mORMot2/releases/latest)) extracted into `mORMot2\static\delphi\`. Without these the linker fails with `E1026 File not found: '..\..\static\delphi\zlibdeflate.obj'`. The FPC variants use `.o` files under `mORMot2\static\<target>` configured via the project's `-Fl` paths.
- See [`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot#readme) for the current per-version test matrix and configuration record (`THorseMormotConfig`).

### ICS — optional, Delphi (Windows + POSIX/Linux)

- Requires **Delphi 2009 or later** (OverbyteICS's minimum) — recommended **10.4 Sydney+** to match the rest of the stack. **Delphi only**: Windows plus **POSIX (Linux64 / macOS)** via ICS's own `Ics.Posix.*` message pump. A **Lazarus/FPC** port is **not viable** — ICS's POSIX layer rides the *Delphi* POSIX RTL, and ICS compiles out OpenSSL under FPC.
- Replaces Indy with [OverbyteICS](https://wiki.overbyte.eu/wiki/index.php/ICS_Download)'s `THttpServer` / `TSslHttpServer`. Distinctive value: **modern OpenSSL 3.x / 4.x** (TLS 1.3, SNI, mTLS); the OpenSSL libraries ship with ICS (DLLs on Windows, `.so` on Linux). On Linux, use `HORSE_APPTYPE_DAEMON` + `THorseICSLinuxDaemonApp.Run` for a signal-handled daemon shape.
- **Installation is manual** (not Boss): install OverbyteICS following the **official ICS instructions** — download/clone ICS (v9.x) and add its `Source/` folder to your search path; then add `horse-provider-ics/src`.
- v1 limitations: uploads must send `Content-Length` (chunked request bodies are rejected by ICS); keep-alive is disabled. See the provider README's *Known limitations*.
- See [`horse-provider-ics`](https://github.com/freitasjca/horse-provider-ics#readme) for setup, the A–K test suite, and the ICS-specific notes.

### HttpSys — optional, Windows-only, built-in

- **Built into Horse** — the `Horse.Provider.HttpSys` unit ships with the framework; **no external library, no Boss dependency**. Just add `HORSE_PROVIDER_HTTPSYS` to your project's Conditional Defines.
- **Windows only**, on **both Delphi and FPC/Lazarus** — it binds `httpapi.dll`, the Windows **http.sys** kernel-mode HTTP stack (the same stack IIS rides). Selecting it on a non-Windows target is a compile error.
- No DLLs to ship — http.sys is part of the OS. Two **machine-level** prerequisites apply because http.sys is global: a **URL reservation** (`netsh http add urlacl …`) for non-`localhost` hosts or privileged ports, and an **`netsh http add sslcert …`** binding for HTTPS (certificate lives in the Windows store, not a `.pem`).
- Same underlying stack as mORMot2's `THttpApiServer` (`mskHttpApi`) — HttpSys is the standalone, dependency-free way to use it. **Mutually exclusive** with CrossSocket / mORMot2 / ICS.

## Host-managed Application types

Apache / ISAPI / CGI / FastCGI **do not use a self-hosted Provider** — none of Indy, `fphttpserver`, CrossSocket, mORMot2, ICS, or HttpSys is involved. The host process (Apache httpd, IIS, the web server) owns the socket and hands the request to Horse via `Web.HTTPApp` (Delphi) or `fpFCGI` / `fpHTTP` (FPC) subclasses.

- **Apache module** — Delphi (`Web.HTTPD24Impl`, `Web.ApacheApp`). Build the `.so` / `.dll` matching the Apache architecture.
- **ISAPI extension** — Delphi only (`Web.Win.ISAPIApp`). Windows + IIS. Match the architecture of the IIS application pool (32-bit pool → Win32 build).
- **CGI** — Delphi (`Web.CGIApp`) and FPC. Cross-platform; one process per request.
- **FastCGI** — FPC (`fpFCGI`). Persistent process; talks to the web server via Unix socket or TCP. Delphi FCGI requires a third-party library and is not part of the shipped Horse providers.

## Compiler-version guards in the source

Horse uses a few defensive guards. If you're contributing patches:

- `{$IF DEFINED(FPC)}` — branch FPC-only code (different RTL, different generics syntax).
- `{$IF CompilerVersion >= 32.0}` — Delphi 10.2 Tokyo introduced `Int64` return types on `TWebRequest.GetIntegerVariable` / `TWebResponse.SetIntegerVariable`. Anything that overrides those needs the guard.
- `{$IF CompilerVersion >= 33.0}` — Delphi 10.3 Rio introduced inline `var`. Horse core avoids inline `var` so it remains compilable on XE7.

For every change, **test‑compile** against both **Delphi** (e.g., `dcc32`, `dcc64`) and **FPC** (`fpc`). Anonymous procedures, generics, and Web/HTTPApp types are the most common cross-compiler trip points.

## Reporting a version-specific bug

When opening an issue:

1. Include the **exact Delphi / FPC version** and the **target platform** (Win64, Linux64, …).
2. Include the **provider define(s)** active in your project.
3. State whether the bug is reproducible on the matching provider sample under `samples/`.

The triage is usually quick once the platform and provider are pinned.

## See also

- [Getting Started](./getting-started.md) — install paths per IDE.
- [Providers & Application types](./providers.md) — the two-axis model: which Provider × Application-type combinations exist, and which units each one `uses`.
