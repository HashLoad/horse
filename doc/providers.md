# Providers & Application types

*Read this in [English](./providers.md) or [Português (BR)](./providers.pt-BR.md).*

Horse separates two architectural choices that are easy to confuse:

1. **Provider** — the HTTP transport that owns the socket and parses requests. Indy is the default; CrossSocket and mORMot2 are optional async alternatives; future providers (nghttp2, …) follow the same pattern.
2. **Application type** — how the Delphi/FPC binary is packaged and started: a console executable, a Windows service, a VCL desktop app, an Apache module, an IIS extension, and so on.

These two axes are *conceptually* orthogonal. In the current Horse build the two choices are encoded in the same set of mutually-exclusive Conditional Defines, but the documentation below keeps them separate so the mental model is clean.

Application code is portable across all combinations — switching is normally a one-define change.

---

## 1. What is a Provider?

A Provider is the layer between Horse and the network. It:

1. Owns the listening socket (or, for host-managed application types, is invoked by the host).
2. Parses an incoming HTTP request into a `TWebRequest` (Indy) or an internal shadow-field representation (CrossSocket, future providers).
3. Calls `THorse.Execute(Req, Res)`, which runs the registered middleware chain and routes.
4. Serialises the response back to the client.

The same Horse routes, middleware, and `THorseRequest` / `THorseResponse` API run under every Provider. You don't rewrite handlers when switching transports.

## 2. Provider catalogue

The **default Provider depends on the compiler**:

- On **Delphi** (for the Console / VCL / Daemon Application types) the default is **Indy** (`TIdHTTPServer` via `IdHTTPWebBrokerBridge`).
- On **FPC** (for the Daemon / HTTPApplication / LCL Application types) the default is FreePascal's **`fphttpserver`** library.
- The optional **CrossSocket** Provider replaces *both* defaults with a cross-compiler async transport.

| Provider | Compiler define | Status | Delphi | Lazarus |
|---|---|---|:---:|:---:|
| **Indy** | _(none on Delphi)_ | Default for Delphi self-hosted | ✔ | n/a |
| **`fphttpserver`** | _(none on FPC)_ | Default for FPC self-hosted | n/a | ✔ |
| **horse-provider-crosssocket** | `HORSE_CROSSSOCKET` | Optional, external package | ✔ | ✔ |
| **horse-provider-mormot** | `HORSE_PROVIDER_MORMOT` | Optional, external package | ✔ | ✔ |
| **[HTTP.sys](./httpsys.md)** | `HORSE_PROVIDER_HTTPSYS` | Optional, built-in (Windows kernel mode) | ✔ | ✔ |
| **[epoll](./epoll.md)** | `HORSE_PROVIDER_EPOLL` | Optional, built-in (Linux async event loop) | ✔ | ✔ |

> **What library does the HTTP work, per Application type?** This is the deciding question — and it's *not* always Indy. The unifying abstraction across every row is `Web.HTTPApp.TWebRequest` on Delphi or `fpHTTP.TRequest` on FPC; below that, the concrete library differs.
>
> | Application type | Compiler | Transport library | Indy? |
> |---|---|---|:---:|
> | Console / VCL / Daemon | Delphi | **Indy** (`TIdHTTPServer` + `IdHTTPWebBrokerBridge`) | ✔ |
> | Daemon / HTTPApplication / LCL | FPC | **`fphttpserver`** | ✘ |
> | Any self-hosted + `HORSE_CROSSSOCKET` | Either | **`Delphi-Cross-Socket`** | ✘ |
> | Any self-hosted + `HORSE_PROVIDER_MORMOT` | Either | **`mORMot2`** (`THttpServer` / `THttpApiServer`) | ✘ |
> | Any self-hosted + `HORSE_PROVIDER_HTTPSYS` | Either | **`HTTP.sys`** (Windows Kernel Driver) | ✘ |
> | Any self-hosted + `HORSE_PROVIDER_EPOLL` | Either | **`epoll`** (Linux kernel epoll API) | ✘ |
> | Apache module | Either | **Apache httpd** (via `Web.HTTPApp.TApacheRequest` / `mod_horse`) | ✘ |
> | ISAPI | Delphi | **IIS** (via `Web.HTTPApp.TISAPIRequest`) | ✘ |
> | CGI | Delphi | **Web server's CGI runner** (via `Web.HTTPApp.TCGIRequest`) | ✘ |
> | FastCGI | FPC | **`fpFCGI`** library (talks to web server) | ✘ |

### Indy (Delphi default for self-hosted)

The default transport on Delphi when no `HORSE_*` define is set and the Application type is Console, VCL, or Daemon. Indy ships inside the Horse repository — no extra `boss install` needed. It uses a **thread-per-connection** model: one OS thread per accepted client, managed by Indy's thread pool. The unit `IdHTTPWebBrokerBridge` bridges Indy's `TIdHTTPServer` to the `Web.HTTPApp.TWebRequest` abstraction that Horse middleware sees.

**Properties:**
- Cross-platform (Windows, Linux, macOS) — Indy itself does the heavy lifting.
- Trivial deployment: copy the executable, run it.
- SSL via OpenSSL DLLs placed next to the binary.
- Scaling ceiling: typically a few hundred to ~1 000 concurrent connections before thread-pool exhaustion or scheduler pressure starts showing.

**Pick this when:** you have at most a few hundred concurrent clients, deployment is "just run the binary", and you don't need long-polling / SSE / WebSockets at scale.

### `fphttpserver` (FPC default for self-hosted)

On FPC builds (Lazarus), the default self-hosted Provider is FreePascal's own **`fphttpserver`** library — not Indy. The FPC provider units (`Horse.Provider.FPC.Daemon`, `Horse.Provider.FPC.HTTPApplication`, `Horse.Provider.FPC.LCL`) `uses fphttpserver, fpHTTP, httpdefs` and present incoming requests as `TRequest` / `TResponse` from `HTTPDefs`.

**Properties:**
- Same thread-per-connection model as Indy, but using FreePascal's RTL.
- Cross-platform on the FPC targets (Linux, Windows, macOS, BSD).
- No Indy dependency — keeps FPC builds free of the OpenSSL DLL deployment story.
- SSL via OpenSSL through `Synapse` / `fpHTTPClient` SSL handlers (provider-dependent).

**Pick this when:** you're on FPC and you want the lightest possible FPC-native build with no extra packages.

### CrossSocket (optional)

[`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket) replaces the Indy transport with [Delphi-Cross-Socket](https://github.com/winddriver/Delphi-Cross-Socket): an async I/O library that uses **IOCP on Windows, epoll on Linux, and kqueue on macOS** — the same primitives nginx and Node.js use.

```sh
boss install horse-provider-crosssocket
```

In your project's Conditional Defines: `HORSE_CROSSSOCKET`. Your code stays the same.

**What changes vs. Indy:**

| | Indy | CrossSocket |
|---|---|---|
| Concurrency | One thread per connection | Fixed IO-thread pool (`CPUCount*2+1`, e.g. 9 on a 4-core, 17 on an 8-core) |
| Idle keep-alive cost | One thread per idle connection | One epoll/IOCP handle — negligible |
| Per-request allocation | New `THorseRequest`/`THorseResponse` | Pre-warmed object pool (32 contexts, scales to 512) |
| Scaling ceiling | ~1 000 concurrent on commodity hardware | 10 000+ concurrent on the same hardware |
| Linux deployment | Indy works but isn't its primary platform | First-class — epoll is the native Linux async primitive |
| Pre-pipeline request validation | None | URL length, header limits, request-smuggling guards, method allowlist |
| Object pool, zero-allocation hot path | No | Yes |

**Pick CrossSocket when:**
- You expect more than a few hundred concurrent clients.
- You deploy on Linux (especially in containers / Docker).
- You serve long-polling, server-sent events, or many idle keep-alive connections.
- You want enforced size / smuggling protections at the transport layer.

CrossSocket and Indy are **drop-in alternatives** for the same Horse codebase. The same middleware (`Horse.CORS`, `Jhonson`, `JWT`, `logger`, etc.) works on both.

For configuration (TLS certificates, body-size limits, IO thread count, mTLS), see the [provider's own documentation](https://github.com/freitasjca/horse-provider-crosssocket#readme).

### Installation 

`horse-provider-crosssocket` pulls Delphi-Cross-Socket through Boss. 

If install manually:

1. Clone [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket).
2. Clone Delphi-Cross-Socket — **two options**:
   - **Recommended:** upstream [`winddriver/Delphi-Cross-Socket`](https://github.com/winddriver/Delphi-Cross-Socket). Tracks the maintainer's release cadence; receives upstream improvements as soon as they land.
   - **Supported alternative:** the fork release [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3), which bundles CnPack as a vendored subtree and adds the mTLS server-mode APIs (`SetCACertificate(File)` + `SetVerifyPeer(Boolean)`). One clone instead of two, at the cost of lagging behind upstream commits between fork syncs. Pick this if you need mTLS server mode or prefer the single-dependency convenience.
3. For the upstream path **only**, also clone [`cnpack/cnvcl`](https://github.com/cnpack/cnvcl) and add `Source/Common` + `Source/Crypto` to the search path. The fork bundles these files already.
4. Add all the resulting paths to your project's Search-path field — see [`horse-provider-crosssocket` README](https://github.com/freitasjca/horse-provider-crosssocket#installation) for the full three-path runbook (upstream, fork, and Boss-for-Horse-only).

> **Why "supported alternative" and not "deprecated"?** The fork remains actively maintained for users who want bundled CnPack convenience or need mTLS today. The trade-off is straightforward: upstream `winddriver/Delphi-Cross-Socket` has a higher commit cadence than any fork can keep up with, so the fork inevitably lags. Three previously-fork-only bug fixes (`PATCH-IOCP-1` shutdown cascade, the zero-body response parser hang, the `_OnBodyEnd` nil-guard) have already been merged upstream as of 2026-Q2 — only the mTLS additions remain genuinely fork-exclusive. An upstream mTLS PR is in preparation; once merged, the fork's only remaining value will be the bundled CnPack convenience.

### mORMot2 (optional)

[`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot) replaces the Indy transport with [mORMot2](https://github.com/synopse/mORMot2): a mature, high-performance library that uses **IOCP on Windows and epoll on Linux** — the same kernel primitives as CrossSocket. mORMot2 manages its own fixed thread pool (default 32 threads) so no `THorseWorkerPool` is needed.

```sh
boss install horse-provider-mormot
```

In your project's Conditional Defines: `HORSE_PROVIDER_MORMOT`. Your code stays the same.

**Three server backends.** The provider can host any of mORMot2's HTTP servers, selected via `THorseMormotConfig.ServerKind`: `mskThreadPool` (`THttpServer`, the default — one thread per concurrent request), `mskAsync` (`THttpAsyncServer`, a non-blocking IOCP/epoll/kqueue event loop that scales past thread-per-request), or `mskHttpApi` (`THttpApiServer`, Windows **http.sys** kernel-mode HTTP — Windows only). Switch at runtime (`Cfg.ServerKind := mskAsync` before `THorse.ListenWithConfig`) or set a project-wide define (`HORSE_MORMOT_ASYNC`, or `HORSE_MORMOT_HTTPAPI` on Windows) to flip the default. With none, the default stays `THttpServer` — unchanged behaviour. `mskHttpApi` registers `http://+:<port>/` with http.sys, which needs Administrator rights or a one-time `netsh http add urlacl`. See the [provider's own documentation](https://github.com/freitasjca/horse-provider-mormot#readme) for details.

**What changes vs. Indy:**

| | Indy | mORMot2 |
|---|---|---|
| Concurrency | One thread per connection | Fixed thread pool (default 32, configurable) |
| Idle keep-alive cost | One thread per idle connection | Thread only runs when data is ready |
| Per-request allocation | New `THorseRequest`/`THorseResponse` | Pre-warmed object pool (32 contexts, scales to 512) |
| Scaling ceiling | ~1 000 concurrent on commodity hardware | 10 000+ concurrent on the same hardware |
| Compiler support | Delphi XE7+ | Delphi 7 through 12.3 Athens, FPC 3.2+ |
| http.sys (Windows) | ❌ | ✔ `THttpApiServer` — kernel-mode HTTP, zero code change |
| External dependency | Indy (bundled) | mORMot2 (add to search path) |

**What changes vs. CrossSocket:**

| | CrossSocket | mORMot2 |
|---|---|---|
| Thread pool | `THorseWorkerPool` (4–64 Horse threads) | Built-in (default 32 threads) |
| External dependency | Delphi-Cross-Socket + CnPack | mORMot2 + (Delphi only) precompiled static `.obj` blobs for zlib/OpenSSL/SQLite |
| Boss-installable | ✔ both deps | ❌ — mORMot2 must be cloned manually and added to the search path |
| Older Delphi support | Delphi 10.2+ | Delphi 7+ |
| http.sys (Windows kernel-mode HTTP) | ❌ | ✔ swap `THttpServer` for `THttpApiServer` |
| mTLS server mode | ✔ via [`freitasjca/Delphi-Cross-Socket >= 1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3) | Roadmap — not yet exposed via `THorseMormotConfig` |

**Pick mORMot2 when:**
- You need Delphi 7 / XE / XE2 support.
- You want no third-party library dependency for standard HTTP (pure Pascal).
- You want Windows http.sys kernel-mode HTTP (`THttpApiServer`).
- You prefer a 15+-year production-proven codebase.

**Pick CrossSocket when:**
- You prefer native async IOCP/epoll control.
- Your project already depends on Delphi-Cross-Socket.

Both providers use IOCP/epoll and a context object pool, so throughput is comparable under typical workloads.

For configuration (`ServerKind`, `ThreadPool`, `MaxBodyBytes`, `DrainTimeoutMs`, `ServerBanner`) and application-type wrappers (VCL, Windows Service, Linux daemon), see the [provider's own documentation](https://github.com/freitasjca/horse-provider-mormot#readme).

> **mORMot2 installation** — mORMot2 is not available via `boss install`. Clone [synopse/mORMot2](https://github.com/synopse/mORMot2) and add `<mORMot2>/src`, `<mORMot2>/src/core`, `<mORMot2>/src/net` to the compiler search path.

### Future providers

The hybrid-interface architecture (`IHorseRawRequest` / `IHorseRawResponse`) introduced by CrossSocket makes it straightforward to add additional transports. For nghttp2 see [`building-a-new-provider.md`](https://github.com/freitasjca/horse-provider-crosssocket/blob/master/doc/building-a-new-provider.md).

---

## 3. Application types — self-hosted

Self-hosted application types run the Provider you chose. The Provider owns the socket; your binary owns the process.

| Application type | Compiler define | Delphi | Lazarus |
|---|---|:---:|:---:|
| **Console** _(default)_ | _(none)_ | ✔ | ✔ |
| **VCL** | `HORSE_VCL` | ✔ | ❌ |
| **Daemon — Windows Service** | `HORSE_DAEMON` | ✔ | n/a |
| **Daemon — Linux daemon (systemd)** | `HORSE_DAEMON` | ✔ | ✔ |
| **LCL** (Lazarus GUI) | `HORSE_LCL` | ❌ | ✔ |
| **HTTPApplication** (FPC) | _(FPC default)_ | ❌ | ✔ |

> **Note** — `HORSE_DAEMON` is a unified application type: the produced binary is a **Windows Service** (`Vcl.SvcMgr.TService` + SCM) on Windows and a **daemon** (`signal(SIGTERM)` + systemd) on Linux. "Daemon" is the Unix-native term; Windows has no native equivalent word, so the define name is borrowed cross-platform.

### Console — the default

The simplest case. Write a `.dpr` with `{$APPTYPE CONSOLE}`, call `THorse.Listen(port)`, run the binary. Works on Windows, Linux, and macOS. Most projects fit this shape.

### VCL — server inside a desktop app

Useful when your Delphi application has a UI and you want it to expose an HTTP control surface (a remote-control endpoint, an embedded admin panel). With `HORSE_VCL` defined, Horse starts the Provider on a background thread; the VCL main thread stays free for the form / message loop. Indy-only today; CrossSocket VCL support is architecturally possible but not currently expressible via the defines.

### Daemon — Windows Service or Linux daemon (systemd)

`HORSE_DAEMON` is a unified application type. The same `.dpr` compiles for both target OSes; the unit's `{$IFDEF MSWINDOWS}` branch selects the host-integration path at compile time.

- **Windows:** Horse wires `Listen` / `Stop` into the Service Control Manager via `Vcl.SvcMgr.TService`. Combined with a service wrapper, you get `sc start MyService` / `sc stop MyService` integration.
- **Linux:** Horse installs POSIX signal handlers for `SIGTERM` / `SIGINT` (the standard systemd shutdown signals) and ignores `SIGPIPE`. Combined with a `.service` unit file, `systemctl start/stop MyService` works identically.

Same transport (Indy on Delphi, `fphttpserver` on FPC, or CrossSocket / mORMot2 when their Provider is also defined) underneath in either case. See the [Deployment Cheatsheet](./deployment.md) for both lifecycle templates side by side.

### LCL — Lazarus GUI application

The Lazarus counterpart to VCL. With `HORSE_LCL` defined, the Provider runs alongside the Lazarus message loop on FPC. Used when a Lazarus desktop app needs to expose HTTP.

### HTTPApplication — FPC native HTTP application

A standalone FPC-only application type that uses FreePascal's `fpHTTP` server scaffolding. Used when you want a pure-FPC build that integrates with Lazarus tooling without involving Indy.

---

## 4. Host-managed application types

When the application type is **host-managed**, the host process (Apache httpd, IIS, the web server) owns the socket and hands a pre-parsed request to your Delphi/FPC code. The Providers from §2 are **not used at all** — **no Indy, no `fphttpserver`, no CrossSocket**. Instead the request abstraction is provided by:

- **Delphi**: built-in `Web.HTTPApp` subclasses — `TApacheRequest` (Apache module), `TISAPIRequest` (IIS), `TCGIRequest` (CGI). Horse `uses Web.WebBroker, Web.ApacheApp / Web.Win.ISAPIApp / Web.CGIApp` accordingly.
- **FPC**: FreePascal's `fpFCGI` library for FastCGI (the FCGI process listens on a socket for the web server, then dispatches via `fpHTTP`).

The host *is* the transport. Horse middleware still sees the same `TWebRequest` / `TRequest` it always sees — that's the unifying abstraction.

| Application type | Compiler define | Build artefact | Hosted by | Delphi | Lazarus |
|---|---|---|---|:---:|:---:|
| **Apache module** | `HORSE_APACHE` | `.so` / `.dll` Apache module | Apache httpd via `mod_horse` | ✔ | ✔ |
| **ISAPI** | `HORSE_ISAPI` | ISAPI extension `.dll` | IIS | ✔ | ❌ |
| **CGI** | `HORSE_CGI` | Standalone executable | Web server (one process per request) | ✔ | ✔ |
| **FastCGI** | `HORSE_FCGI` | Standalone executable | Web server (persistent FCGI pool) | ✔ | ✔ |

**Pick one of these when:** you already have an Apache / IIS / nginx deployment and want to integrate Horse without standing up a separate process. The host handles SSL, gzip, virtual hosting, and you focus on the application.

**Trade-offs:**
- Lifecycle controlled by the host — no graceful shutdown hooks, no event loop you own.
- Build / deploy more involved than a single executable.
- The Provider catalogue above doesn't apply — features like CrossSocket's async I/O, request smuggling guards, or object pool are architecturally absent because there is no socket to own.

---

## 5. Compatibility matrix

Provider × Application type — which combinations are currently expressible (and architecturally compatible)? The Delphi-only Application types are in the top half; FPC-only ones in the bottom; host-managed across the right.

|  | Console (D) | VCL (D) | Daemon (D) | Daemon (FPC) | LCL (FPC) | HTTPApplication (FPC) | Apache | ISAPI | CGI | FCGI |
|---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| **Indy** _(Delphi default)_ | ✔ | ✔ | ✔ | n/a | n/a | n/a | n/a | n/a | n/a | n/a |
| **`fphttpserver`** _(FPC default)_ | n/a | n/a | n/a | ✔ | ✔ | ✔ | n/a | n/a | n/a | n/a |
| **CrossSocket** (`HORSE_PROVIDER_CROSSSOCKET`) | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ❌ | ❌ | ❌ | ❌ |
| **mORMot2** (`HORSE_PROVIDER_MORMOT`) | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ❌ | ❌ | ❌ | ❌ |
| _Host-managed_ (Apache/ISAPI/CGI/FCGI) | n/a | n/a | n/a | n/a | n/a | n/a | ✔ | ✔ | ✔ | ✔ |

Legend:
- **✔** — supported and expressible with the current defines. Since PATCH-HORSE-2, every CrossSocket × Application-type cell is supported via the cross-product convenience units in `horse-provider-crosssocket` (e.g. `Horse.Provider.CrossSocket.VCL`, `…Daemon`, `…FPC.Daemon`, `…FPC.LCL`, `…FPC.HTTPApplication`). mORMot2 ships the matching cross-product set in `horse-provider-mormot`: `Horse.Provider.Mormot` (Console default), `…Mormot.VCL`, `…Mormot.Daemon` (Windows TService + POSIX runner in one unit), `…Mormot.FPC.Daemon`, `…Mormot.FPC.LCL`, `…Mormot.FPC.HTTPApplication`.
- **❌** — architecturally impossible. Apache / ISAPI / CGI / FCGI own the socket themselves; an async self-hosted transport like CrossSocket or mORMot2 can't coexist.
- **n/a** — meaningless combination. Indy doesn't run on FPC; `fphttpserver` doesn't run on Delphi; host-managed application types don't use a self-hosted Provider; self-hosted types don't run under a host's lifecycle.

PATCH-HORSE-1 in `Horse.pas` enforces the ❌ cells at compile time with `{$MESSAGE FATAL}` so misconfigured projects fail fast instead of silently picking the wrong code path.

---

## 6. Selecting your defines

The selection happens at **compile time** via Project Options → Conditional Defines. PATCH-HORSE-2 splits the defines into three explicit namespaces — one per axis — and the chain composes them.

### The three namespaces

| Axis | Prefix | Meaning |
|---|---|---|
| A · **Provider** | `HORSE_PROVIDER_*` | HTTP transport library — Indy (Delphi default), `fphttpserver` (FPC default), CrossSocket, mORMot2 |
| B · **Application type** | `HORSE_APPTYPE_*` | Binary lifecycle shape — Console (default), VCL, Daemon, LCL, HTTPApplication |
| C · **Host-managed runtime** | `HORSE_HOST_*` | Web server owns the socket — Apache, ISAPI, CGI, FastCGI |

Axis C wins outright when set (no Provider involved). Axes A and B compose freely.

### Common combinations

| Goal | Define(s) to set |
|---|---|
| Default — Console + Indy (Delphi) | _(none)_ |
| Default — HTTPApplication + `fphttpserver` (FPC) | _(none)_ |
| VCL desktop app + Indy | `HORSE_APPTYPE_VCL` |
| Windows service + Indy | `HORSE_APPTYPE_DAEMON` |
| Lazarus GUI + `fphttpserver` | `HORSE_APPTYPE_LCL` |
| Async high-performance Console + CrossSocket | `HORSE_PROVIDER_CROSSSOCKET` |
| **CrossSocket + VCL** *(new in PATCH-HORSE-2)* | `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_VCL` |
| **CrossSocket + Windows service** *(new in PATCH-HORSE-2)* | `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_DAEMON` |
| **CrossSocket + Linux daemon (FPC)** *(new in PATCH-HORSE-2)* | `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_DAEMON` (on FPC) |
| **CrossSocket + Lazarus LCL** *(new in PATCH-HORSE-2)* | `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_LCL` |
| **mORMot2 Console** | `HORSE_PROVIDER_MORMOT` |
| **mORMot2 + VCL** | `HORSE_PROVIDER_MORMOT` + `HORSE_APPTYPE_VCL` |
| **mORMot2 + Windows service** | `HORSE_PROVIDER_MORMOT` + `HORSE_APPTYPE_DAEMON` (on Windows) |
| **mORMot2 + Linux daemon** | `HORSE_PROVIDER_MORMOT` + `HORSE_APPTYPE_DAEMON` (on Linux) |
| Apache module | `HORSE_HOST_APACHE` |
| IIS ISAPI extension | `HORSE_HOST_ISAPI` |
| Plain CGI | `HORSE_HOST_CGI` |
| FastCGI | `HORSE_HOST_FCGI` |

### Legacy aliases (Horse <3.2 compatibility)

Every old define is automatically translated to the new namespace by an alias block at the top of `Horse.pas` — every existing `.dproj` / `.lpi` continues to compile unchanged.

| Legacy define | New namespaced define |
|---|---|
| `HORSE_CROSSSOCKET` | `HORSE_PROVIDER_CROSSSOCKET` |
| `HORSE_VCL` | `HORSE_APPTYPE_VCL` |
| `HORSE_DAEMON` | `HORSE_APPTYPE_DAEMON` |
| `HORSE_LCL` | `HORSE_APPTYPE_LCL` |
| `HORSE_APACHE` | `HORSE_HOST_APACHE` |
| `HORSE_ISAPI` | `HORSE_HOST_ISAPI` |
| `HORSE_CGI` | `HORSE_HOST_CGI` |
| `HORSE_FCGI` | `HORSE_HOST_FCGI` |
| `HORSE_NOPROVIDER` | unchanged (escape hatch) |

New projects should prefer the namespaced names — they're self-documenting and they make the three-axis model visible in the project's conditional defines.

`Horse.pas` resolves the active selection in a two-stage `{$IFDEF}` chain — host-managed first (Axis C wins outright), then Provider × Application-type composition. **There is no runtime switching.** If you want to deploy the same code as both a console binary and an Apache module, build twice with different defines.

## 7. What does NOT change when you switch

- Your route declarations.
- Your handler bodies.
- Your middleware.
- The `THorseRequest` / `THorseResponse` API your code uses.
- The supported Delphi / FPC versions (within each Provider's range — see [Compiler Support](./compiler-support.md)).

The Provider and Application type are intentionally swappable. If you keep your code transport-neutral (and Horse encourages this), you can A/B test Indy vs CrossSocket simply by toggling the define and rebuilding.

---

## 8. Running CrossSocket as each Application type

`HORSE_CROSSSOCKET` cannot combine with `HORSE_VCL` / `HORSE_DAEMON` / `HORSE_LCL` / `HORSE_ISAPI` / `HORSE_APACHE` / `HORSE_CGI` / `HORSE_FCGI` at compile time (PATCH-HORSE-1 enforces this). To achieve the runtime shape of each of those Application types **while using CrossSocket as the transport**, define only `HORSE_CROSSSOCKET` and add the small shape-specific shell yourself.

The common pattern across every shape:

1. **Define only `HORSE_CROSSSOCKET`** — no other `HORSE_*` provider define.
2. Pick the right **project type** for the desired shape (Console / VCL Forms / Lazarus / Service Application / …).
3. Drive `THorse.Listen` from the right **lifecycle event** for that shape.
4. Call `THorse.StopListen` on shutdown so CrossSocket drains active requests (SEC-30) before the process exits.

CrossSocket's `Listen` automatically picks between blocking and non-blocking behaviour based on `IsConsole`:

- `IsConsole = True` (console binary) → `Listen` blocks the calling thread until `StopListen` is called.
- `IsConsole = False` (VCL / LCL / TService / …) → `Listen` starts the IO threads and returns immediately; the calling thread is free to drive a GUI message loop or service control loop. Call `StopListen` from a teardown handler.

### 8.1 Console (Delphi)

The default and simplest case. `THorse.Listen` blocks the main thread; `StopListen` from a Ctrl-C handler unblocks it.

```pascal
program MyServer;

{$APPTYPE CONSOLE}                 // → IsConsole = True

uses
  Winapi.Windows, Horse;

function CtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  case dwCtrlType of
    CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT,
    CTRL_SHUTDOWN_EVENT:
      begin
        THorse.StopListen;          // drain → Listen returns
        Result := True;
      end;
  else
    Result := False;
  end;
end;

begin
  SetConsoleCtrlHandler(@CtrlHandler, True);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin Res.Send('pong'); end);

  THorse.Listen(9000);             // blocks
end.
```

Define: `HORSE_PROVIDER_CROSSSOCKET` (or the legacy alias `HORSE_CROSSSOCKET`). Project type: **Console Application**.

### 8.2 VCL desktop app (Delphi)

A VCL Forms project with an embedded HTTP server. The VCL main thread runs the message loop; CrossSocket runs on its own IO threads.

```pascal
unit Main.Form;

interface

uses
  Vcl.Forms, System.Classes, Horse;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin Res.Send('pong'); end);

  // IsConsole = False in a VCL app → Listen starts the IO threads
  // and returns immediately. The VCL message loop keeps the form alive.
  THorse.Listen(9000);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  THorse.StopListen;               // graceful drain before the form closes
end;

end.
```

Defines: `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_VCL` (PATCH-HORSE-2). Project type: **VCL Forms Application**. Do **not** add `{$APPTYPE CONSOLE}` to the .dpr — that would force `IsConsole = True` and Listen would block the main thread, freezing the UI.

**Tip:** the optional convenience base class `TfrmHorseVCLHost` (in `Horse.Provider.CrossSocket.VCL`) pre-wires `FormCreate` → `THorse.Listen(Port)` and `FormClose` → `THorse.StopListen`. Inherit from it instead of writing the wiring above by hand.

### 8.3 Linux daemon (Delphi cross-compiled to Linux)

A standalone Linux binary supervised by systemd. The daemon shape is provided by systemd, not by Horse.

```pascal
program MyDaemon;

{$APPTYPE CONSOLE}                 // → IsConsole = True; Listen blocks

uses
  {$IFDEF LINUX} Posix.Signal, {$ENDIF}
  Horse;

{$IFDEF LINUX}
procedure HandleSignal(ASignal: Integer); cdecl;
begin
  THorse.StopListen;               // SIGTERM from systemd → drain → exit
end;
{$ENDIF}

begin
  {$IFDEF LINUX}
  signal(SIGTERM, @HandleSignal);
  signal(SIGINT,  @HandleSignal);
  {$ENDIF}

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin Res.Send('pong'); end);

  THorse.Listen(9000);
end.
```

systemd unit at `/etc/systemd/system/myhorse.service`:

```ini
[Unit]
Description=My Horse Server (CrossSocket)
After=network.target

[Service]
Type=simple
User=horse
WorkingDirectory=/opt/myhorse
ExecStart=/opt/myhorse/MyDaemon
ExecStop=/bin/kill -TERM $MAINPID
Restart=on-failure
RestartSec=5s
LimitNOFILE=65536

[Install]
WantedBy=multi-user.target
```

```sh
sudo systemctl daemon-reload && sudo systemctl enable --now myhorse
```

Defines: `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_DAEMON` (PATCH-HORSE-2). Project target: **Linux64**.

**Tip:** the optional convenience runner `THorseCrossSocketLinuxDaemonApp.Run(@SetupRoutes, Port)` (in `Horse.Provider.CrossSocket.Daemon` — the same unit as §8.4's `THorseCrossSocketService`, just the non-Windows branch) installs `signal(SIGTERM/SIGINT)` handlers, ignores `SIGPIPE`, and calls `THorse.Listen` for you. The whole `program` body collapses to a single call.

> **Note on `HORSE_APPTYPE_DAEMON` on Delphi:** the same define means "Windows Service" when building for Windows and "Linux daemon" when building for Linux. `Horse.Provider.CrossSocket.Daemon.pas` carries both lifecycle helpers (`THorseCrossSocketService` under `{$IFDEF MSWINDOWS}`, `THorseCrossSocketLinuxDaemonApp` under `{$ELSE}`). This mirrors the cross-platform behaviour of the Indy-based `Horse.Provider.Daemon.pas`. "Daemon" is the *intent* (OS-supervised long-running process); the OS-specific machinery is picked by the build target.

### 8.4 Windows Service (Delphi)

A Delphi Service Application that drives CrossSocket. The Windows SCM start/stop hooks call `THorse.Listen` / `StopListen` on a worker thread so the SCM message pump stays responsive.

```pascal
unit MyHorseSvc;

interface

uses
  System.SysUtils, System.Classes, Vcl.SvcMgr, Horse;

type
  TMyHorseService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FListenerThread: TThread;
  end;

var
  MyHorseService: TMyHorseService;

implementation

{$R *.dfm}

procedure TMyHorseService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin Res.Send('pong'); end);

  // IsConsole = False in a service app → Listen returns immediately
  // and CrossSocket runs on its own IO threads. But we also have to
  // keep the *service* call non-blocking so the SCM gets its ack — so
  // we run Listen on a dedicated thread that we own.
  FListenerThread := TThread.CreateAnonymousThread(
    procedure begin THorse.Listen(9000); end);
  FListenerThread.FreeOnTerminate := False;
  FListenerThread.Start;

  Started := True;
end;

procedure TMyHorseService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  THorse.StopListen;                // drain
  if Assigned(FListenerThread) then
  begin
    FListenerThread.WaitFor;
    FreeAndNil(FListenerThread);
  end;
  Stopped := True;
end;

end.
```

Defines: `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_DAEMON` (PATCH-HORSE-2). Project type: **Service Application** (File → New → Other → Service Application).

**Tip:** the optional convenience base class `THorseCrossSocketService` (in `Horse.Provider.CrossSocket.Daemon`) pre-wires `ServiceStart` to spawn the worker thread for `Listen` and `ServiceStop` to drain + join cleanly. Inherit from it instead of writing the wiring above by hand.

Install / uninstall via the standard SCM verbs. **Run from an elevated (Administrator) Command Prompt** — `/install`, `/uninstall`, `sc start`, and `sc stop` all go through the Service Control Manager, which rejects non-elevated tokens with `EOSError ... Code: 5. Access is denied.` even when the user is in the Administrators group (UAC filters the token).

```bat
MyHorseServer.exe /install
sc start MyHorseService
sc stop  MyHorseService
MyHorseServer.exe /uninstall
```

Simpler alternative if you don't want to write the TService wrapper: build a Console binary (§8.1) and register it as a Windows service via [NSSM](https://nssm.cc/). NSSM sends `Ctrl+Break` on stop; the `CtrlHandler` from §8.1 catches it and calls `StopListen`.

### 8.5 Linux daemon (FPC / Lazarus)

Same shape as §8.3 but built with FPC. The FPC default Provider on a no-define build is `fphttpserver`; defining `HORSE_CROSSSOCKET` swaps it for CrossSocket and the binary's HTTP layer becomes async.

```pascal
program MyDaemon;

{$MODE DELPHI}{$H+}
{$APPTYPE CONSOLE}                 // → IsConsole = True; Listen blocks

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  SysUtils, Horse;

{$IFDEF UNIX}
procedure HandleSignal(ASignal: cint); cdecl;
begin
  THorse.StopListen;
end;
{$ENDIF}

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  {$IFDEF UNIX}
  fpSignal(SIGTERM, @HandleSignal);
  fpSignal(SIGINT,  @HandleSignal);
  {$ENDIF}

  THorse.Get('/ping', @GetPing);   // note the @ — FPC needs it
  THorse.Listen(9000);
end.
```

Defines: `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_DAEMON` (PATCH-HORSE-2). Build with `fpc` or `lazbuild`, target Linux.

**Tip:** the optional convenience runner `THorseCrossSocketDaemonApp.Run(@SetupRoutes, Port)` (in `Horse.Provider.CrossSocket.FPC.Daemon`) installs `fpSignal(SIGTERM)` / `SIGINT` handlers and calls `THorse.Listen` for you. The whole `program` body collapses to a single line: `THorseCrossSocketDaemonApp.Run(@SetupRoutes, 9000);`.

systemd unit is identical to §8.3.

### 8.6 Lazarus LCL desktop app (FPC)

The Lazarus counterpart to §8.2. Lazarus GUI app embedding a CrossSocket HTTP server.

```pascal
unit Main.Form;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Forms, Horse;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  end;

implementation

{$R *.lfm}

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  THorse.Get('/ping', @GetPing);
  THorse.Listen(9000);             // IsConsole = False → returns immediately
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  THorse.StopListen;
end;

end.
```

Defines: `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_LCL` (PATCH-HORSE-2). Project type: **Lazarus Application** (a normal GUI project). Do not use `{$APPTYPE CONSOLE}`.

**Tip:** the optional convenience base class `TfrmHorseLCLHost` (in `Horse.Provider.CrossSocket.FPC.LCL`) is the Lazarus mirror of `TfrmHorseVCLHost` — same `Port` / `OnHorseListen` / auto-wired `FormCreate`/`FormClose`.

### 8.7 FPC HTTPApplication

The FPC HTTPApplication shape is a standalone FPC executable that traditionally uses `fphttpapp.Application.Run` to own the main loop. With CrossSocket, the loop is owned by CrossSocket's IO threads instead — so the structure collapses to the same shape as §8.5 (a console-shape FPC binary that calls `THorse.Listen`).

```pascal
program MyHttpApp;

{$MODE DELPHI}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  SysUtils, Horse;

{$IFDEF UNIX}
procedure HandleSignal(ASignal: cint); cdecl;
begin
  THorse.StopListen;
end;
{$ENDIF}

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  {$IFDEF UNIX}
  fpSignal(SIGTERM, @HandleSignal);
  fpSignal(SIGINT,  @HandleSignal);
  {$ENDIF}

  THorse.Get('/ping', @GetPing);
  THorse.Listen(9000);             // CrossSocket owns the loop
end.
```

Define: `HORSE_PROVIDER_CROSSSOCKET` (no `HORSE_APPTYPE_*` — HTTPApplication is the FPC default for self-hosted; PATCH-HORSE-2 resolves the chain to the existing console-shape unit). The convenience class `THorseCrossSocketHTTPApp.Run(@SetupRoutes, Port)` (in `Horse.Provider.CrossSocket.FPC.HTTPApplication`) delegates to the same signal-handler runner as §8.5.

If you actually need to keep the `fphttpapp.TFPHTTPApplication` lifecycle (because some library you depend on expects it), instantiate it but never call `Application.Run` — let `THorse.Listen` own the main loop instead. Two competing event loops in the same process is the failure mode PATCH-HORSE-1 explicitly prevents at compile time.

---

### Quick reference

| Application type | `{$APPTYPE CONSOLE}` | `IsConsole` | `Listen` blocks? | Where to call `Listen` | Where to call `StopListen` |
|---|:---:|:---:|:---:|---|---|
| Console (Delphi) | ✔ | True  | Yes | `begin … end.`            | `SetConsoleCtrlHandler` |
| VCL (Delphi) | ❌ | False | No  | `FormCreate`              | `FormClose` |
| Linux daemon (Delphi) | ✔ | True  | Yes | `begin … end.`            | POSIX `SIGTERM` handler |
| Windows Service (Delphi) | ❌ | False | No  | `TService.ServiceStart` on a worker thread | `TService.ServiceStop` |
| Linux daemon (FPC) | ✔ | True  | Yes | `begin … end.`            | `fpSignal(SIGTERM, …)` |
| LCL (FPC / Lazarus) | ❌ | False | No  | `FormCreate`              | `FormClose` |
| FPC HTTPApplication | ✔ | True  | Yes | `begin … end.`            | `fpSignal(SIGTERM, …)` |

All seven cases share the same compile-time configuration: **define `HORSE_CROSSSOCKET` only**, no other `HORSE_*` provider define.

---

## 9. Running mORMot2 as each Application type

The same `IsConsole` / lifecycle pattern from §8 applies to mORMot2 — the provider units (`Horse.Provider.Mormot.*`) wrap mORMot's `THttpServer` instead of `TCrossHttpServer` but expose the same `Listen` / `StopListen` contract. The four-step recipe is identical:

1. **Define only `HORSE_PROVIDER_MORMOT`** — no other `HORSE_*` provider define.
2. Pick the right **project type** for the desired shape (Console / VCL Forms / Lazarus / Service Application / …).
3. Drive `THorse.Listen` from the right **lifecycle event** for that shape.
4. Call `THorse.StopListen` on shutdown so mORMot's active-request counter drains active requests before the process exits.

mORMot's `Listen` honours the same `IsConsole` switch as CrossSocket: `True` → blocks the calling thread until `StopListen`; `False` → returns immediately after `THttpServer.WaitStarted` so the GUI / service / LCL loop owns the main thread.

### 9.1 Side-by-side with §8 — the only differences

The whole code skeleton in each of §8.1–8.7 ports to mORMot2 by **two substitutions**:

| In §8 (CrossSocket) | In the mORMot2 equivalent |
|---|---|
| `HORSE_PROVIDER_CROSSSOCKET` define | `HORSE_PROVIDER_MORMOT` define |
| `Horse.Provider.CrossSocket.{VCL,Daemon,FPC.Daemon,FPC.LCL,FPC.HTTPApplication}` convenience helper | `Horse.Provider.Mormot.{VCL,Daemon,FPC.Daemon,FPC.LCL,FPC.HTTPApplication}` |

The convenience helpers map one-to-one — the table below is the complete inventory:

| Shape | CrossSocket helper | mORMot2 helper |
|---|---|---|
| Console (§8.1) | _(none — direct provider use)_ | _(none — direct provider use)_ |
| VCL desktop (§8.2) | `TfrmHorseVCLHost` in `Horse.Provider.CrossSocket.VCL` | `TfrmHorseMormotVCLHost` in `Horse.Provider.Mormot.VCL` |
| Linux daemon, Delphi (§8.3) | `THorseCrossSocketLinuxDaemonApp.Run` in `Horse.Provider.CrossSocket.Daemon` | `THorseMormotLinuxDaemonApp.Run` in `Horse.Provider.Mormot.Daemon` |
| Windows Service (§8.4) | `THorseCrossSocketService` (`TService` base) in `Horse.Provider.CrossSocket.Daemon` | `THorseMormotService` (`TService` base) in `Horse.Provider.Mormot.Daemon` |
| Linux daemon, FPC (§8.5) | `THorseCrossSocketDaemonApp.Run` in `Horse.Provider.CrossSocket.FPC.Daemon` | `THorseMormotFPCDaemonApp.Run` in `Horse.Provider.Mormot.FPC.Daemon` |
| Lazarus LCL (§8.6) | `TfrmHorseLCLHost` in `Horse.Provider.CrossSocket.FPC.LCL` | `TfrmHorseMormotLCLHost` in `Horse.Provider.Mormot.FPC.LCL` |
| FPC HTTPApplication (§8.7) | `THorseCrossSocketHTTPApp.Run` in `Horse.Provider.CrossSocket.FPC.HTTPApplication` | `THorseMormotHTTPApp.Run` in `Horse.Provider.Mormot.FPC.HTTPApplication` |

The same `{$IFDEF MSWINDOWS}` / `{$ELSE}` rule that gives `HORSE_APPTYPE_DAEMON` two meanings under CrossSocket applies to mORMot too: `Horse.Provider.Mormot.Daemon.pas` carries `THorseMormotService` (Windows TService) under `{$IFDEF MSWINDOWS}` and `THorseMormotLinuxDaemonApp` (POSIX signal-handler runner) under `{$ELSE}`.

### 9.2 Minimal example — Console (mORMot2)

```pascal
program MyMormotServer;

{$APPTYPE CONSOLE}                 // → IsConsole = True

uses
  Winapi.Windows, Horse;           // resolves THorse → THorseProviderMormot

function CtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  case dwCtrlType of
    CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT,
    CTRL_SHUTDOWN_EVENT:
      begin
        THorse.StopListen;
        Result := True;
      end;
  else
    Result := False;
  end;
end;

begin
  SetConsoleCtrlHandler(@CtrlHandler, True);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin Res.Send('pong'); end);

  THorse.Listen(9000);             // blocks
end.
```

Define: `HORSE_PROVIDER_MORMOT`. Project type: **Console Application**. No CrossSocket-style convenience runner here — Console is direct provider use, identical to §8.1 except for the resolved unit behind `THorse`.

### 9.3 Tuning the mORMot transport

Unlike CrossSocket (which uses `THorseWorkerPool` for the Horse pipeline), mORMot owns its own thread pool inside `THttpServer` — default 32 threads. Adjust via `THorseMormotConfig` and the typed entry point:

```pascal
uses
  Horse, Horse.Provider.Mormot, Horse.Provider.Mormot.Config;

var
  Config: THorseMormotConfig;
begin
  Config                := THorseMormotConfig.Default;
  Config.ThreadPool     := 64;                  // bigger pool for blocking handlers
  Config.MaxBodyBytes   := 16 * 1024 * 1024;    // 16 MB
  Config.DrainTimeoutMs := 10_000;              // graceful drain window
  Config.ServerBanner   := 'MyServer/1.0';      // sent as Server: header

  THorseProviderMormot.ListenWithConfig(9000, Config);
end.
```

The full configuration record, http.sys swap (`THttpApiServer`), TLS options, and security defaults are documented in [`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot#readme).

---

## 10. Architecture note

Every self-hosted Provider implements the same `THorseProviderAbstract` base class. The Provider:

1. Owns the listening socket.
2. Parses an incoming request.
3. Calls `THorse.Execute(Req, Res)`, which runs the registered middleware chain and routes.
4. Serialises the response back to the client.

Non-Indy Providers (CrossSocket and any future transports) use a small set of lightweight interfaces — `IHorseRawRequest` / `IHorseRawResponse` — adapted to look like `TWebRequest` / `TWebResponse` for middleware compatibility. As a result, middleware that pokes `Req.RawWebRequest.Method` or `Res.RawWebResponse.SetCustomHeader` directly (e.g. `Horse.CORS`) keeps working unchanged across all Providers.

If you want to build a **new** Provider, the [hybrid-interface architecture guide](https://github.com/freitasjca/horse-provider-crosssocket/blob/master/doc/building-a-new-provider.md) in the CrossSocket repo walks through the pattern.

### 10.1 `TCP_NODELAY` on self-hosted Providers

All self-hosted Providers disable **Nagle's algorithm** (`TCP_NODELAY`) on every accepted connection:

- **Indy** (Console / Daemon / VCL) sets `AContext.Binding.UseNagle := False` from the bridge's `OnConnect`.
- **CrossSocket** calls `TSocketAPI.SetTcpNoDelay(AConnection.Socket, True)` from the server's `OnConnected` — in the shared transport, so every CrossSocket Application type (Console, VCL, Daemon, FPC variants) inherits it.
- **mORMot2** already enables `TCP_NODELAY` by default.

**Why.** Without it, on **Linux loopback** the small request/response ping-pong of an HTTP keep-alive connection collides with the kernel's ~40 ms **delayed-ACK** timer, pinning throughput at a flat ~44 ms/request floor (~2 270 req/s) regardless of how fast the server actually is. Disabling Nagle removes that stall. (Windows loopback doesn't trigger it, but the option is harmless-to-beneficial there too — lower latency.)

**Does it affect more than small responses?** It's a **per-connection** option, so it applies to *every* request, not just tiny ones — but the effect is **positive or neutral for all request shapes, never harmful**. It changes only *when* already-buffered bytes leave the socket, never *what* bytes or *in what order*; TCP remains a reliable, ordered byte stream and response content is byte-for-byte identical.

| Response shape | Effect |
|---|---|
| Small responses (JSON APIs, headers-only, 204) | **Improved** — removes the keep-alive delayed-ACK stall |
| Large responses (file downloads, big bodies) | **Neutral** — Nagle only ever held back a trailing sub-MSS chunk; that final flush is now immediate |
| POST / uploads | **Neutral** — Nagle is an outbound concern; inbound body is unaffected |

The classic downside of `TCP_NODELAY` — many tiny per-response socket writes becoming many small packets — does not apply here: Indy, CrossSocket, and Horse all buffer the full response and flush it in one (or few) writes. This matches universal HTTP-server practice (nginx, Apache, Go `net/http`, mORMot all set `TCP_NODELAY` unconditionally).

> The FPC `fphttpserver` family and host-managed types (Apache / ISAPI / CGI / FastCGI) are **not** changed: `fphttpserver` exposes no clean per-connection hook, and host-managed deployments don't own the HTTP accept socket — the front web server's own Nagle policy applies.

### 10.2 Indy provider connection defaults — `MaxConnections` & `ListenQueue`

The Indy self-hosted providers (Console / Daemon / VCL) now apply two **safe defaults** when you
don't set them explicitly:

| Property | Old effective default | New default | Why |
|---|---|---|---|
| `THorse.MaxConnections` | **32** (WebBroker's `Web.WebReq.TWebRequestHandler` module-pool cap) | **1024** | The 32 cap returns **~60 % HTTP 500** (`EWebBrokerException: "Maximum number of concurrent connections exceeded"`) under **keep-alive + response-header middleware + concurrency ≥ ~40**. Raising the module-pool ceiling makes the out-of-the-box build safe. |
| `THorse.ListenQueue` | **15** (Indy's `IdListenQueueDefault`) | **511** | 15 is too small for concurrent connection bursts → dropped/refused connections. 511 mirrors nginx's backlog (the OS clamps it to `net.core.somaxconn` / `SOMAXCONN`). |

**Behaviour & compatibility.** These are applied **only when the value is left unset** — if you
already call `THorse.MaxConnections := N` or `THorse.ListenQueue := N` before `Listen`, your value
wins, unchanged. The `MaxConnections` default raises **only** the WebBroker module-pool ceiling
(the thing that produced the 500s); it does **not** impose an Indy TCP connection cap unless you set
one. Tune both up for very high concurrency (e.g. `1000`+ at c≈500).

```pascal
THorse.MaxConnections := 4096;   // optional — overrides the 1024 default
THorse.ListenQueue    := 1024;   // optional — overrides the 511 default
THorse.Listen(9000);
```

> **Provider scope:** this applies to the **Indy** providers only — they're the ones backed by
> WebBroker. CrossSocket and mORMot have no WebBroker module pool (they never produced these 500s);
> CrossSocket connection limits live in `THorseCrossSocketConfig.MaxConnections`, and mORMot sizes
> its own fixed thread pool (`THorseMormotConfig.ThreadPool`, default 32). The constants live in
> `Horse.Provider.Config` (`DEFAULT_MAX_CONNECTIONS`, `DEFAULT_LISTEN_QUEUE`).

#### How to size them — and why they're **not** derived from CPU count

These two values govern **I/O concurrency and connection-burst absorption**, not compute — so they
are deliberately **fixed defaults, not a function of the machine's CPU count.** Size them against the
variables that actually matter:

- **`ListenQueue` = the kernel TCP accept backlog** — a *burst buffer* for connections waiting to be
  `accept()`ed. Size it to your **peak connection-arrival burst**, not your cores; a faster box drains
  the queue *quicker* and needs *less* backlog, not more. The OS hard-clamps it to
  `net.core.somaxconn` (Linux) / `SOMAXCONN` (Windows) — raise that to match if you set a high value.
- **`MaxConnections` = a ceiling on concurrent in-flight requests.** The useful level is driven by
  your handlers' **I/O profile**: CPU-bound handlers saturate near core-count, but typical
  **I/O-bound** handlers (DB, external HTTP, files) profitably run far more concurrent requests than
  there are cores, because most are blocked waiting. It's a **safety ceiling**, so the practical limit
  is **RAM / `ulimit -n`** — Indy is thread-per-connection (~1 MB stack + one fd per connection), so
  e.g. 1024 connections ≈ up to ~1 GB of thread stacks worst-case.

> **Why not scale with CPU?** `MaxConnections` is a *ceiling*; deriving it from cores would give small
> boxes a *low* cap and re-introduce the keep-alive 500s on exactly the machines least able to absorb
> them. CPU count belongs on the knobs that count multiplexing/compute threads — and those already
> auto-scale: CrossSocket's IO threads (`CPUCount*2+1`) and the CPU-bound `THorseWorkerPool` (4–64
> threads). Connection ceilings and accept backlogs are a different category.

**Rule of thumb:** raise `MaxConnections` toward your expected peak concurrent in-flight requests
(bounded by RAM/fd limits), and `ListenQueue` toward your expected peak accept burst (bounded by
`somaxconn`). Leave them at the defaults until a load test says otherwise.

## See also

- [Getting Started](./getting-started.md) — your first Console + Indy server.
- [Middleware Ecosystem](./middleware-ecosystem.md) — middleware that works across all Providers.
- [Compiler Support](./compiler-support.md) — Delphi/FPC versions per Provider and Application type.
- [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket) — the optional CrossSocket async Provider's own documentation.
- [`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot) — the optional mORMot2 async Provider's own documentation.
