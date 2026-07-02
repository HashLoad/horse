# Horse Documentation

*Read this in [English](./index.md) or [Português (BR)](./index.pt-BR.md).*

Welcome. This is the documentation hub for [Horse](https://github.com/HashLoad/horse) — an Express-inspired web framework for Delphi and Lazarus.

If you're new, start with [Getting Started](./getting-started.md). If you have a working server and want to make a specific change, jump straight to the relevant topic below.

---

## Reading order for newcomers

1. **[Getting Started](./getting-started.md)** — install, write a hello-world server, run it.
2. **[Routing](./routing.md)** — declare endpoints, path parameters, route groups, query strings.
3. **[Request & Response](./request-response.md)** — read the request, write the response, headers, cookies, sessions, file uploads/downloads.
4. **[Middleware](./middleware.md)** — chain handlers, registration order, write your own. For publishing reusable middleware, see **[Writing a Middleware](./writing-middleware.md)**.
5. **[Providers & Application types](./providers.md)** — pick the transport Provider (Indy default; CrossSocket, mORMot2, and ICS optional; HttpSys built-in, Windows) and the Application type (Console default, VCL, Daemon, LCL, HTTPApplication) — or a host-managed application type (Apache, ISAPI, CGI, FastCGI).

## Reference

| Document | What you'll find |
|---|---|
| [Getting Started](./getting-started.md) | Install via Boss; minimal Delphi and Lazarus examples; project structure conventions. |
| [Routing](./routing.md) | `THorse.Get` / `Post` / `Put` / `Delete` / `Patch` / `Head` / `Use`; path params; route groups; wildcards; HTTP method enum. |
| [Request & Response](./request-response.md) | `THorseRequest` (body, params, query, headers, cookies, sessions, multipart). `THorseResponse` (`Send`, `Status`, `ContentType`, `AddHeader`, `RedirectTo`, `SendFile`, `Download`, `RawWebResponse`). |
| [Middleware](./middleware.md) | The `Next` proc model; built-in vs custom; registration order; per-route vs global. |
| [Writing a Middleware](./writing-middleware.md) | Authoring a production-quality middleware: skeleton, configuration patterns, thread safety, Provider-neutral coding, cross-compiler pitfalls, testing matrix, Boss packaging, publishing. |
| [Providers & Application types](./providers.md) | The two-axis model: **Provider** (transport — Indy default; CrossSocket, mORMot2, ICS optional; HttpSys built-in, Windows) × **Application type** (Console / VCL / Daemon / LCL / HTTPApplication, plus host-managed Apache / ISAPI / CGI / FCGI). Compatibility matrix and selection guidance. |
| [Middleware Ecosystem](./middleware-ecosystem.md) | Official `HashLoad/*` packages and the community-maintained list. |
| [Compiler Support](./compiler-support.md) | Tested Delphi releases, FPC versions, target platforms, compiler-version guards. |
| [Deployment Cheatsheet](./deployment.md) | One-page reference for shipping a CrossSocket or mORMot2 binary as any of the seven Application shapes (Console / VCL / Daemon / Windows Service / FPC daemon / LCL / FPC HTTPApplication). |

## How the docs are organised

```
README.md                      ← landing page; one-paragraph intro and links here
doc/
├── index.md                   ← this file
├── getting-started.md         ← first 30 minutes with Horse
├── routing.md                 ← URL → handler binding
├── request-response.md        ← THorseRequest and THorseResponse API
├── middleware.md              ← chaining handlers
├── providers.md               ← choosing a transport
├── middleware-ecosystem.md    ← package catalogue
└── compiler-support.md        ← versions / platforms
```

Each document is self-contained and cross-links to the others where relevant. There is no required reading order beyond the newcomer flow above.

## Contributing to the docs

Edits welcome — open a PR against `master` modifying the relevant file under `doc/`. Keep each page focused on one topic; if a section grows beyond a few hundred lines, split it into a sibling document and link from `index.md`.

## Got stuck?

- Telegram channel: [@hashload](https://t.me/hashload)
- GitHub Issues: [`HashLoad/horse/issues`](https://github.com/HashLoad/horse/issues)
- Source code is short and readable — when in doubt, `Horse.pas`, `Horse.Request.pas`, and `Horse.Response.pas` together total under 2 000 lines.
