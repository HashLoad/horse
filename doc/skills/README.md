# Horse Framework — Agent Skills

> [!NOTE]
> **Purpose**: This directory contains instruction packages optimized for AI agents (such as Antigravity, Claude, and GitHub Copilot). While the standard documentation is designed for human developers, these skills are optimized to help AIs quickly understand and apply correct, memory-safe, and idiomatic patterns when working with the Horse Web Framework.

<p align="center">
  <i>Read this in <a href="./README.md">English</a> or <a href="./README.pt-BR.md">Português (BR)</a>.</i>
</p>

---

## Available Skills

| Skill Name | Path | Load When |
| :--- | :--- | :--- |
| **horse-app-structure** | [`horse-app-structure/SKILL.md`](./horse-app-structure/SKILL.md) | Setting up a new project, setting up the `.dpr` bootstrap, initial middleware pipeline configuration, or port listening. |
| **horse-routing** | [`horse-routing/SKILL.md`](./horse-routing/SKILL.md) | Configuring HTTP methods, route parameters, wildcards, or grouping routes within Controllers. |
| **horse-middlewares** | [`horse-middlewares/SKILL.md`](./horse-middlewares/SKILL.md) | Configuring official middlewares (Johnson, CORS, basic-auth, compression, logger) and ensuring correct execution order. |
| **horse-request-response** | [`horse-request-response/SKILL.md`](./horse-request-response/SKILL.md) | Reading request payload, query parameters, route parameters, headers, or sending HTTP responses and setting statuses. |
| **horse-files-streams** | [`horse-files-streams/SKILL.md`](./horse-files-streams/SKILL.md) | Handling file uploads (multipart formData), file downloads, or custom data streams. |
| **horse-providers** | [`horse-providers/SKILL.md`](./horse-providers/SKILL.md) | Choosing or configuring server adapters (Indy, CGI, ISAPI, Apache, Daemon, HTTP.sys). |
| **horse-writing-middleware** | [`horse-writing-middleware/SKILL.md`](./horse-writing-middleware/SKILL.md) | Implementing custom middlewares for Horse following the correct signature and execution sequence. |
| **horse-database-pooling** | [`horse-database-pooling/SKILL.md`](./horse-database-pooling/SKILL.md) | Enforcing thread-safety during database connection setup, configuring connection pooling (FireDAC / UniDAC). |
| **horse-security-auth** | [`horse-security-auth/SKILL.md`](./horse-security-auth/SKILL.md) | Securing endpoints using JWT or Basic-Auth middlewares and configuring protected route groups. |
| **horse-ssl-tls** | [`horse-ssl-tls/SKILL.md`](./horse-ssl-tls/SKILL.md) | Enabling SSL/TLS (HTTPS) configuration across different providers (HTTP.sys, ICS). |
| **horse-integration-tests** | [`horse-integration-tests/SKILL.md`](./horse-integration-tests/SKILL.md) | Writing automated HTTP tests for endpoints using DUnit/DUnitX and mock server configurations. |
| **horse-lazarus-compatibility** | [`horse-lazarus-compatibility/SKILL.md`](./horse-lazarus-compatibility/SKILL.md) | Ensuring Lazarus and FPC cross-compiler compatibility, handling anonymous methods differences and FPC JSON libs. |
| **horse-performance-tuning** | [`horse-performance-tuning/SKILL.md`](./horse-performance-tuning/SKILL.md) | Writing high-performance handlers, optimizing heap allocations, and selecting optimal transport providers. |
| **horse-zero-allocation** | [`horse-zero-allocation/SKILL.md`](./horse-zero-allocation/SKILL.md) | Coding for Zero-Allocation, using stack buffers, string slices, object pooling, and avoiding thread lock contention. |
| **horse-mvc-architecture** | [`horse-mvc-architecture/SKILL.md`](./horse-mvc-architecture/SKILL.md) | Structuring corporate APIs using Clean MVC principles, separating transport from business logic. |
| **horse-minimal-api** | [`horse-minimal-api/SKILL.md`](./horse-minimal-api/SKILL.md) | Building rapid, low-boilerplate microservices and mock APIs inside single-file bootstrap models. |

---

## Critical Rules for AIs (Apply to All Skills)

1. **Middlewares Order**: The registration order is **critical**. Middlewares like `CORS` and `Jhonson` must be registered **before** defining any routes.
2. **Johnson Memory Management**: The Johnson middleware takes ownership of any `TJSONObject` or `TJSONArray` sent via `Res.Send<T>`. **NEVER** call `.Free` on a JSON object after sending it through `Res.Send<T>` when Johnson is active.
3. **Route Parameters**: Parameters are defined using the colon syntax (e.g., `/products/:id`), not curly braces (e.g., `/products/{id}`).
4. **Thread-Safety**: Horse is inherently multithreaded. **NEVER** share physical database connections (`TFDConnection` or query components) globally across requests. Every route handler must instantiate its own database connection (preferably using connection pooling) or protect shared resources using locks (`TCriticalSection`).
5. **Console Output**: Call `SetConsoleCharSet` in console mode endpoints if necessary to prevent character encoding issues.
6. **Stream Management**: The `THorseResponse` object takes ownership of any stream passed to `SendFile`, `Download`, or `Render`. **NEVER** call `.Free` or `FreeAndNil` on a stream after sending it via these response methods.
