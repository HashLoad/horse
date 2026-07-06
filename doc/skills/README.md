# Horse Framework — Agent Skills

> [!NOTE]
> **Purpose**: This directory contains instruction packages optimized for AI agents (such as Antigravity, Claude, and GitHub Copilot). While the standard documentation is designed for human developers, these skills are optimized to help AIs quickly understand and apply correct, memory-safe, and idiomatic patterns when working with the Horse Web Framework.

---

## Available Skills

| Skill Name | Filename | Load When |
| :--- | :--- | :--- |
| **horse-app-structure** | `horse-app-structure.md` | Setting up a new project, setting up the `.dpr` bootstrap, initial middleware pipeline configuration, or port listening. |
| **horse-routing** | `horse-routing.md` | Configuring HTTP methods, route parameters, wildcards, or grouping routes within Controllers. |
| **horse-middlewares** | `horse-middlewares.md` | Configuring official middlewares (Johnson, CORS, basic-auth, compression, logger) and ensuring correct execution order. |
| **horse-request-response** | `horse-request-response.md` | Reading request payload, query parameters, route parameters, headers, or sending HTTP responses and setting statuses. |
| **horse-files-streams** | `horse-files-streams.md` | Handling file uploads (multipart formData), file downloads, or custom data streams. |
| **horse-providers** | `horse-providers.md` | Choosing or configuring server adapters (Indy, CGI, ISAPI, Apache, Daemon, HTTP.sys). |
| **horse-writing-middleware** | `horse-writing-middleware.md` | Implementing custom middlewares for Horse following the correct signature and execution sequence. |

---

## Critical Rules for AIs (Apply to All Skills)

1. **Middlewares Order**: The registration order is **critical**. Middlewares like `CORS` and `Jhonson` must be registered **before** defining any routes.
2. **Johnson Memory Management**: The Johnson middleware takes ownership of any `TJSONObject` or `TJSONArray` sent via `Res.Send<T>`. **NEVER** call `.Free` on a JSON object after sending it through `Res.Send<T>` when Johnson is active.
3. **Route Parameters**: Parameters are defined using the colon syntax (e.g., `/products/:id`), not curly braces (e.g., `/products/{id}`).
4. **Thread-Safety**: Horse is inherently multithreaded. **NEVER** share physical database connections (`TFDConnection` or query components) globally across requests. Every route handler must instantiate its own database connection (preferably using connection pooling) or protect shared resources using locks (`TCriticalSection`).
5. **Console Output**: Call `SetConsoleCharSet` in console mode endpoints if necessary to prevent character encoding issues.
6. **Stream Management**: The `THorseResponse` object takes ownership of any stream passed to `SendFile`, `Download`, or `Render`. **NEVER** call `.Free` or `FreeAndNil` on a stream after sending it via these response methods.
