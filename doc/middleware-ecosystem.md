# Middleware Ecosystem

*Read this in [English](./middleware-ecosystem.md) or [Português (BR)](./middleware-ecosystem.pt-BR.md).*

Horse keeps a small core. Almost everything you'd associate with a "web framework" — JSON parsing, CORS, JWT, compression, logging — lives in separate, opt-in middleware packages. This page catalogues the official ones (maintained by HashLoad) and the community list.

For how middleware actually works, see [Middleware](./middleware.md).

---

## Official middleware (HashLoad/*)

For a more maintainable ecosystem, the official middleware lives in separate repositories rather than the Horse core. Install with Boss:

```sh
boss install <repo>
```

| Middleware | What it does | Delphi | Lazarus |
|---|---|:---:|:---:|
| [horse/jhonson](https://github.com/HashLoad/jhonson) | Parses JSON request bodies, serialises JSON responses | ✔️ | ✔️ |
| [horse/basic-auth](https://github.com/HashLoad/horse-basic-auth) | HTTP Basic authentication | ✔️ | ✔️ |
| [horse/cors](https://github.com/HashLoad/horse-cors) | Cross-Origin Resource Sharing headers + preflight | ✔️ | ✔️ |
| [horse/stream](https://github.com/HashLoad/horse-octet-stream) | `application/octet-stream` body handling | ✔️ | ✔️ |
| [horse/jwt](https://github.com/HashLoad/horse-jwt) | JSON Web Token authentication | ✔️ | ✔️ |
| [horse/exception](https://github.com/HashLoad/handle-exception) | Convert raised exceptions to consistent JSON error responses | ✔️ | ✔️ |
| [horse/logger](https://github.com/HashLoad/horse-logger) | Per-request access log | ✔️ | ✔️ |
| [horse/compression](https://github.com/HashLoad/horse-compression) | gzip / deflate response compression | ✔️ | ✔️ |

### Typical composition

```delphi
uses
  Horse,
  Horse.Jhonson,
  Horse.CORS,
  Horse.HandleException,
  Horse.Logger,
  Horse.JWT;

begin
  THorse
    .Use(Horse.HandleException.New)    // outermost — turn errors into clean responses
    .Use(Horse.Logger.New)             // log every request
    .Use(CORS)                         // CORS headers + OPTIONS preflight
    .Use(Jhonson)                      // parse JSON bodies
    .Use(JWT('secret'));               // require a valid token (innermost)

  // routes …

  THorse.Listen(9000);
end.
```

Registration order matters — see [Middleware](./middleware.md#registration).

---

## Third-party middleware (community)

These are not maintained by HashLoad but are listed here because they're commonly used. The list grows over time — open a PR against this doc if yours should be here.

| Middleware | Description | Delphi | Lazarus |
|---|---|:---:|:---:|
| [bittencourtthulio/etag](https://github.com/bittencourtthulio/Horse-ETag) | ETag generation | ✔️ | ✔️ |
| [bittencourtthulio/paginate](https://github.com/bittencourtthulio/Horse-Paginate) | List response pagination | ✔️ | ✔️ |
| [bittencourtthulio/cachecontrol](https://github.com/bittencourtthulio/horse-cachecontrol) | Cache-Control response headers | ✔️ | ❌ |
| [gabrielbaltazar/gbswagger](https://github.com/gabrielbaltazar/gbswagger) | Swagger / OpenAPI generation | ✔️ | ❌ |
| [willhubner/socketIO](https://github.com/WillHubner/Horse-SocketIO) | Socket.IO server | ✔️ | ❌ |
| [dliocode/ratelimit](https://github.com/dliocode/horse-ratelimit) | Rate limiting | ✔️ | ❌ |
| [dliocode/slowdown](https://github.com/dliocode/horse-slowdown) | Progressive request delay | ✔️ | ❌ |
| [giorgiobazzo/upload](https://github.com/giorgiobazzo/horse-upload) | File upload helpers | ✔️ | ❌ |
| [dliocode/query](https://github.com/dliocode/horse-query) | Query-string DSL helpers | ✔️ | ❌ |
| [CarlosHe/healthcheck](https://github.com/CarlosHe/horse-healthcheck) | `/healthz` endpoint | ✔️ | ❌ |
| [CarlosHe/staticfiles](https://github.com/CarlosHe/horse-staticfiles) | Static file serving | ✔️ | ❌ |
| [CachopaWeb/horse-server-static](https://github.com/CachopaWeb/horse-server-static) | Static file serving (alternative) | ✔️ | ✔️ |
| [arvanus/horse-exception-logger](https://github.com/arvanus/horse-exception-logger) | Exception logging | ✔️ | ✔️ |
| [claudneysessa/Horse-CSResponsePagination](https://github.com/claudneysessa/Horse-CSResponsePagination) | Response pagination | ✔️ | ❌ |
| [claudneysessa/Horse-XSuperObjects](https://github.com/claudneysessa/Horse-XSuperObjects) | XSuperObject integration | ✔️ | ❌ |
| [andre-djsystem/horse-bearer-auth](https://github.com/andre-djsystem/horse-bearer-auth) | Bearer-token authentication | ✔️ | ✔️ |
| [andre-djsystem/horse-manipulate-request](https://github.com/andre-djsystem/horse-manipulate-request) | Request mutation helpers | ✔️ | ✔️ |
| [andre-djsystem/horse-manipulate-response](https://github.com/andre-djsystem/horse-manipulate-response) | Response mutation helpers | ✔️ | ✔️ |
| [antoniojmsjr/Horse-IPGeoLocation](https://github.com/antoniojmsjr/Horse-IPGeoLocation) | IP geolocation lookup | ✔️ | ❌ |
| [antoniojmsjr/Horse-XMLDoc](https://github.com/antoniojmsjr/Horse-XMLDoc) | XML documentation generation | ✔️ | ❌ |
| [isaquepinheiro/horse-jsonbr](https://github.com/HashLoad/JSONBr) | JSON helpers | ✔️ | ❌ |
| [IagooCesaar/Horse-JsonInterceptor](https://github.com/IagooCesaar/Horse-JsonInterceptor) | JSON request/response interceptor | ✔️ | ❌ |
| [dliocode/horse-datalogger](https://github.com/dliocode/horse-datalogger) | Structured data logger | ✔️ | ❌ |
| [marcobreveglieri/horse-prometheus-metrics](https://github.com/marcobreveglieri/horse-prometheus-metrics) | Prometheus metrics endpoint | ✔️ | ❌ |
| [weslleycapelari/horse-documentation](https://github.com/weslleycapelari/horse-documentation) | Auto-generated API docs | ✔️ | ❌ |
| [weslleycapelari/horse-validator](https://github.com/weslleycapelari/horse-validator) | Request payload validation | ✔️ | ❌ |

## Third-party transport Providers

Beyond middleware, the community has produced alternative **transport Providers** — the layer Horse uses to own the socket and parse HTTP. This is a separate axis from the middleware list above (see [Providers & Application types](./providers.md) for the full conceptual model). The most actively maintained third-party Provider is:

| Provider | Description | License | Repository |
|---|---|---|---|
| **horse-provider-crosssocket** | Async IOCP / epoll / kqueue transport. Replaces the default Indy Provider with [Delphi-Cross-Socket](https://github.com/winddriver/Delphi-Cross-Socket). Targets high-concurrency deployments. | MIT | [freitasjca/horse-provider-crosssocket](https://github.com/freitasjca/horse-provider-crosssocket) |

See [Providers & Application types](./providers.md) for when to switch off the default Indy transport, the compatibility matrix vs. each Application type, and how to combine the two choices.

## Adding your middleware to this list

1. Publish your package with a Boss-compatible `boss.json`.
2. Open a PR against `doc/middleware-ecosystem.md` adding a row in the appropriate table.
3. Confirm it compiles on the matrix you're claiming (Delphi / Lazarus).

There's no exclusivity — anyone can publish a Horse-compatible package without coordination.

## See also

- [Middleware](./middleware.md) — the model, how to write your own, registration order.
- [Providers](./providers.md) — middleware works across all transports; this is why.
