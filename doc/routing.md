# Routing

*Read this in [English](./routing.md) or [Português (BR)](./routing.pt-BR.md).*

A *route* binds a URL path + HTTP method to a callback. This document covers everything Horse can express in a route definition.

For the API of the request and response objects passed to each callback, see [Request & Response](./request-response.md).

---

## Basic routes

`THorse` exposes one method per HTTP verb. Each takes a path string and a callback:

```delphi
THorse.Get   ('/ping',     procedure(Req: THorseRequest; Res: THorseResponse) begin Res.Send('pong');    end);
THorse.Post  ('/items',    procedure(Req: THorseRequest; Res: THorseResponse) begin Res.Send('created'); end);
THorse.Put   ('/items/:id', ...);
THorse.Patch ('/items/:id', ...);
THorse.Delete('/items/:id', ...);
THorse.Head  ('/items/:id', ...);
```

Method-routing is exact: `THorse.Get` only matches `GET` requests to that path. A request with the wrong method on a known path returns `405 Method Not Allowed`. A request with an unknown path returns `404 Not Found`.

## Path parameters

Use a colon-prefixed segment to capture part of the URL:

```delphi
THorse.Get('/users/:id',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    Res.Send('User: ' + Req.Params['id']);
  end);
```

- `GET /users/42` → `User: 42` (and `Req.Params['id'] = '42'`)
- `GET /users/`   → `404`
- `GET /users`    → `404`

Multiple parameters in a single path work the same way:

```delphi
THorse.Get('/teams/:teamId/members/:memberId',
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    T, M: string;
  begin
    T := Req.Params['teamId'];
    M := Req.Params['memberId'];
    Res.Send(Format('Team %s, member %s', [T, M]));
  end);
```

Path parameters are always strings. Convert them yourself with `StrToInt`, `TryStrToInt`, etc.

## Query strings

Query strings are accessed via `Req.Query`:

```delphi
// GET /search?name=Horse&category=framework
THorse.Get('/search',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    Res.Send('Looking for ' + Req.Query['name'] + ' in ' + Req.Query['category']);
  end);
```

Missing keys return an empty string. If you need to know whether a key was present, use `Req.Query.TryGetValue` (returns `False` when absent).

## Route groups

Group related routes under a common prefix:

```delphi
THorse.Group.Prefix('/api/v1')
  .Get   ('/users',     ListUsers)
  .Post  ('/users',     CreateUser)
  .Get   ('/users/:id', GetUser)
  .Put   ('/users/:id', UpdateUser)
  .Delete('/users/:id', DeleteUser);
```

The above is identical to writing `THorse.Get('/api/v1/users', ...)`, etc. Groups can carry their own middleware:

```delphi
THorse.Group
  .Use(JWT(SECRET))           // middleware applies to everything in this group
  .Prefix('/api/v1/admin')
  .Get ('/stats', GetStats)
  .Post('/audit', WriteAudit);
```

## Wildcard middleware

`THorse.Use(...)` registers middleware that runs on every request, regardless of path:

```delphi
THorse.Use(MyLogger);            // every request is logged
THorse.Use('/api', RequireAuth); // only /api/* requires auth
```

See [Middleware](./middleware.md) for the full story.

## The `TMethodType` enum

Internally, routes are stored by method. The enum lives in `Horse.Commons`:

```pascal
type
  TMethodType = (mtAny, mtGet, mtPut, mtPost, mtHead, mtDelete, mtPatch);
```

`mtAny` is the wildcard used by middleware (`THorse.Use`) — it matches any method.

> **Note:** `OPTIONS`, `TRACE`, and `CONNECT` are **not** in `TMethodType`. They route as `mtAny` (matching wildcard middleware only). The `Horse.CORS` middleware uses this to intercept `OPTIONS` for preflight handling. If you need to discriminate by raw verb in your own middleware, use `Req.Method: string` or `Req.RawWebRequest.Method`.

## Pattern matching rules

- **Case-sensitive** path matching: `/Users` and `/users` are different routes.
- **No trailing slash normalisation**: `/users` and `/users/` are different routes. Decide your project convention and stick to it.
- **First-registered wins for identical patterns** — registering `/users/:id` twice is a duplicate; the second registration emits a runtime error in recent Horse versions (`Duplicate route detected: [GET] /users/:id`).
- **Parameter segments cannot have multiple colons** — `/users/:id:name` is invalid; use two segments instead (`/users/:id/:name`).

## Sub-resources

There's no built-in `mount` like Express, but you can express sub-resources with groups:

```delphi
procedure RegisterUsersRoutes(AGroup: THorseCoreGroup);
begin
  AGroup
    .Get   ('',         ListUsers)
    .Post  ('',         CreateUser)
    .Get   ('/:id',     GetUser);
end;

// In main:
RegisterUsersRoutes(THorse.Group.Prefix('/api/v1/users'));
```

The same `RegisterUsersRoutes` can be mounted under multiple prefixes (`/api/v1/users` and `/api/v2/users`) without duplication — useful for API versioning when v2 only adds new endpoints.

## Listing routes (for debugging)

Horse doesn't ship a `printRoutes()` helper, but you can iterate the router tree directly. The simpler approach is to print each route as you register it:

```delphi
procedure RegisterAndLog(const Method, Path: string; Cb: THorseCallback);
begin
  case Method of
    'GET':  THorse.Get   (Path, Cb);
    'POST': THorse.Post  (Path, Cb);
    // ...
  end;
  WriteLn(Method, ' ', Path);
end;
```

For non-trivial apps, keep the route registration centralised in one unit so the layout is obvious from a single file.

## Radix Router (Optional - Extreme Performance)

For large-scale applications or high-performance APIs with hundreds of routes, Horse optionally includes a routing engine based on a **Prefix Tree (Radix Tree)**.

Unlike the default linear router which performs path resolution in $O(N)$ (scanning routes sequentially), the Radix router resolves URLs in $O(K)$, where $K$ is the path string length. This guarantees extreme throughput and constant-time routing performance, regardless of the amount of registered routes in your application.

### Activating the Radix Router

Simply invoke the static class procedure `THorse.UseRadixRouter` at the very beginning of your application setup, before registering any route handlers:

```delphi
begin
  THorse.UseRadixRouter;

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

## See also

- [Request & Response](./request-response.md) — what you do inside the callback.
- [Middleware](./middleware.md) — `THorse.Use`, the `Next` proc, registration order.
- [Providers](./providers.md) — how the transport layer hands the request to your route callback.
