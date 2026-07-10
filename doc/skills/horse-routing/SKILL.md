---
name: horse-routing
description: Guidelines for setting up endpoints, defining path parameters, route wildcards, and structuring HTTP controllers.
---

# Horse Routing

## Defining HTTP Endpoints
Horse supports registering endpoints for all standard HTTP verbs using static or instance methods:

```pascal
THorse.Get('/products', GetProductsHandler);
THorse.Post('/products', CreateProductHandler);
THorse.Put('/products', UpdateProductHandler);
THorse.Delete('/products', DeleteProductHandler);
THorse.Patch('/products', PatchProductHandler);
```

---

## Route Parameters
Path parameters are defined using the **colon syntax (`:param`)**:

*   **Correct**: `THorse.Get('/users/:id', GetUserHandler);`
*   **Incorrect**: `THorse.Get('/users/{id}', GetUserHandler);`

Inside the handler, you access the parameter via `Req.Params.Items['id']` (or `Req.Params['id']`).

---

## Wildcards
Use the wildcard asterisk (`*`) to match any subpath:

```pascal
// Matches /public/css/site.css, /public/images/logo.png, etc.
THorse.Get('/public/*', ServeStaticFilesHandler);
```

---

## Route Grouping & Prefixes (THorse.Group)
You can group related routes under a common path prefix using `THorse.Group`. This is highly useful for API versioning (e.g., `/v1`) and isolating group-level middlewares:

*   **Prefixing Routes**:
    ```pascal
    THorse.Group.Prefix('/v1')
      .Get('/products', GetProductsHandler)
      .Get('/customers', GetCustomersHandler);
    ```

*   **Group-level Middlewares (Restricted Middlewares)**:
    Instead of registering a middleware globally via `THorse.Use`, you can register it specifically for a group using `.Use()`. The middleware will only run for endpoints defined inside that group:
    ```pascal
    THorse.Group.Prefix('/admin')
      .Use(AuthMiddleware) // Only runs for routes under /admin
      .Get('/dashboard', GetDashboardHandler)
      .Get('/settings', GetSettingsHandler);
    ```

## Route-level Middlewares (Local)
You can pass an array of route-specific middlewares (`array of THorseCallback`) to apply checks only to a single endpoint. These run after global and group-level middlewares:

*   **Static routing**:
    ```pascal
    THorse.Get('/admin/dashboard', [AuthMiddleware, LoggerMiddleware], GetDashboardHandler);
    ```

*   **Fluent routing**:
    ```pascal
    THorse.Route('/reports')
      .Get([AuthMiddleware, LoggerMiddleware], GetReportsHandler)
      .Post([AuthMiddleware], CreateReportHandler);
    ```

---

## Grouping with Controllers
To keep routes organized, group them logically within static class methods or dedicated units:

```pascal
type
  TUserController = class
  public
    class procedure RegisterRoutes;
    class procedure ListUsers(Req: THorseRequest; Res: THorseResponse; Next: TProc);
  end;

class procedure TUserController.RegisterRoutes;
begin
  THorse.Get('/users', ListUsers);
end;
```
