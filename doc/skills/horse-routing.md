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
