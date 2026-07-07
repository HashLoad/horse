---
name: horse-middlewares
description: Guide to using standard Horse middlewares (CORS, Jhonson, compression, basic-auth, logger) and pipeline registration order.
---

# Horse Middlewares

## Core Middleware Pipeline
Middlewares intercept and process HTTP requests/responses before reaching the endpoint handlers.

### Required Registration Order
The registration order of middlewares in `THorse` is **highly critical**. Middlewares must be registered **before** defining any routes:

```pascal
begin
  // 1. Logging and Error Handling (FIRST)
  THorse.Use(HandleException);
  
  // 2. Security and Headers
  THorse.Use(CORS);
  
  // 3. Payload Compression and Formatting
  THorse.Use(OctetStream);
  THorse.Use(Jhonson); // Parse JSON body and format JSON response
  
  // 4. Register Routes (AFTER all global middlewares)
  TProductController.RegisterRoutes;
  
  THorse.Listen(9000);
end;
```

---

## The Johnson Middleware (Critical)
The **`Jhonson`** middleware automatically handles JSON parsing (`TJSONObject` / `TJSONArray`) for requests and responses.

*   **Ownership Transfer**: When you call `Res.Send<TJSONObject>(LJson)` or `Res.Send<TJSONArray>(LArray)`, the Johnson middleware takes ownership of that object and will **automatically destroy it** after sending.
*   **Safety Rule**: **NEVER** call `.Free` or `FreeAndNil` on a JSON object after sending it through `Res.Send<T>`. Doing so will cause a Double-Free memory corruption (Access Violation) when the middleware attempts to clean it up.
