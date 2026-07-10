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

## Middleware Scope & Execution Flow
Middlewares can be declared at three levels:
1. **Global** (via `THorse.Use`): Runs on every request.
2. **Group-level** (via `THorseGroup.Use`): Runs on all routes inside a specific group prefix.
3. **Route-level (Local)**: Runs only on that specific route (defined as an array `[Middleware1, Middleware2]` in the verb method).

### Execution Flow:
The execution order follows the nested onion pattern:
```
Global Middlewares → Group Middlewares → Route-level Middlewares → Final Route Handler
```

Always design middlewares to call `Next()` to pass control, or skip it to short-circuit the request (e.g., unauthorized requests).

---

## The Johnson Middleware (Critical)
The **`Jhonson`** middleware automatically handles JSON parsing (`TJSONObject` / `TJSONArray`) for requests and responses.

*   **Ownership Transfer**: When you call `Res.Send<TJSONObject>(LJson)` or `Res.Send<TJSONArray>(LArray)`, the Johnson middleware takes ownership of that object and will **automatically destroy it** after sending.
*   **Safety Rule**: **NEVER** call `.Free` or `FreeAndNil` on a JSON object after sending it through `Res.Send<T>`. Doing so will cause a Double-Free memory corruption (Access Violation) when the middleware attempts to clean it up.

---

## Global Error Handler (OnError)
Horse features a native global error handling callback (`THorse.OnError`). It intercepts all unhandled exceptions occurring inside any middleware or route handler.

### Key Points:
* **Registration**: Use `THorse.OnError(MyGlobalErrorHandler)` during application startup.
* **Control Exceptions**: Control exceptions (`EHorseCallbackInterrupted` and `EHorseException`) are bypassed and **do not** trigger the global `OnError` callback.
* **Safety (Fail-Safe)**: If the registered `OnError` callback itself crashes, the framework handles the crash safely and returns a structured `500 Internal Server Error` response with the exception detail, protecting the socket from leaking or crashing.
* **Signature**: The callback is a classic procedure type `THorseOnError = procedure(const ARequest: THorseRequest; const AResponse: THorseResponse; const AException: Exception)`.
