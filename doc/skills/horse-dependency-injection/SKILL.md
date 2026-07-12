---
name: horse-dependency-injection
description: Guide for managing request-scoped contextual services and IoC (dependency injection) in Delphi and Lazarus.
---

# Horse Contextual Dependency Injection (Request Scope)

## 1. Request Scope Architecture
The `Services` property in `THorseRequest` provides a thread-safe, request-scoped Inversion of Control (IoC) container. This container manages the lifecycle of classes and objects that need to exist only during the execution of a single HTTP request (e.g., database connections, repository patterns, user context).

*   **Ownership**: The container takes ownership (`doOwnsValues := True`) of registered instances.
*   **Automatic Disposal**: All registered service instances are automatically destroyed when the request finishes, eliminating memory leaks and removing the need for manual `try/finally` blocks inside route handlers.

---

## 2. Direct Instance Injection (`Add`)
Use `Add` to register an already instantiated object. The container takes ownership and will destroy the instance when the request ends.

```pascal
procedure SetContextMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LUserContext: TUserContext;
begin
  LUserContext := TUserContext.Create;
  LUserContext.UserId := Req.Headers['X-User-ID'];
  
  // Register the instance
  Req.Services.Add(TUserContext, LUserContext);
  Next();
end;

procedure GetProfileHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LUserContext: TUserContext;
begin
  // Resolve and use the registered service
  LUserContext := TUserContext(Req.Services.Resolve(TUserContext));
  Res.Send('Profile data for user: ' + LUserContext.UserId);
end;
```

---

## 3. Lazy Factory Injection (`AddFactory`)
Use `AddFactory` to register a factory delegate. The object will only be instantiated at the exact moment `Resolve` is called (*Lazy Loading*). Once instantiated, it is cached for subsequent resolves in the same request and destroyed at the end.

This is highly recommended for heavy resources (like database connections) that might not be needed in every execution path of a route.

```pascal
procedure RegisterDatabaseMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  // Register the factory method
  Req.Services.AddFactory(TFDConnection,
    function: TObject
    begin
      Result := TFDConnection.Create(nil);
      TFDConnection(Result).ConnectionDefName := 'PooledPGDef';
      TFDConnection(Result).Connected := True;
    end);
  Next();
end;

procedure QueryUsersHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LConnection: TFDConnection;
  LQuery: TFDQuery;
begin
  // The connection is physically created and opened only on the next line!
  LConnection := TFDConnection(Req.Services.Resolve(TFDConnection));
  
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Connection := LConnection;
    LQuery.SQL.Text := 'SELECT id, name FROM users';
    LQuery.Open;
    Res.Send(LQuery.ToJSONArray);
  finally
    LQuery.Free;
  end; // LConnection is automatically freed by the container when the request ends!
end;
```

---

## 4. Best Practices for Dependency Injection in Horse
1.  **Avoid Global Resolving**: Never resolve services outside the active request flow. The `Services` container belongs exclusively to the thread handling the current `THorseRequest`.
2.  **Cast Returned Objects**: The `Resolve` method returns a raw `TObject` to preserve compatibility across old Delphi versions. You must cast it to your concrete class (e.g., `TMyService(Req.Services.Resolve(TMyService))`).
3.  **One Registry Per Class Type**: Only one instance or factory can be registered per class type in the same request. Registering a class that is already registered will raise an exception.
4.  **No Manual Frees**: Do not call `.Free` or `FreeAndNil` on objects resolved from `Req.Services` that you want to persist until the end of the request.
