---
name: horse-minimal-api
description: Guide for building lightweight, low-boilerplate, and rapid HTTP services (Minimal APIs) in Horse.
---

# Horse Minimal API

For focused microservices, mock APIs, or rapid prototyping, you can leverage Horse's lightweight routing and configuration DSL to build complete HTTP services in a single file with minimal boilerplate.

---

## 1. Single-File Bootstrap Design (Delphi)
A Minimal API structures all route registrations, basic logic, and middleware imports directly within the main `.dpr` bootstrap file using inline anonymous procedures:

```pascal
program MinimalAPI;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.JSON,
  Horse,
  Horse.Jhonson;

begin
  // 1. Fluent Middleware Configuration
  THorse.Use(Jhonson);

  // 2. Inline Route Mappings
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Get('/hello/:name',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('Hello, ' + Req.Params.Field('name').AsString + '!');
    end);

  // 3. Fast JSON response
  THorse.Get('/status',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LStatus: TJSONObject;
    begin
      LStatus := TJSONObject.Create(TJSONPair.Create('status', 'online'));
      Res.Send(LStatus); // Johnson takes ownership
    end);

  // 4. Port Listening
  THorse.Listen(9000);
end.
```

---

## 2. Best Practices for Minimal APIs
Although Minimal APIs promote rapid development, follow these principles to avoid "spaghetti code":

1.  **Extract Complex Handlers**: If a route handler exceeds 25 lines of code or requires database calls, do not write it inline. Extract it to a local unit procedure.
2.  **Use Radix Router**: Opt-in to `THorse.UseRadixRouter;` to ensure maximum matching speed if your API contains path parameters.
3.  **Keep it Focused**: A Minimal API should serve a single responsibility (Microservice). If you need to manage multiple domains (e.g. Users, Products, Invoices) in the same project, migrate to modular controllers.

---

## 3. When to Transition to MVC Architecture
Transition from a Minimal API to a structured **MVC/Modular** architecture when:

*   You start mixing database logic (queries) and HTML rendering/formatting inside the `.dpr` file.
*   Your project grows beyond 10 HTTP endpoints.
*   You need to reuse database layers across multiple CLI, daemon, or GUI applications.
*   You need to write structured unit/integration tests without bootstrap coupling.
