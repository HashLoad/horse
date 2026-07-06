---
name: horse-app-structure
description: Guide for setting up Horse applications, bootstrap program (.dpr), basic console initialization, and registering modules.
---

# Horse App Structure

## Application Bootstrap (.dpr)
A standard Horse application is typically configured as a Delphi Console Application (`{$APPTYPE CONSOLE}`).

```pascal
program MyHorseApp;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse,
  Horse.Jhonson,
  Horse.CORS;

begin
  // 1. Opt-in for the High-Performance Radix Tree Router (highly recommended)
  THorse.UseRadixRouter;

  // 2. Register Global Middlewares
  THorse
    .Use(CORS)
    .Use(Jhonson);

  // 3. Register Routes / Controllers
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  // 4. Start Server with Listen Callback (prevents sequential execution blocking)
  THorse.Listen(9000,
    procedure
    begin
      Writeln('Server is running on port ' + THorse.Port.ToString);
    end);
end.
```

---

## Modular Architecture
For production grade projects, keep the `.dpr` file clean by delegating route definitions to specialized `Controller` classes:

1. **Keep Controllers stateless**: Do not store request-specific variables as fields in a controller class unless they are thread-safe.
2. **Uses Sequence**: Ensure modules containing business services or repository layers are decoupled from the HTTP transport layer (`Horse` units).

---

## Thread-Safe Data Access & Database Connections
Horse is inherently multithreaded (each request runs on a separate worker thread). Accessing a database inside a route handler requires strict thread safety to avoid memory corruption, race conditions, or application crashes (Access Violations):

1. **NEVER share connection components globally**: Do not put a global `TFDConnection` or query component in your Controller or DataModule and share it across handlers.
2. **Local instantiation (Thread-local)**: Always create the connection and query components inside the route handler, and release them in a `try...finally` block (Delphi XE7+ compatible):

```pascal
procedure GetCustomerHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LConnection: TFDConnection;
  LQuery: TFDQuery;
begin
  LConnection := TFDConnection.Create(nil);
  LQuery := TFDQuery.Create(nil);
  try
    // Configure connection locally (preferably utilizing FireDAC Connection Pooling)
    LConnection.ConnectionDefName := 'MyPooledConnectionDef';
    LConnection.Connected := True;
    
    LQuery.Connection := LConnection;
    LQuery.SQL.Text := 'SELECT * FROM CUSTOMERS WHERE ID = :ID';
    LQuery.ParamByName('ID').AsInteger := Req.Params.Field('id').AsInteger;
    LQuery.Open;
    
    Res.Send(LQuery.ToJSONArray);
  finally
    LQuery.Free;
    LConnection.Free;
  end;
end;
```
