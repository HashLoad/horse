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
  Horse,
  Horse.Jhonson,
  Horse.CORS;

begin
  // 1. Register Global Middlewares
  THorse
    .Use(CORS)
    .Use(Jhonson);

  // 2. Register Routes / Controllers
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  // 3. Listen on Port
  THorse.Listen(9000);
end.
```

---

## Modular Architecture
For production grade projects, keep the `.dpr` file clean by delegating route definitions to specialized `Controller` classes:

1. **Keep Controllers stateless**: Do not store request-specific variables as fields in a controller class unless they are thread-safe.
2. **Uses Sequence**: Ensure modules containing business services or repository layers are decoupled from the HTTP transport layer (`Horse` units).
