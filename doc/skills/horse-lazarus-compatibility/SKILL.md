---
name: horse-lazarus-compatibility
description: Guide for ensuring Lazarus and Free Pascal (FPC) compatibility, addressing anonymous methods differences, JSON units, and compiler directives.
---

# Horse Lazarus & FPC Compatibility

To write cross-platform Horse applications that compile flawlessly in both Delphi and Lazarus/FPC, you must adhere to Free Pascal's compiler rules and syntax limitations.

---

## 1. No Anonymous Methods in FPC (The Core Rule)
While Delphi supports registering routes using inline anonymous procedures (methods) directly, **Free Pascal (FPC) does not support anonymous methods with variable capturing** in the same syntax.

### Delphi Syntax (Will Fail to Compile on Lazarus)
```pascal
// Do NOT use this inline syntax if targeting Lazarus/FPC
THorse.Get('/ping',
  procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
  begin
    Res.Send('pong');
  end);
```

### Lazarus/FPC Correct Syntax
Always declare handlers as standard standalone procedures or method pointers, and pass them as callback parameters:

```pascal
{$MODE DELPHI}{$H+} // CRITICAL compiler directive for Lazarus

uses
  Horse;

// 1. Declare handler as a standard procedure
procedure GetPingHandler(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  // 2. Register the procedure callback
  THorse.Get('/ping', GetPingHandler);
  THorse.Listen(9000);
end.
```

---

## 2. Standard Compiler Directives for Lazarus
Always include these two compiler directives at the absolute top of your Lazarus `.pas` or `.dpr` (program) files:

```pascal
{$MODE DELPHI}{$H+}
```
*   `{$MODE DELPHI}`: Instructs FPC to emulate Delphi's object-oriented syntax and generic type resolutions.
*   `{$H+}`: Enforces the use of Long Strings (`AnsiString`) by default instead of short strings (which are limited to 255 characters).

---

## 3. JSON Framework Differences
Delphi and Lazarus use entirely different built-in JSON serialization libraries. 

*   **Delphi**: Uses `System.JSON` (`TJSONObject`, `TJSONArray`).
*   **Lazarus/FPC**: Uses `fpjson` and `jsonparser` (`TJSONObject`, `TJSONArray`).

### Writing Cross-Compiler JSON Code
If your middleware or controllers need to compile on both platforms, use conditional compilation defines (`$IFDEF`):

```pascal
uses
  {$IFDEF FPC}
  fpjson, jsonparser,
  {$ELSE}
  System.JSON,
  {$ENDIF}
  Horse;

procedure GetStatusHandler(Req: THorseRequest; Res: THorseResponse);
var
  LJson: TJSONObject;
begin
  LJson := TJSONObject.Create;
  try
    {$IFDEF FPC}
    LJson.Add('status', 'healthy');
    {$ELSE}
    LJson.AddPair('status', 'healthy');
    {$ENDIF}
    
    Res.Send(LJson);
  finally
    // Johnson middleware takes ownership and frees this object. 
    // Refer to horse-middlewares skill.
  end;
end;
```

---

## 4. Best Practices for Cross-Compiler Apps
1.  **Prefer standalone controller procedures**: Keep all route handlers in a separate unit as standard procedures, avoiding inline code in the bootstrap project file.
2.  **Avoid Delphi-specific RTL**: Do not use units like `System.NetEncoding` or `System.Hash` without providing an alternative like `LclIntf` or FPC's built-in cryptographic units when compiling under FPC.
