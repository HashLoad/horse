---
name: horse-writing-middleware
description: Guide to writing custom middlewares for the Horse framework, following the callback flow with the Next parameter.
---

# Writing Custom Horse Middlewares

## Middleware Signature
A custom middleware is a procedure that receives `THorseRequest`, `THorseResponse`, and a callback procedure `Next`.

```pascal
procedure MyCustomMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  // 1. Logic BEFORE the request reaches the endpoint handler
  Req.Headers.AddOrSetValue('X-Processed-By', 'MyMiddleware');
  
  // 2. Call Next to delegate execution to the subsequent middleware or route handler
  Next;
  
  // 3. Logic AFTER the route handler completes its execution
  Res.Headers.AddOrSetValue('X-Execution-Finished', 'True');
end;
```

---

## Terminating Execution Early
If a validation fails (e.g., authentication is missing), do **NOT** call `Next`. Set the status and send the response immediately. This halts the pipeline execution.

```pascal
procedure AuthMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LToken: string;
begin
  LToken := Req.Headers.Items['Authorization'];
  
  if LToken.IsEmpty then
  begin
    // Halt execution by sending response and omitting the Next call
    Res.Send('Unauthorized').Status(THTTPStatus.Unauthorized);
    Exit;
  end;
  
  // Token is valid, proceed with pipeline
  Next;
end;
```
