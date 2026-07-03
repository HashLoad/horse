# Horse Integrity and Resilience Testing

Horse includes an automated suite of integrity tests designed to validate the framework's behavior in real network scenarios, as well as analyze its resilience under fatal system failures and critical memory overflows.

These tests reside in the folder: `samples/delphi/console_complete/`

---

## Why are Integrity Tests Useful?

Unlike traditional unit tests (which test individual classes and methods in isolation), integrity tests simulate a real HTTP client consuming an active Horse API over the network.

They are extremely useful for:
- **Preventing Regressions**: Guaranteeing that changes in the HTTP parser, router, middleware, or memory buffers do not break the expected behavior of real network calls.
- **Validating Verbs and Headers**: Verifying correct parsing of query strings, route parameters, case-insensitive headers, and the custom `QUERY` HTTP verb.
- **Testing real uploads**: Validating actual file transmissions sent as `multipart/form-data`.
- **Resilience Analysis (Robustness)**: Assessing how the Horse server handles fatal errors inside the application logic (such as null pointers) without crashing the service.

---

## What is Tested?

The integrity test script ([test_integrity.ps1](file:///d:/Delphi/horse/samples/delphi/console_complete/test_integrity.ps1)) runs two consecutive validation passes: one compiling the sample server ([ConsoleComplete.dpr](file:///d:/Delphi/horse/samples/delphi/console_complete/ConsoleComplete.dpr)) with the **Default Router** (`RouterTree`), and another with the **Radix Router** (`RadixRouter`). Each pass performs the following real HTTP requests using `Invoke-RestMethod` and `curl`:

1. **GET /ping**: Basic request flow and quick ping-pong verification.
2. **GET /resource/:id**: Validates URL parameter extraction, query strings, and case-insensitive authorization headers.
3. **POST /resource**: Transmission of raw payload inside the request body.
4. **PUT, PATCH, DELETE /resource/:id**: RESTful verbs for resource modification and deletion.
5. **QUERY /search**: Custom `QUERY` verb sending search payloads.
6. **POST /upload**: Actual multipart file upload.
7. **GET /error-trigger**: Clean, formatted exceptions raised using `EHorseException`.
8. **GET /clientes, GET /pessoas, GET /qualquercoisa**: Route matching priority, checking that exact paths are resolved before fallback wildcard (`*`) matching.

---

## Resilience and Limitations (Fatal System Failures)

The integrity suite includes specific endpoints to trigger catastrophic crashes and analyze the Horse server's resilience:

### 1. Intercepting Access Violations (AV)
Accessing `/av-trigger` intentionally performs a write operation on a null pointer reference:
```delphi
var
  LPointer: PInteger;
begin
  LPointer := nil;
  LPointer^ := 42; // Access Violation!
end;
```
* **Horse Behavior**: Horse successfully intercepts the violation on the request handler thread level. It blocks the main executable from crashing, returns an HTTP `500 Internal Server Error` message, and keeps the server active to continue handling other requests.

### 2. Stack Overflow
Accessing `/stack-trigger` triggers an infinite recursion that consumes the thread's memory stack allocation limit:
```delphi
var
  LRecurse: TProc;
begin
  LRecurse := procedure
    begin
      LRecurse();
    end;
  LRecurse(); // Stack Overflow!
end;
```
* **SO Limitations**: A Stack Overflow represents a hardware/OS-level resource limit. When the thread memory stack size limit is exceeded, the OS intervenes and terminates the server process immediately for stability. The integrity script correctly logs and handles this crash scenario.

---

## Running Locally

To run the integrity tests locally in a Windows environment using PowerShell:

```powershell
cd samples/delphi/console_complete
.\test_integrity.ps1
```

The script manages the compilation of both routers using the Delphi compiler (`dcc32`), launches the background process for each scenario, executes the validation requests, terminates the servers, and prints a final combined success/failure summary.
