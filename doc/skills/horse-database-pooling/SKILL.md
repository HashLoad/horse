---
name: horse-database-pooling
description: Guide for setting up thread-safe database connection pooling (FireDAC / UniDAC) in multithreaded Horse applications.
---

# Horse Database Pooling

## 1. The Multithreaded Database Rule
Horse handles each incoming HTTP request in a separate thread. Therefore, sharing a single database connection components (like a global `TFDConnection` or query component placed on a global DataModule/Controller) across requests **will cause race conditions, data corruption, and Access Violations**.

*   **Rule**: Every route handler thread must instantiate its own database connection (or request one from a thread-safe connection pool) and free it immediately after use.

---

## 2. Setting Up FireDAC Connection Pooling
FireDAC provides built-in, highly optimized connection pooling. To configure and use it:

### Phase A: Defining the Connection Definition (Global Setup)
Configure the connection definition globally in your application bootstrap (`program` block or startup class) using `TFDManager`:

```pascal
uses
  FireDAC.Stan.Intf, FireDAC.Phys.PG, FireDAC.Comp.Client;

procedure SetupConnectionPool;
var
  LParams: TStrings;
begin
  LParams := TStringList.Create;
  try
    LParams.Values['DriverID'] := 'PG'; // PostgreSQL example
    LParams.Values['Server'] := 'localhost';
    LParams.Values['Database'] := 'my_db';
    LParams.Values['User_Name'] := 'postgres';
    LParams.Values['Password'] := 'secret';
    LParams.Values['Pooled'] := 'True'; // CRITICAL: Enables pooling
    LParams.Values['POOL_MaximumItems'] := '50'; // Set maximum connections in pool
    
    // Register the definition with FDManager
    FDManager.AddConnectionDef('MyPooledPGDef', 'PG', LParams);
  finally
    LParams.Free;
  end;
end;
```

### Phase B: Acquiring and Releasing Connections in Handlers
In your route handlers, instantiate the `TFDConnection` locally, referencing the pooled definition name. Always protect resources with `try...finally`:

```pascal
procedure GetUsersHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LConnection: TFDConnection;
  LQuery: TFDQuery;
begin
  LConnection := TFDConnection.Create(nil);
  LQuery := TFDQuery.Create(nil);
  try
    // Reference the pooled definition - this will instantly pull a connection from the pool
    LConnection.ConnectionDefName := 'MyPooledPGDef';
    LConnection.Connected := True;
    
    LQuery.Connection := LConnection;
    LQuery.SQL.Text := 'SELECT id, name FROM users';
    LQuery.Open;
    
    // Johnson middleware takes ownership of the JSON object/array
    Res.Send(LQuery.ToJSONArray);
  finally
    LQuery.Free;
    LConnection.Free; // Automatically releases connection back to the pool
  end;
end;
```

---

## 3. Setting Up UniDAC Connection Pooling
If using Devart UniDAC, enable pooling via the connection string parameter or using `TUniConnection.SpecificOptions`:

```pascal
procedure GetUsersUniDACHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LConnection: TUniConnection;
  LQuery: TUniQuery;
begin
  LConnection := TUniConnection.Create(nil);
  LQuery := TUniQuery.Create(nil);
  try
    LConnection.ProviderName := 'PostgreSQL';
    LConnection.Server := 'localhost';
    LConnection.Database := 'my_db';
    LConnection.Username := 'postgres';
    LConnection.Password := 'secret';
    
    // Enable Pooling in UniDAC
    LConnection.Pooling := True;
    LConnection.PoolingOptions.MaxPoolSize := 50;
    
    LConnection.Connect;
    
    LQuery.Connection := LConnection;
    LQuery.SQL.Text := 'SELECT id, name FROM users';
    LQuery.Open;
    
    Res.Send(LQuery.ToJSONArray);
  finally
    LQuery.Free;
    LConnection.Free;
  end;
end;

---

## 3.5. Automated Connection Management via Request Services (IoC)
With Horse's native Request Scope IoC container, you can register a lazy factory for database connections. This ensures the connection is only opened if/when a route resolves it, and guarantees it is freed automatically when the request ends.

```pascal
// Setup connection factory inside a global middleware
THorse.Use(procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
  begin
    Req.Services.AddFactory(TFDConnection,
      function: TObject
      begin
        Result := TFDConnection.Create(nil);
        TFDConnection(Result).ConnectionDefName := 'MyPooledPGDef';
        TFDConnection(Result).Connected := True;
      end);
    Next();
  end);

// Usage in Route Handler (No manual connection free required!)
THorse.Get('/users', procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
  var
    LConnection: TFDConnection;
    LQuery: TFDQuery;
  begin
    LConnection := TFDConnection(Req.Services.Resolve(TFDConnection));
    LQuery := TFDQuery.Create(nil);
    try
      LQuery.Connection := LConnection;
      LQuery.SQL.Text := 'SELECT id, name FROM users';
      LQuery.Open;
      Res.Send(LQuery.ToJSONArray);
    finally
      LQuery.Free;
    end; // LConnection is freed automatically by Horse when the request finishes!
  end);
```

---

## 4. Best Practices for Database Handlers
1.  **Keep Queries Short**: Hold database connections open for the absolute shortest time possible. Perform non-database calculations before opening or after closing the connection.
2.  **Always use `try...finally`**: Ensure connection/query components are freed even if an exception occurs during SQL execution.
3.  **Read-Only Operations**: Set `ReadOnly := True` on your queries where possible to improve pooling performance.
