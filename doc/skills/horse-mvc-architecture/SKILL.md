---
name: horse-mvc-architecture
description: Guide for structuring corporate Horse applications using Clean MVC (Model-View-Controller) principles and decoupling HTTP layers from business logic.
---

# Horse MVC Architecture

For large-scale, production-ready REST APIs, avoid placing business logic inside route handlers or keeping everything in the `.dpr` bootstrap file. Implement a clean, decoupled MVC (Model-View-Controller) architecture.

---

## 1. Project Directory Structure
Keep your project organized by dividing concerns into dedicated directories under the `src/` folder:

```text
my_project/
├── my_project.dpr
└── src/
    ├── controllers/   # Mappings of HTTP endpoints to service invocations
    ├── services/      # Core business rules, validations, and orchestration
    ├── repositories/  # Database access layer (SQL execution, FireDAC queries)
    └── models/        # Entity classes and data structures (Domain)
```

---

## 2. Decoupling Rules (The Golden Rule)
To ensure testability and maintenance, never couple your business logic to the web framework.

*   **Rule**: The `Service`, `Repository`, and `Model` layers **must never** import Horse units or reference Horse objects (like `THorseRequest` or `THorseResponse`).
*   **Reason**: If you decide to migrate from HTTP to a CLI application or gRPC, your business services and repositories remain 100% untouched.

---

## 3. Implementation Blueprint

### A. The Repository Layer (Database Access)
Retrieves and updates raw database data. It knows nothing about HTTP.

```pascal
unit Repository.Customer;

interface

uses
  System.JSON, FireDAC.Comp.Client;

type
  TCustomerRepository = class
  public
    class function FindById(const AId: Integer): TJSONObject;
  end;

implementation

class function TCustomerRepository.FindById(const AId: Integer): TJSONObject;
var
  LConnection: TFDConnection;
  LQuery: TFDQuery;
begin
  Result := nil;
  LConnection := TFDConnection.Create(nil);
  LQuery := TFDQuery.Create(nil);
  try
    LConnection.ConnectionDefName := 'MyPooledDef';
    LConnection.Connected := True;
    
    LQuery.Connection := LConnection;
    LQuery.SQL.Text := 'SELECT id, name, email FROM customers WHERE id = :id';
    LQuery.ParamByName('id').AsInteger := AId;
    LQuery.Open;
    
    if not LQuery.IsEmpty then
      Result := LQuery.ToJSONObject; // Returns raw JSON entity
  finally
    LQuery.Free;
    LConnection.Free;
  end;
end;

end.
```

### B. The Service Layer (Business Logic)
Implements business validation and orchestrates repositories.

```pascal
unit Service.Customer;

interface

uses
  System.JSON;

type
  TCustomerService = class
  public
    class function GetCustomer(const AId: Integer): TJSONObject;
  end;

implementation

uses
  Repository.Customer, System.SysUtils;

class function TCustomerService.GetCustomer(const AId: Integer): TJSONObject;
begin
  if AId <= 0 then
    raise Exception.Create('Invalid Customer ID');
    
  Result := TCustomerRepository.FindById(AId);
  
  if not Assigned(Result) then
    raise Exception.Create('Customer not found');
end;

end.
```

### C. The Controller Layer (HTTP Transport Adapter)
The ONLY layer that depends on Horse. It parses incoming requests, calls services, and formats HTTP responses.

```pascal
unit Controller.Customer;

interface

uses
  Horse;

type
  TCustomerController = class
  private
    class procedure GetCustomerHandler(Req: THorseRequest; Res: THorseResponse);
  public
    class procedure RegisterRoutes;
  end;

implementation

uses
  Service.Customer, System.JSON, System.SysUtils, Horse.Commons;

class procedure TCustomerController.RegisterRoutes;
begin
  THorse.Get('/customers/:id', GetCustomerHandler);
end;

class procedure TCustomerController.GetCustomerHandler(Req: THorseRequest; Res: THorseResponse);
var
  LId: Integer;
  LCustomer: TJSONObject;
begin
  try
    LId := Req.Params.Field('id').AsInteger;
    
    // Invocate Business Service (No Horse dependencies passed!)
    LCustomer := TCustomerService.GetCustomer(LId);
    
    Res.Send(LCustomer);
  except
    on E: Exception do
      Res.Status(THTTPStatus.BadRequest).Send(TJSONObject.Create(TJSONPair.Create('error', E.Message)));
  end;
end;

end.
unit
```

### D. The Bootstrap (.dpr)
Now, your main bootstrap file only handles middleware setup and controller registration, keeping it extremely clean:

```pascal
program MyAPI;

{$APPTYPE CONSOLE}

uses
  Horse,
  Horse.Jhonson,
  Controller.Customer;

begin
  THorse.Use(Jhonson);

  // Register all Controller routes
  TCustomerController.RegisterRoutes;

  THorse.Listen(9000);
end.
```
