---
name: horse-integration-tests
description: Guide for writing automated integration tests for Horse endpoints using DUnit/DUnitX and THTTPClient.
---

# Horse Integration Testing

Automated integration tests ensure your API routes, request parsing, and error-handling pipelines behave correctly under simulated HTTP traffic.

---

## 1. The Dynamic Port Pattern
When running tests locally or in CI/CD pipelines (like GitHub Actions), static ports (e.g., `9000`) might be occupied. To avoid port conflicts:

1.  **Generate a dynamic port** for each test run.
2.  **Start Horse** on that port inside the setup method.
3.  **Perform HTTP requests** against `http://localhost:<dynamic_port>`.
4.  **Terminate Horse** inside the teardown method.

---

## 2. Integration Test Example (DUnitX)
Here is a complete integration test suite demonstrating how to bootstrap Horse, perform requests with Delphi's native `THTTPClient`, and assert responses:

```pascal
unit Test.UserAPI;

interface

uses
  DUnitX.TestFramework,
  System.Net.HttpClient,
  System.SysUtils;

type
  [TestFixture]
  TTestUserAPI = class
  private
    FPort: Integer;
    FClient: THTTPClient;
    function GetBaseURL: string;
  public
    [SetupFixture]
    procedure SetupFixture; // Start Horse Server
    [TearDownFixture]
    procedure TearDownFixture; // Terminate Horse Server
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    
    [Test]
    procedure TestGetPingReturnsPong;
    [Test]
    procedure TestGetInvalidUserReturns404;
  end;

implementation

uses
  Horse,
  System.JSON,
  System.Net.URLClient;

function TTestUserAPI.GetBaseURL: string;
begin
  Result := 'http://localhost:' + FPort.ToString;
end;

procedure TTestUserAPI.SetupFixture;
begin
  // Choose a random dynamic port
  Randomize;
  FPort := 10000 + Random(50000);
  
  // Register endpoints to test
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  THorse.Get('/users/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LId: Integer;
    begin
      LId := Req.Params.Field('id').AsInteger;
      if LId = 999 then
        Res.Status(THTTPStatus.NotFound).Send('User not found')
      else
        Res.Send('User found');
    end);

  // Start Horse asynchronously (non-blocking)
  THorse.Listen(FPort);
end;

procedure TTestUserAPI.TearDownFixture;
begin
  // Terminate the process / stop Horse provider
  THorse.Terminate;
end;

procedure TTestUserAPI.Setup;
begin
  FClient := THTTPClient.Create;
end;

procedure TTestUserAPI.TearDown;
begin
  FClient.Free;
end;

procedure TTestUserAPI.TestGetPingReturnsPong;
var
  LResponse: IHTTPResponse;
begin
  LResponse := FClient.Get(GetBaseURL + '/ping');
  
  Assert.AreEqual(200, LResponse.StatusCode);
  Assert.AreEqual('pong', LResponse.ContentAsString);
end;

procedure TTestUserAPI.TestGetInvalidUserReturns404;
var
  LResponse: IHTTPResponse;
begin
  LResponse := FClient.Get(GetBaseURL + '/users/999');
  
  Assert.AreEqual(404, LResponse.StatusCode);
  Assert.AreEqual('User not found', LResponse.ContentAsString);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestUserAPI);
end.
```

---

## 3. Best Practices for Integration Testing
1.  **Isolate Database State**: Before running tests, mock database tables or use transactional rollbacks to ensure tests start with clean, predictable data.
2.  **Separate Test Logic**: Keep your route handlers in dedicated `Controller` units. Do not define routes inline in your test project setup unless you are testing general configuration middleware behavior.
3.  **Always call `THorse.Terminate`**: Failing to call `Terminate` in your tear down fixture will leave the TCP socket open, preventing subsequent test executions from binding to the same port.
