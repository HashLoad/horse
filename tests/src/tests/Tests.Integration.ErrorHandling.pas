unit Tests.Integration.ErrorHandling;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, Horse.Exception, RESTRequest4D,
  System.SysUtils, System.Classes, System.Threading, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationErrorHandling = class
  private
    const TEST_PORT = 9096;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestGenericExceptionReturnsHTTP500;
    [Test]
    procedure TestCustomHorseExceptionReturnsCorrectStatus;
  end;

implementation

{ TTestIntegrationErrorHandling }

procedure TTestIntegrationErrorHandling.SetupFixture;
begin
  THorse.Get('/error/generic',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      raise Exception.Create('Something went wrong internally');
    end);

  THorse.Get('/error/custom',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      raise EHorseException.Create.Status(THTTPStatus.BadRequest).Error('{"error":"Invalid request parameters"}');
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(500);
end;

procedure TTestIntegrationErrorHandling.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(100);
end;

procedure TTestIntegrationErrorHandling.TestGenericExceptionReturnsHTTP500;
var
  LReq: IRequest;
  LRes: IResponse;
begin
  LReq := TRequest.New;
  LRes := LReq.BaseURL(Format('http://localhost:%d/error/generic', [TEST_PORT]))
    .Accept('text/plain')
    .Get;

  Assert.AreEqual(500, LRes.StatusCode, 'Generic exception should result in HTTP 500');
  Assert.AreEqual('Internal Application Error', LRes.Content, 'Body should contain default error message');
end;

procedure TTestIntegrationErrorHandling.TestCustomHorseExceptionReturnsCorrectStatus;
var
  LReq: IRequest;
  LRes: IResponse;
begin
  LReq := TRequest.New;
  LRes := LReq.BaseURL(Format('http://localhost:%d/error/custom', [TEST_PORT]))
    .Accept('application/json')
    .Get;

  Assert.AreEqual(400, LRes.StatusCode, 'EHorseException should return its custom status');
  Assert.AreEqual('{"error":"Invalid request parameters"}', LRes.Content, 'Body should contain custom exception content');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationErrorHandling);

end.
