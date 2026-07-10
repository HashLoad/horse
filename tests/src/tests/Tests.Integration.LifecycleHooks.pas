unit Tests.Integration.LifecycleHooks;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, RESTRequest4D,
  System.SysUtils, System.Classes, System.Threading, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationLifecycleHooks = class
  private
    const TEST_PORT = 9097;
    class var FOnResponseCalled: Boolean;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestOnRequestAndPreParsingAndPreValidationAndOnSend;
    [Test]
    procedure TestOnRequestAbortingEarly;
    [Test]
    procedure TestOnResponseExecution;
  end;

implementation

{ TTestIntegrationLifecycleHooks }

procedure TTestIntegrationLifecycleHooks.SetupFixture;
begin
  FOnResponseCalled := False;

  // 1. Registro dos Hooks de teste
  THorse.AddOnRequest(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Req.Headers.Dictionary.AddOrSetValue('X-OnRequest-Hook', 'Passed-OnRequest');
      Next;
    end);

  THorse.AddPreParsing(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Req.Headers.Dictionary.AddOrSetValue('X-PreParsing-Hook', 'Passed-PreParsing');
      Next;
    end);

  THorse.AddPreValidation(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Req.Headers.Dictionary.AddOrSetValue('X-PreValidation-Hook', 'Passed-PreValidation');
      Next;
    end);

  THorse.AddOnSend(
    procedure(const Req: THorseRequest; const Res: THorseResponse; var AContent: string)
    begin
      AContent := AContent + '-ModifiedByOnSend';
    end);

  THorse.AddOnResponse(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      FOnResponseCalled := True;
      Next;
    end);

  // 2. Registro do gancho que aborta antecipadamente
  THorse.Get('/abort-early',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('Should not reach here');
    end);

  // Registra um OnRequest exclusivo para a rota de abortar antecipadamente
  THorse.AddOnRequest(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      if Req.PathInfo = '/abort-early' then
      begin
        Res.Send('AbortedEarly').Status(THTTPStatus.Forbidden);
        // Não chama Next
      end
      else
        Next;
    end);

  // 3. Rota de teste padrão
  THorse.Get('/test-lifecycle',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LResultJson: string;
    begin
      LResultJson := Format('{"onrequest":"%s","preparsing":"%s","prevalidation":"%s"}', [
        Req.Headers['X-OnRequest-Hook'],
        Req.Headers['X-PreParsing-Hook'],
        Req.Headers['X-PreValidation-Hook']
      ]);
      Res.Send(LResultJson);
    end);

  // Inicializa o servidor em background
  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(2000);
end;

procedure TTestIntegrationLifecycleHooks.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(500);
end;

procedure TTestIntegrationLifecycleHooks.TestOnRequestAndPreParsingAndPreValidationAndOnSend;
var
  LReq: IRequest;
  LRes: IResponse;
  LExpectedContent: string;
begin
  LReq := TRequest.New;
  LRes := LReq.BaseURL(Format('http://localhost:%d/test-lifecycle', [TEST_PORT]))
    .Accept('application/json')
    .Get;

  Assert.AreEqual(200, LRes.StatusCode, 'Should return HTTP 200');
  
  LExpectedContent := '{"onrequest":"Passed-OnRequest","preparsing":"Passed-PreParsing","prevalidation":"Passed-PreValidation"}-ModifiedByOnSend';
  Assert.AreEqual(LExpectedContent, LRes.Content, 'Should have executed and Modified by OnSend');
end;

procedure TTestIntegrationLifecycleHooks.TestOnRequestAbortingEarly;
var
  LReq: IRequest;
  LRes: IResponse;
begin
  LReq := TRequest.New;
  LRes := LReq.BaseURL(Format('http://localhost:%d/abort-early', [TEST_PORT]))
    .Get;

  Assert.AreEqual(403, LRes.StatusCode, 'Should return HTTP 403 Forbidden due to early abort');
  Assert.AreEqual('AbortedEarly-ModifiedByOnSend', LRes.Content, 'Should return the aborted content (modified by OnSend)');
end;

procedure TTestIntegrationLifecycleHooks.TestOnResponseExecution;
var
  LReq: IRequest;
  LRes: IResponse;
begin
  FOnResponseCalled := False;
  LReq := TRequest.New;
  LRes := LReq.BaseURL(Format('http://localhost:%d/test-lifecycle', [TEST_PORT]))
    .Get;

  Assert.AreEqual(200, LRes.StatusCode);
  
  // Como o onResponse é disparado no final, vamos dar um pequeno sleep para garantir que a thread de socket terminou
  Sleep(200);
  Assert.IsTrue(FOnResponseCalled, 'OnResponse hook should have been called');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationLifecycleHooks);

end.
