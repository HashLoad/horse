unit Tests.Horse.Core.Middleware;

interface

uses
  DUnitX.TestFramework, Horse.Core.RouterTree, Horse.Request, Horse.Response,
  System.SysUtils, System.Generics.Collections,
  Horse.Exception.Interrupted,
  {$IF DEFINED(FPC)} HTTPApp {$ELSE} Web.HTTPApp {$ENDIF}, Horse.Commons,
  Horse, Horse.Proc, Horse.Callback;

type
  [TestFixture]
  TTestHorseCoreMiddleware = class
  private
    FRouterTree: THorseRouterTree;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestMiddlewareNormalExecutionChain;
    [Test]
    procedure TestMiddlewareEarlyInterruption;
    [Test]
    procedure TestMiddlewareExceptionPropagation;
    [Test]
    procedure TestMultipleMiddlewaresInRouterTree;
    [Test]
    procedure TestMiddlewareExceptionFreeAbort;
    {$IF DEFINED(FPC)}
    [Test]
    procedure TestFPCLegacyCallbackAssignment;
    {$ENDIF}
  end;

implementation

{ TTestHorseCoreMiddleware }

procedure TTestHorseCoreMiddleware.Setup;
begin
  FRouterTree := THorseRouterTree.Create;
  FRequest := THorseRequest.Create(nil);
  FResponse := THorseResponse.Create(nil);
end;

procedure TTestHorseCoreMiddleware.TearDown;
begin
  FRouterTree.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestHorseCoreMiddleware.TestMiddlewareNormalExecutionChain;
var
  LTrace: TList<string>;
begin
  LTrace := TList<string>.Create;
  try
    // Registramos middleware 1
    FRouterTree.RegisterMiddleware(
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        LTrace.Add('M1_START');
        Next();
        LTrace.Add('M1_END');
      end);

    // Registramos middleware 2
    FRouterTree.RegisterMiddleware(
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        LTrace.Add('M2_START');
        Next();
        LTrace.Add('M2_END');
      end);

    // Registramos a rota final
    FRouterTree.RegisterRoute(mtGet, '/test',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        LTrace.Add('ROUTE');
      end);

    FRequest.Populate('GET', mtGet, '/test', '', '');
    Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));

    // Validamos se a ordem de execução da pilha seguiu o modelo FIFO/LIFO correto
    Assert.AreEqual(5, LTrace.Count);
    Assert.AreEqual('M1_START', LTrace[0]);
    Assert.AreEqual('M2_START', LTrace[1]);
    Assert.AreEqual('ROUTE', LTrace[2]);
    Assert.AreEqual('M2_END', LTrace[3]);
    Assert.AreEqual('M1_END', LTrace[4]);
  finally
    LTrace.Free;
  end;
end;

procedure TTestHorseCoreMiddleware.TestMiddlewareEarlyInterruption;
var
  LTrace: TList<string>;
begin
  LTrace := TList<string>.Create;
  try
    // Middleware 1 interrompe o fluxo lancando EHorseCallbackInterrupted
    FRouterTree.RegisterMiddleware(
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        LTrace.Add('M1_INTERRUPT');
        raise EHorseCallbackInterrupted.Create;
      end);

    // Middleware 2 que nunca deve rodar
    FRouterTree.RegisterMiddleware(
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        LTrace.Add('M2');
        Next();
      end);

    // Rota que nunca deve rodar
    FRouterTree.RegisterRoute(mtGet, '/test',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        LTrace.Add('ROUTE');
      end);

    FRequest.Populate('GET', mtGet, '/test', '', '');

    Assert.WillRaise(
      procedure
      begin
        FRouterTree.Execute(FRequest, FResponse);
      end,
      EHorseCallbackInterrupted);

    // Apenas o Middleware 1 deve ter executado
    Assert.AreEqual(1, LTrace.Count);
    Assert.AreEqual('M1_INTERRUPT', LTrace[0]);
  finally
    LTrace.Free;
  end;
end;

procedure TTestHorseCoreMiddleware.TestMiddlewareExceptionPropagation;
begin
  // Middleware lança exceção
  FRouterTree.RegisterMiddleware(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      raise EOSError.Create('Database connection error');
    end);

  FRouterTree.RegisterRoute(mtGet, '/test',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      // Rota final
    end);

  FRequest.Populate('GET', mtGet, '/test', '', '');

  // A exceção deve propagar para fora do Execute
  Assert.WillRaise(
    procedure
    begin
      FRouterTree.Execute(FRequest, FResponse);
    end,
    EOSError);
end;

procedure TTestHorseCoreMiddleware.TestMultipleMiddlewaresInRouterTree;
var
  I: Integer;
  LTrace: TList<Integer>;
begin
  LTrace := TList<Integer>.Create;
  try
    for I := 1 to 15 do
    begin
      FRouterTree.RegisterMiddleware(
        procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
        begin
          LTrace.Add(1);
          Next();
          LTrace.Add(-1);
        end);
    end;

    FRouterTree.RegisterRoute(mtGet, '/test',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        LTrace.Add(100);
      end);

    FRequest.Populate('GET', mtGet, '/test', '', '');
    Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));

    Assert.AreEqual(31, LTrace.Count);
    for I := 0 to 14 do
      Assert.AreEqual(1, LTrace[I]);
    
    Assert.AreEqual(100, LTrace[15]);

    for I := 16 to 30 do
      Assert.AreEqual(-1, LTrace[I]);
  finally
    LTrace.Free;
  end;
end;

{$IF DEFINED(FPC)}
procedure MyLegacyCallback(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  //
end;

function GetLegacyCallback: THorseCallback;
begin
  Result := MyLegacyCallback;
end;

procedure TTestHorseCoreMiddleware.TestFPCLegacyCallbackAssignment;
var
  LCallback: THorseCallback;
begin
  LCallback := GetLegacyCallback;
  Assert.IsNotNull(Pointer(LCallback));
end;
{$ENDIF}

procedure TTestHorseCoreMiddleware.TestMiddlewareExceptionFreeAbort;
var
  LTrace: TList<string>;
begin
  LTrace := TList<string>.Create;
  try
    FRouterTree.RegisterMiddleware(
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        LTrace.Add('M1_START');
        Res.Status(THTTPStatus.BadRequest).Abort;
        LTrace.Add('M1_ABORTED');
      end);

    FRouterTree.RegisterMiddleware(
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        LTrace.Add('M2');
        Next();
      end);

    FRouterTree.RegisterRoute(mtGet, '/test',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
      begin
        LTrace.Add('ROUTE');
      end);

    FRequest.Populate('GET', mtGet, '/test', '', '');
    Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));

    Assert.AreEqual(2, LTrace.Count);
    Assert.AreEqual('M1_START', LTrace[0]);
    Assert.AreEqual('M1_ABORTED', LTrace[1]);
    Assert.IsTrue(FResponse.Aborted);
    Assert.AreEqual(Integer(THTTPStatus.BadRequest), FResponse.Status);
  finally
    LTrace.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseCoreMiddleware);

end.
