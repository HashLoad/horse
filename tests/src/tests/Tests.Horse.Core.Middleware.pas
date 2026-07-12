unit Tests.Horse.Core.Middleware;

interface

{$X+}

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
  LCallback: THorseCallback;
begin
  LTrace := TList<string>.Create;
  try
    // Registramos middleware 1
    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add('M1_START');
        Next();
        LTrace.Add('M1_END');
      end;
    FRouterTree.RegisterMiddleware(LCallback);

    // Registramos middleware 2
    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add('M2_START');
        Next();
        LTrace.Add('M2_END');
      end;
    FRouterTree.RegisterMiddleware(LCallback);

    // Registramos a rota final
    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add('ROUTE');
      end;
    FRouterTree.RegisterRoute(mtGet, '/test', LCallback);

    FRequest.Populate('GET', mtGet, '/test', '', '');
    Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));

    // Validamos se a ordem de execução da pilha seguiu o modelo FIFO/LIFO correto
    Assert.IsTrue(LTrace.Count = 5);
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
  LCallback: THorseCallback;
  LMethod: TProc;
begin
  LTrace := TList<string>.Create;
  try
    // Middleware 1 interrompe o fluxo lancando EHorseCallbackInterrupted
    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add('M1_INTERRUPT');
        raise EHorseCallbackInterrupted.Create;
      end;
    FRouterTree.RegisterMiddleware(LCallback);

    // Middleware 2 que nunca deve rodar
    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add('M2');
        Next();
      end;
    FRouterTree.RegisterMiddleware(LCallback);

    // Rota que nunca deve rodar
    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add('ROUTE');
      end;
    FRouterTree.RegisterRoute(mtGet, '/test', LCallback);

    FRequest.Populate('GET', mtGet, '/test', '', '');

    LMethod := procedure
      var
        LRet: Boolean;
      begin
        LRet := FRouterTree.Execute(FRequest, FResponse);
      end;
    Assert.WillRaise(LMethod, EHorseCallbackInterrupted);

    // Apenas o Middleware 1 deve ter executado
    Assert.IsTrue(LTrace.Count = 1);
    Assert.AreEqual('M1_INTERRUPT', LTrace[0]);
  finally
    LTrace.Free;
  end;
end;

procedure TTestHorseCoreMiddleware.TestMiddlewareExceptionPropagation;
var
  LCallback: THorseCallback;
  LMethod: TProc;
begin
  // Middleware lança exceção
  LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      raise EOSError.Create('Database connection error');
    end;
  FRouterTree.RegisterMiddleware(LCallback);

  LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      // Rota final
    end;
  FRouterTree.RegisterRoute(mtGet, '/test', LCallback);

  FRequest.Populate('GET', mtGet, '/test', '', '');

  // A exceção deve propagar para fora do Execute
  LMethod := procedure
    var
      LRet: Boolean;
    begin
      LRet := FRouterTree.Execute(FRequest, FResponse);
    end;
  Assert.WillRaise(LMethod, EOSError);
end;

procedure TTestHorseCoreMiddleware.TestMultipleMiddlewaresInRouterTree;
var
  I: Integer;
  LTrace: TList<Integer>;
  LCallback: THorseCallback;
begin
  LTrace := TList<Integer>.Create;
  try
    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add(1);
        Next();
        LTrace.Add(-1);
      end;
    for I := 1 to 15 do
    begin
      FRouterTree.RegisterMiddleware(LCallback);
    end;

    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add(100);
      end;
    FRouterTree.RegisterRoute(mtGet, '/test', LCallback);

    FRequest.Populate('GET', mtGet, '/test', '', '');
    Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));

    Assert.IsTrue(LTrace.Count = 31);
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
  LCallback: THorseCallback;
begin
  LTrace := TList<string>.Create;
  try
    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add('M1_START');
        Res.Status(THTTPStatus.BadRequest).Abort;
        LTrace.Add('M1_ABORTED');
      end;
    FRouterTree.RegisterMiddleware(LCallback);

    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add('M2');
        Next();
      end;
    FRouterTree.RegisterMiddleware(LCallback);

    LCallback := procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LTrace.Add('ROUTE');
      end;
    FRouterTree.RegisterRoute(mtGet, '/test', LCallback);

    FRequest.Populate('GET', mtGet, '/test', '', '');
    Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));

    Assert.IsTrue(LTrace.Count = 2);
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
