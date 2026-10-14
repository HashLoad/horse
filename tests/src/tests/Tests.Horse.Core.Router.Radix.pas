unit Tests.Horse.Core.Router.Radix;

interface

uses
  DUnitX.TestFramework, Horse.Core.Router.Radix, Horse.Request, Horse.Response,
  System.SysUtils, System.Generics.Collections, Horse, Horse.Exception, Horse.Exception.Interrupted, Horse.Provider.RawAdapters, Horse.Callback, Horse.Proc,
  {$IF DEFINED(FPC)} HTTPApp {$ELSE} Web.HTTPApp {$ENDIF}, Horse.Commons;

type
  [TestFixture]
  TTestHorseCoreRouterRadix = class
  private
    FRouter: THorseRadixRouter;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
  public
    [TestCase('GenericMiddleware', '0,1,0')]
    [TestCase('HorseMiddleware', '1,1,0')]
    [TestCase('GenericFallback', '0,0,0')]
    [TestCase('HorseFallback', '1,0,0')]
    [TestCase('GenericOnError', '0,2,0')]
    [TestCase('HorseBeforeOnError', '1,2,0')]
    [TestCase('GenericRethrow', '0,3,0')]
    [TestCase('HorseRethrow', '1,3,0')]
    [TestCase('GenericSecondCallback', '0,1,1')]
    [TestCase('HorseSecondCallback', '1,1,1')]
    [TestCase('GenericInnerMiddleware', '0,1,2')]
    [TestCase('HorseInnerMiddleware', '1,1,2')]
    [TestCase('MiddlewareBeforeOnError', '0,4,0')]
    [TestCase('HorseMiddlewareBeforeOnError', '1,4,0')]
    [TestCase('PreserveErrorResponse', '0,5,0')]
    procedure ExceptionPropagation(const AKind, AMode, ASource: Integer);
    [TestCase('WithoutOnError', '0')]
    [TestCase('WithOnError', '1')]
    procedure InterruptedIsNotAnError(const AHook: Integer);
    [Test]
    procedure FallbackFlushesCookies;
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ExecuteRouteGroupWithParams;
    [Test]
    procedure ExecuteRouteGroupWithSubRoute;
    [Test]
    procedure ExecuteRouteGroupWithTwoSubRoute;
    [Test]
    procedure ExecuteRouteGroupWithThreeSubRoute;
    [Test]
    procedure ExecuteRouteGroupWithTwoParams;
    [Test]
    procedure ExecuteRouteGroupWithSubRouteAndParams;
    [Test]
    procedure ExecuteRouteGroupWithSubRouteAndTwoParams;
    [Test]
    procedure ExecuteRouteGroupWithParamsAndSubRoute;
    [Test]
    procedure ExecuteRouteGroupWithParamsAndSubRouteAndParams;
    [Test]
    procedure ExecuteRouteWithTwoConsecutiveParams;
    [Test]
    procedure ExecuteRouteWithMiddlewares;
    [Test]
    procedure ExecuteRouteWithCoringaoPriority;
    [Test]
    procedure ExecuteRouteWithMethodNotAllowedAllowHeader;
    [Test]
    procedure ExecuteRouteWithPrefix;
{$IF SizeOf(Char) > 1}
    [Test]
    procedure ExecuteRouteWithUtf8LiteralAndParam;
{$ENDIF}
  end;

implementation

var
  LOnError: Boolean;

procedure PropagationOnError(const Req: THorseRequest; const Res: THorseResponse; const E: Exception);
begin
  LOnError := True;
  Res.Send('hook').Status(THTTPStatus.ServiceUnavailable);
end;

procedure TTestHorseCoreRouterRadix.ExceptionPropagation(const AKind, AMode, ASource: Integer);
var
  LCaught: Boolean;
  LThrow: THorseCallback;
begin
  LCaught := False;
  LOnError := False;
  THorse.OnError(nil);
  try
    if (AMode = 2) or (AMode = 4) then
      THorse.OnError(PropagationOnError);
    if (AMode = 1) or (AMode = 3) or (AMode = 4) then
      FRouter.RegisterMiddleware(
        procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
        begin
          try
            Next;
          except
            on E: Exception do
            begin
              LCaught := True;
              if AMode = 3 then
                raise;
              Res.ContentType('application/json').Send('{"handled":true}').Status(THTTPStatus.UnprocessableEntity);
            end;
          end;
        end);
    LThrow :=
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        if AMode = 5 then
          Res.Send('existing error').Status(THTTPStatus.Conflict);
        if AKind = 1 then
          raise EHorseException.New.Error('route error').Status(THTTPStatus.BadRequest);
        raise Exception.Create('route error');
      end;
    if ASource = 1 then
      FRouter.RegisterRouteMiddleware(mtGet, '/error',
        procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
        begin
          Next;
        end);
    if ASource = 2 then
      FRouter.RegisterMiddleware(LThrow);
    FRouter.RegisterRoute(mtGet, '/error', LThrow);
    FRequest.Populate('GET', mtGet, '/error', '', '');
    Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
    Assert.AreEqual((AMode = 1) or (AMode = 3) or (AMode = 4), LCaught, 'Middleware must see the exception');
    Assert.AreEqual((AMode = 2) and (AKind = 0), LOnError, 'OnError fallback precedence');
    if (AMode = 1) or (AMode = 4) then
    begin
      Assert.AreEqual(422, FResponse.Status);
      Assert.AreEqual('{"handled":true}', FResponse.BodyText);
      Assert.AreEqual('application/json', FResponse.CSContentType);
    end
    else if AMode = 5 then
    begin
      Assert.AreEqual(409, FResponse.Status);
      Assert.AreEqual('existing error', FResponse.BodyText);
    end
    else if AKind = 1 then
    begin
      Assert.AreEqual(400, FResponse.Status);
      Assert.AreEqual('{"error":"route error"}', FResponse.BodyText);
      Assert.AreEqual('application/json; charset=UTF-8', FResponse.CSContentType);
    end
    else if AMode = 2 then
    begin
      Assert.AreEqual(503, FResponse.Status);
      Assert.AreEqual('hook', FResponse.BodyText);
    end
    else
    begin
      Assert.AreEqual(500, FResponse.Status);
      Assert.AreEqual('Internal Application Error: route error', FResponse.BodyText);
    end;
  finally
    THorse.OnError(nil);
  end;
end;


procedure TTestHorseCoreRouterRadix.InterruptedIsNotAnError(const AHook: Integer);
var
  LInterrupted, LContinued: Boolean;
begin
  LOnError := False;
  LInterrupted := False;
  LContinued := False;
  THorse.OnError(nil);
  try
    if AHook = 1 then
      THorse.OnError(PropagationOnError);
    FRouter.RegisterRouteMiddleware(mtGet, '/interrupt',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        Res.Send('stopped').Status(THTTPStatus.Accepted);
        raise EHorseCallbackInterrupted.Create;
      end);
    FRouter.RegisterRoute(mtGet, '/interrupt',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        LContinued := True;
      end);
    FRequest.Populate('GET', mtGet, '/interrupt', '', '');
    try
      Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
    except
      on EHorseCallbackInterrupted do
        LInterrupted := True;
    end;
    Assert.AreEqual(AHook = 0, LInterrupted);
    Assert.IsFalse(LOnError);
    Assert.IsFalse(LContinued);
    Assert.AreEqual(202, FResponse.Status);
    Assert.AreEqual('stopped', FResponse.BodyText);
  finally
    THorse.OnError(nil);
  end;
end;

procedure TTestHorseCoreRouterRadix.FallbackFlushesCookies;
var
  LRaw: TWebResponse;
begin
  LRaw := TInterfacedWebResponse.Create(nil);
  try
    FreeAndNil(FResponse);
    FResponse := THorseResponse.Create(LRaw);
    FRouter.RegisterRoute(mtGet, '/cookie-error',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        Res.Cookie('error-cookie', 'retained');
        raise Exception.Create('cookie error');
      end);
    FRequest.Populate('GET', mtGet, '/cookie-error', '', '');
    Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
    Assert.AreEqual('Internal Application Error: cookie error', string(LRaw.Content));
    Assert.AreEqual(1, LRaw.Cookies.Count);
    Assert.AreEqual('retained', string(LRaw.Cookies[0].Value));
  finally
    FreeAndNil(FResponse);
    LRaw.Free;
  end;
end;

{ TTestHorseCoreRouterRadix }

{$IF SizeOf(Char) > 1}
procedure TTestHorseCoreRouterRadix.ExecuteRouteWithUtf8LiteralAndParam;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/ação/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('42', Req.Params.Items['id']);
    end);

  FRequest.Populate('GET', mtGet, '/ação/42', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

{$ENDIF}

procedure TTestHorseCoreRouterRadix.Setup;
begin
  FRouter := THorseRadixRouter.Create;
  FRequest := THorseRequest.Create(nil);
  FResponse := THorseResponse.Create(nil);
end;

procedure TTestHorseCoreRouterRadix.TearDown;
begin
  FRouter.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteGroupWithParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/users/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
      Assert.AreEqual('/users/:id', Req.MatchedRoute);
    end);

  FRequest.Populate('GET', mtGet, '/users/123', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteGroupWithSubRoute;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/users/profile',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/profile', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteGroupWithTwoSubRoute;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/users/profile/settings',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/profile/settings', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteGroupWithThreeSubRoute;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/users/profile/settings/advanced',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/profile/settings/advanced', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteGroupWithTwoParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/users/:id/posts/:postid',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
      Assert.AreEqual('456', Req.Params.Items['postid']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/posts/456', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteGroupWithSubRouteAndParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/users/posts/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('999', Req.Params.Items['id']);
    end);

  FRequest.Populate('GET', mtGet, '/users/posts/999', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteGroupWithSubRouteAndTwoParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/users/posts/:id/comments/:commentid',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('12', Req.Params.Items['id']);
      Assert.AreEqual('34', Req.Params.Items['commentid']);
    end);

  FRequest.Populate('GET', mtGet, '/users/posts/12/comments/34', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteGroupWithParamsAndSubRoute;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/users/:id/posts',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/posts', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteGroupWithParamsAndSubRouteAndParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/users/:id/posts/:postid',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
      Assert.AreEqual('456', Req.Params.Items['postid']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/posts/456', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteWithTwoConsecutiveParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.RegisterRoute(mtGet, '/users/:id/:name',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
      Assert.AreEqual('regys', Req.Params.Items['name']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/regys', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteWithMiddlewares;
var
  LStep: Integer;
begin
  LStep := 0;
  
  // Middleware Global
  FRouter.RegisterMiddleware(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Assert.AreEqual(0, LStep);
      LStep := 1;
      Next();
      Assert.AreEqual(3, LStep);
      LStep := 4;
    end);

  // Route Middleware
  FRouter.RegisterRouteMiddleware(mtGet, '/test',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Assert.AreEqual(1, LStep);
      LStep := 2;
      Next();
      Assert.AreEqual(3, LStep);
    end);

  // Endpoint final
  FRouter.RegisterRoute(mtGet, '/test',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Assert.AreEqual(2, LStep);
      LStep := 3;
    end);

  FRequest.Populate('GET', mtGet, '/test', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.AreEqual(4, LStep);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteWithCoringaoPriority;
var
  LClientesCalled: Boolean;
  LPessoasCalled: Boolean;
  LCoringaoCalled: Boolean;
begin
  LClientesCalled := False;
  LPessoasCalled := False;
  LCoringaoCalled := False;

  FRouter.RegisterRoute(mtGet, '/clientes',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LClientesCalled := True;
    end);

  FRouter.RegisterRoute(mtGet, '/pessoas',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LPessoasCalled := True;
    end);

  FRouter.RegisterRoute(mtGet, '*',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCoringaoCalled := True;
    end);

  // Testando rota exata /clientes
  LClientesCalled := False;
  LPessoasCalled := False;
  LCoringaoCalled := False;
  FRequest.Populate('GET', mtGet, '/clientes', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LClientesCalled);
  Assert.IsFalse(LPessoasCalled);
  Assert.IsFalse(LCoringaoCalled);

  // Testando rota exata /pessoas
  LClientesCalled := False;
  LPessoasCalled := False;
  LCoringaoCalled := False;
  FRequest.Populate('GET', mtGet, '/pessoas', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsFalse(LClientesCalled);
  Assert.IsTrue(LPessoasCalled);
  Assert.IsFalse(LCoringaoCalled);

  // Testando rota genérica (wildcard)
  LClientesCalled := False;
  LPessoasCalled := False;
  LCoringaoCalled := False;
  FRequest.Populate('GET', mtGet, '/qualquercoisa', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsFalse(LClientesCalled);
  Assert.IsFalse(LPessoasCalled);
  Assert.IsTrue(LCoringaoCalled);
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteWithMethodNotAllowedAllowHeader;
var
  LAllow: string;
begin
  FRouter.RegisterRoute(mtGet, '/users',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
    end);

  FRouter.RegisterRoute(mtPost, '/users',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
    end);

  FRequest.Populate('PUT', mtPut, '/users', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.AreEqual(405, FResponse.Status);

  {$IF DEFINED(FPC)}
  LAllow := FResponse.CustomHeaders.Values['Allow'];
  {$ELSE}
  LAllow := FResponse.CustomHeaders.Items['Allow'];
  {$ENDIF}

  Assert.IsTrue(LAllow.Contains('GET'));
  Assert.IsTrue(LAllow.Contains('POST'));
end;

procedure TTestHorseCoreRouterRadix.ExecuteRouteWithPrefix;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouter.Prefix('/api');
  Assert.AreEqual('/api', FRouter.GetPrefix);

  FRouter.RegisterRoute(mtGet, '/users',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/api/users', '', '');
  Assert.IsTrue(FRouter.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseCoreRouterRadix);

end.
