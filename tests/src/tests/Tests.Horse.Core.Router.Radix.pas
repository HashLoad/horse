unit Tests.Horse.Core.Router.Radix;

interface

uses
  DUnitX.TestFramework, Horse.Core.Router.Radix, Horse.Request, Horse.Response,
  System.SysUtils, System.Generics.Collections,
  {$IF DEFINED(FPC)} HTTPApp {$ELSE} Web.HTTPApp {$ENDIF}, Horse.Commons;

type
  [TestFixture]
  TTestHorseCoreRouterRadix = class
  private
    FRouter: THorseRadixRouter;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
  public
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
  end;

implementation

{ TTestHorseCoreRouterRadix }

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

initialization
  TDUnitX.RegisterTestFixture(TTestHorseCoreRouterRadix);

end.
