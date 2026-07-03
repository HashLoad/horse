unit Tests.Horse.Core.RouterTree;

interface

uses
  DUnitX.TestFramework, Horse.Core.RouterTree, Horse.Request, Horse.Response,
  System.SysUtils, System.Generics.Collections,
  {$IF DEFINED(FPC)} HTTPApp {$ELSE} Web.HTTPApp {$ENDIF}, Horse.Commons;

type
  [TestFixture]
  TTestHorseCoreRouterTree = class
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
    procedure ExecuteRouteWithParamsRegexAndNormalParams;
    [Test]
    procedure ExecuteRouteWithTwoParamsRegexAndNormalParams;
    [Test]
    procedure ExecuteRouteWithNormalParamsAndTwoParamsRegex;
    [Test]
    procedure ExecuteRouteWithMultipleRegexGroup;
    [Test]
    procedure ExecuteRouteWithMultipleRegexGroupSameSpecificity;
    [Test]
    procedure ExecuteRouteWithRegexGroupAndLiteral;
    [Test]
    procedure ExecuteRouteWithTwoConsecutiveParams;
    [Test]
    procedure ExecuteRouteWithCoringao;
    [Test]
    procedure ExecuteRouteWithCoringaoSubRoute;
    [Test]
    procedure ExecuteRouteWithCoringaoMiddlePath;
    [Test]
    procedure ExecuteRouteWithTwoConsecutiveCoringao;
    [Test]
    procedure ExecuteRouteWithCoringaoPriority;
  end;

implementation

{ TTestHorseCoreRouterTree }

procedure TTestHorseCoreRouterTree.Setup;
begin
  FRouterTree := THorseRouterTree.Create;
  FRequest := THorseRequest.Create(nil);
  FResponse := THorseResponse.Create(nil);
end;

procedure TTestHorseCoreRouterTree.TearDown;
begin
  FRouterTree.Free;
  FRequest.Free;
  FResponse.Free;
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteGroupWithParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteGroupWithSubRoute;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/profile',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/profile', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteGroupWithTwoSubRoute;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/profile/settings',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/profile/settings', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteGroupWithThreeSubRoute;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/profile/settings/advanced',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/profile/settings/advanced', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteGroupWithTwoParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/:id/posts/:postid',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
      Assert.AreEqual('456', Req.Params.Items['postid']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/posts/456', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteGroupWithSubRouteAndParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/posts/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('999', Req.Params.Items['id']);
    end);

  FRequest.Populate('GET', mtGet, '/users/posts/999', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteGroupWithSubRouteAndTwoParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/posts/:id/comments/:commentid',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('12', Req.Params.Items['id']);
      Assert.AreEqual('34', Req.Params.Items['commentid']);
    end);

  FRequest.Populate('GET', mtGet, '/users/posts/12/comments/34', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteGroupWithParamsAndSubRoute;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/:id/profile',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/profile', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteGroupWithParamsAndSubRouteAndParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/:id/posts/:postid/comments',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
      Assert.AreEqual('456', Req.Params.Items['postid']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/posts/456/comments', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithParamsRegexAndNormalParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/([0-9]+)/posts/:postid',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('456', Req.Params.Items['postid']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/posts/456', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithTwoParamsRegexAndNormalParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/([0-9]+)/posts/([a-z]+)/comments/:commentid',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('789', Req.Params.Items['commentid']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/posts/abc/comments/789', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithNormalParamsAndTwoParamsRegex;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/:id/posts/([a-z]+)/comments/([0-9]+)',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/posts/abc/comments/456', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithMultipleRegexGroup;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/([0-9]+)/([a-z]+)',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/123/abc', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithMultipleRegexGroupSameSpecificity;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/([0-9]+)/profile',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/123/profile', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithRegexGroupAndLiteral;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/([0-9]+)/settings',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/123/settings', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithTwoConsecutiveParams;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/:id/:subid',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
      Assert.AreEqual('123', Req.Params.Items['id']);
      Assert.AreEqual('456', Req.Params.Items['subid']);
    end);

  FRequest.Populate('GET', mtGet, '/users/123/456', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithCoringao;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/*',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/any/random/path', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithCoringaoSubRoute;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/*',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/any/random/path', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithCoringaoMiddlePath;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/([a-zA-Z0-9\-]+)/profile',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/any-value/profile', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithTwoConsecutiveCoringao;
var
  LCalled: Boolean;
begin
  LCalled := False;
  FRouterTree.RegisterRoute(mtGet, '/users/([a-zA-Z0-9\-]+)/([a-zA-Z0-9\-]+)',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCalled := True;
    end);

  FRequest.Populate('GET', mtGet, '/users/any/random', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LCalled);
end;

procedure TTestHorseCoreRouterTree.ExecuteRouteWithCoringaoPriority;
var
  LClientesCalled: Boolean;
  LPessoasCalled: Boolean;
  LCoringaoCalled: Boolean;
begin
  LClientesCalled := False;
  LPessoasCalled := False;
  LCoringaoCalled := False;

  FRouterTree.RegisterRoute(mtGet, '/clientes',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LClientesCalled := True;
    end);

  FRouterTree.RegisterRoute(mtGet, '/pessoas',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LPessoasCalled := True;
    end);

  FRouterTree.RegisterRoute(mtGet, '*',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      LCoringaoCalled := True;
    end);

  // Testando rota exata /clientes
  LClientesCalled := False;
  LPessoasCalled := False;
  LCoringaoCalled := False;
  FRequest.Populate('GET', mtGet, '/clientes', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsTrue(LClientesCalled);
  Assert.IsFalse(LPessoasCalled);
  Assert.IsFalse(LCoringaoCalled);

  // Testando rota exata /pessoas
  LClientesCalled := False;
  LPessoasCalled := False;
  LCoringaoCalled := False;
  FRequest.Populate('GET', mtGet, '/pessoas', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsFalse(LClientesCalled);
  Assert.IsTrue(LPessoasCalled);
  Assert.IsFalse(LCoringaoCalled);

  // Testando rota genérica (wildcard)
  LClientesCalled := False;
  LPessoasCalled := False;
  LCoringaoCalled := False;
  FRequest.Populate('GET', mtGet, '/qualquercoisa', '', '');
  Assert.IsTrue(FRouterTree.Execute(FRequest, FResponse));
  Assert.IsFalse(LClientesCalled);
  Assert.IsFalse(LPessoasCalled);
  Assert.IsTrue(LCoringaoCalled);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseCoreRouterTree);

end.
