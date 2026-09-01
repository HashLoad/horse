unit Tests.Horse.Core.Router.Radix.FPC;

interface

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestHorseCoreRouterRadixFPC = class
  public
    [Test]
    procedure ExecuteAsciiCallback;
{$IF SizeOf(Char) > 1}
    [Test]
    procedure ExecuteUtf8Literal;
{$ENDIF}
  end;

implementation

uses
  Horse.Callback,
  Horse.Commons,
  Horse.Core.Router.Radix,
  Horse.Proc,
  Horse.Request,
  Horse.Response;

var
  GCalled: Boolean;

procedure RouteCallback(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  GCalled := True;
end;

procedure TTestHorseCoreRouterRadixFPC.ExecuteAsciiCallback;
var
  Callback: THorseCallback;
  Request: THorseRequest;
  Response: THorseResponse;
  Router: THorseRadixRouter;
begin
  GCalled := False;
  Callback := Pointer(@RouteCallback);
  Router := THorseRadixRouter.Create;
  Request := THorseRequest.Create(nil);
  Response := THorseResponse.Create(nil);
  try
    Router.RegisterRoute(mtGet, '/ping', Callback);
    Request.Populate('GET', mtGet, '/ping', '', '');

    Assert.IsTrue(Router.Execute(Request, Response));
    Assert.IsTrue(GCalled);
    Assert.AreEqual('/ping', Request.MatchedRoute);
  finally
    Response.Free;
    Request.Free;
    Router.Free;
  end;
end;

{$IF SizeOf(Char) > 1}
procedure TTestHorseCoreRouterRadixFPC.ExecuteUtf8Literal;
var
  Callback: THorseCallback;
  Request: THorseRequest;
  Response: THorseResponse;
  Router: THorseRadixRouter;
begin
  Callback := Pointer(@RouteCallback);
  Router := THorseRadixRouter.Create;
  Request := THorseRequest.Create(nil);
  Response := THorseResponse.Create(nil);
  try
    GCalled := False;
    Router.RegisterRoute(mtGet, '/ação/:id', Callback);
    Request.Populate('GET', mtGet, '/ação/42', '', '');

    Assert.IsTrue(Router.Execute(Request, Response));
    Assert.IsTrue(GCalled);
    Assert.AreEqual('/ação/:id', Request.MatchedRoute);
    Assert.AreEqual('42', Request.Params.Items['id']);
  finally
    Response.Free;
    Request.Free;
    Router.Free;
  end;
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TTestHorseCoreRouterRadixFPC);

end.
