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
  private
    procedure ExecuteNested(const APreValidation: Boolean);
  public
    [Test]
    procedure ExecuteAsciiCallback;
    [Test]
    procedure ExecuteNestedOnRequest;
    [Test]
    procedure ExecuteNestedPreValidation;
{$IF SizeOf(Char) > 1}
    [Test]
    procedure ExecuteUtf8Literal;
{$ENDIF}
  end;

implementation

uses
  Horse,
  Horse.Callback,
  Horse.Commons,
  Horse.Core.Router.Radix,
  Horse.Proc,
  Horse.Request,
  Horse.Response;

var
  GCalled: Boolean;
  GInnerCalled: Boolean;
  GInnerExecuted: Boolean;
  GInsideNestedExecute: Boolean;
  GInnerRouter: THorseRadixRouter;
  GInnerRequest: THorseRequest;
  GInnerResponse: THorseResponse;

procedure RouteCallback(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  GCalled := True;
end;

procedure InnerRouteCallback(Req: THorseRequest; Res: THorseResponse;
  Next: TNextProc);
begin
  GInnerCalled := True;
end;

procedure ExecuteInnerRoute;
begin
  GInsideNestedExecute := True;
  try
    GInnerExecuted := GInnerRouter.Execute(GInnerRequest, GInnerResponse);
  finally
    GInsideNestedExecute := False;
  end;
end;

procedure NestedHook(Req: THorseRequest; Res: THorseResponse;
  Next: TNextProc);
begin
  if not GInsideNestedExecute then
    ExecuteInnerRoute;
  Next;
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

procedure TTestHorseCoreRouterRadixFPC.ExecuteNested(
  const APreValidation: Boolean);
var
  Callback: THorseCallback;
  Request: THorseRequest;
  Response: THorseResponse;
  Router: THorseRadixRouter;
begin
  GCalled := False;
  GInnerCalled := False;
  GInnerExecuted := False;
  GInsideNestedExecute := False;
  Router := THorseRadixRouter.Create;
  Request := THorseRequest.Create(nil);
  Response := THorseResponse.Create(nil);
  GInnerRouter := THorseRadixRouter.Create;
  GInnerRequest := THorseRequest.Create(nil);
  GInnerResponse := THorseResponse.Create(nil);
  try
    Callback := Pointer(@RouteCallback);
    Router.RegisterRoute(mtGet, '/outer', Callback);
    Callback := Pointer(@InnerRouteCallback);
    GInnerRouter.RegisterRoute(mtGet, '/inner', Callback);
    Request.Populate('GET', mtGet, '/outer', '', '');
    GInnerRequest.Populate('GET', mtGet, '/inner', '', '');
    if APreValidation then
    begin
      Callback := Pointer(@NestedHook);
      THorse.AddPreValidation(Callback);
    end
    else
    begin
      Callback := Pointer(@NestedHook);
      THorse.AddOnRequest(Callback);
    end;

    Assert.IsTrue(Router.Execute(Request, Response));
    Assert.IsTrue(GInnerExecuted);
    Assert.IsTrue(GInnerCalled);
    Assert.IsTrue(GCalled);
  finally
    THorse.ResetHooks;
    GInnerResponse.Free;
    GInnerRequest.Free;
    GInnerRouter.Free;
    GInnerResponse := nil;
    GInnerRequest := nil;
    GInnerRouter := nil;
    Response.Free;
    Request.Free;
    Router.Free;
  end;
end;

procedure TTestHorseCoreRouterRadixFPC.ExecuteNestedOnRequest;
begin
  ExecuteNested(False);
end;

procedure TTestHorseCoreRouterRadixFPC.ExecuteNestedPreValidation;
begin
  ExecuteNested(True);
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
