program FunctionReferencesCheck;

{$MODE DELPHI}{$H+}
{$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}

uses
  cthreads, SysUtils,
  Horse, Horse.Core, Horse.Instance, Horse.Callback, Horse.Proc,
  Horse.Core.Router.Contract, Horse.Core.RouterTree, Horse.Core.Router.Radix,
  Horse.Commons, Horse.Request, Horse.Response;

var
  Checks: Integer;
  Destroyed: Integer;

procedure Check(Condition: Boolean; const MessageText: string);
begin
  Inc(Checks);

  if not Condition then
    raise Exception.Create(MessageText);
end;

{$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}
type
  TLifetime = class(TInterfacedObject)
  public
    destructor Destroy; override;
  end;

destructor TLifetime.Destroy;
begin
  Inc(Destroyed);
  inherited;
end;

function MakeRoute(const Text: string; Counter, Step: PInteger): THorseCallback;
var
  Pin: IInterface;
begin
  Pin := TLifetime.Create;
  Result := procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Check(Pin <> nil, 'Captured interface must survive the factory');
      Check(Step^ = 4, 'Route must follow pre-validation');
      Inc(Counter^);
      Step^ := 5;
      Res.Send(Text);
    end;
end;

procedure InstallCapturedRoute(const Router: IHorseRouter; Counter, Step: PInteger);
begin
  Router.RegisterRoute(mtGet, '/closure', MakeRoute('captured value', Counter, Step));
end;

procedure TestRouter(UseRadix: Boolean);
var
  Router: IHorseRouter;
  Req: THorseRequest;
  Res: THorseResponse;
  Count, Step, Before: Integer;
begin
  THorse.ResetHooks;
  Count := 0;
  Step := 0;
  Before := Destroyed;

  if UseRadix then
    Router := THorseRadixRouter.Create
  else
    Router := THorseRouterTree.Create;

  THorse.AddOnRequest(procedure(AReq: THorseRequest; ARes: THorseResponse; Next: TNextProc)
    begin
      Check(Step = 0, 'onRequest order');
      Step := 1;
      Next();
    end);
  THorse.AddPreParsing(procedure(AReq: THorseRequest; ARes: THorseResponse; Next: TNextProc)
    begin
      Check(Step = 1, 'preParsing order');
      Step := 2;
      Next();
    end);
  THorse.AddPreValidation(procedure(AReq: THorseRequest; ARes: THorseResponse; Next: TNextProc)
    begin
      Check(Step = 3, 'preValidation must follow middleware');
      Step := 4;
      Next();
    end);
  THorse.AddOnResponse(procedure(AReq: THorseRequest; ARes: THorseResponse; Next: TNextProc)
    begin
      Check(Step = 6, 'onResponse must follow middleware unwind');
      Step := 7;
      Next();
    end);
  Router.RegisterMiddleware(procedure(AReq: THorseRequest; ARes: THorseResponse; Next: TNextProc)
    begin
      Check(Step = 2, 'Middleware order');
      Step := 3;
      Next();
      Check(Step = 5, 'Next must return after route');
      Step := 6;
    end);
  InstallCapturedRoute(Router, @Count, @Step);
  Check(Destroyed = Before, 'Router must retain captured interface');

  Req := THorseRequest.Create(nil);
  try
    Res := THorseResponse.Create(nil);
    try
      Req.Populate('GET', mtGet, '/closure', '', '');
      Check(Router.Execute(Req, Res), 'Router must handle closure');
      Check(Count = 1, 'Captured counter must change once');
      Check(Step = 7, 'All hooks must complete');
      Check(Res.BodyText = 'captured value', 'Captured string must survive factory');
      Check(Res.Status = 200, 'Closure route status');
    finally
      Res.Free;
    end;
  finally
    Req.Free;
    THorse.ResetHooks;
    Router := nil;
  end;

  Check(Destroyed = Before + 1, 'Router destruction must release captured interface');
  WriteLn('PASS captured route, middleware, hooks and lifetime; radix=', UseRadix);
end;

function MakePair(const Text: string): THorseCallbackRequestResponse;
begin
  Result := procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(Text);
    end;
end;

function MakeRequest(Counter: PInteger): THorseCallbackRequest;
begin
  Result := procedure(Req: THorseRequest)
    begin
      Inc(Counter^);
    end;
end;

procedure ExecutePath(const Router: IHorseRouter; const Path, Body: string; Status: Integer;
  Method: TMethodType = mtGet; ExpectHandled: Boolean = True);
var
  Req: THorseRequest;
  Res: THorseResponse;
begin
  Req := THorseRequest.Create(nil);
  try
    Res := THorseResponse.Create(nil);
    try
      if Method = mtPost then
        Req.Populate('POST', mtPost, Path, '', '')
      else
        Req.Populate('GET', mtGet, Path, '', '');
      Check(Router.Execute(Req, Res) = ExpectHandled, 'Unexpected handled result at ' + Path);
      Check(Res.BodyText = Body, 'Adapter response body at ' + Path + ': ' + Res.BodyText);
      Check(Res.Status = Status, 'Adapter response status at ' + Path + ': ' + IntToStr(Res.Status));
    finally
      Res.Free;
    end;
  finally
    Req.Free;
  end;
end;

procedure TestFallbacks(UseRadix: Boolean);
var
  Router: IHorseRouter;
begin
  if UseRadix then
    Router := THorseRadixRouter.Create
  else
    Router := THorseRouterTree.Create;

  Router.RegisterRoute(mtGet, '/exists', procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('exists');
    end);
  ExecutePath(Router, '/missing', 'Not Found', 404, mtGet, UseRadix);
  ExecutePath(Router, '/exists', 'Method Not Allowed', 405, mtPost);
  WriteLn('PASS not-found and method-not-allowed callbacks; radix=', UseRadix);
end;

procedure TestCoreAdapters;
var
  Count, I: Integer;
  Previous: IHorseRouter;
begin
  Count := 0;
  Previous := THorse.Routes;
  THorse.Routes := THorseRouterTree.Create;
  try
    THorse.Get('/pair', MakePair('core pair'));
    THorse.Get('/request', MakeRequest(@Count));
    ExecutePath(THorse.Routes, '/pair', 'core pair', 200);
    ExecutePath(THorse.Routes, '/request', '', 204);
    Check(Count = 1, 'Core request-only adapter');

    for I := 1 to 70 do
      THorse.Get('/pair' + IntToStr(I), MakePair('pair' + IntToStr(I)));

    for I := 1 to 70 do
      ExecutePath(THorse.Routes, '/pair' + IntToStr(I), 'pair' + IntToStr(I), 200);
  finally
    THorse.Routes := Previous;
  end;

  WriteLn('PASS core abbreviated callbacks');
end;

procedure TestReentrantNext(UseRadix: Boolean);
var
  Outer, Inner: IHorseRouter;
  Count: Integer;
begin
  Count := 0;

  if UseRadix then
  begin
    Outer := THorseRadixRouter.Create;
    Inner := THorseRadixRouter.Create;
  end else
  begin
    Outer := THorseRouterTree.Create;
    Inner := THorseRouterTree.Create;
  end;

  try
    Inner.RegisterRoute(mtGet, '/inner', procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        Inc(Count);
        Res.Send('inner');
      end);
    Outer.RegisterRouteMiddleware(mtGet, '/outer',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        ExecutePath(Inner, '/inner', 'inner', 200);
        Next();
      end);
    Outer.RegisterRoute(mtGet, '/outer', procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        Inc(Count);
        Res.Send('outer');
      end);
    ExecutePath(Outer, '/outer', 'outer', 200);
    Check(Count = 2, 'Next must resume outer route after nested dispatch');
  finally
    Outer := nil;
    Inner := nil;
  end;

  WriteLn('PASS reentrant route continuation; radix=', UseRadix);
end;

procedure TestInstanceHooks(Instance: THorseInstance; Expected: Integer);
var
  Req: THorseRequest;
  Res: THorseResponse;
  Count: Integer;
begin
  Count := 0;
  Instance.AddOnRequest(procedure(AReq: THorseRequest; ARes: THorseResponse; Next: TNextProc)
    begin
      Inc(Count, Expected);
      Next();
    end);
  Instance.AddOnResponse(procedure(AReq: THorseRequest; ARes: THorseResponse; Next: TNextProc)
    begin
      Inc(Count, Expected);
      Next();
    end);

  Req := THorseRequest.Create(nil);
  try
    Res := THorseResponse.Create(nil);
    try
      Instance.ExecuteOnRequest(Req, Res, procedure begin Inc(Count); end);
      Instance.ExecuteOnResponse(Req, Res);
      Check(Count = Expected * 2 + 1, 'Instance closure hooks and continuation');
    finally
      Res.Free;
    end;
  finally
    Req.Free;
    Instance.ResetHooks;
  end;
end;

procedure TestInstances;
var
  First, Second: THorseInstance;
  Count: Integer;
begin
  Count := 0;
  First := THorseInstance.Create;
  try
    Second := THorseInstance.Create;
    try
      First.Get('/same', MakePair('first'));
      Second.Get('/same', MakePair('second'));
      First.Get('/request', MakeRequest(@Count));
      ExecutePath(First.GetRoutes, '/same', 'first', 200);
      ExecutePath(Second.GetRoutes, '/same', 'second', 200);
      ExecutePath(First.GetRoutes, '/request', '', 200);
      Check(Count = 1, 'Instance request-only adapter');
      TestInstanceHooks(First, 2);
      TestInstanceHooks(Second, 5);
    finally
      Second.Free;
    end;
  finally
    First.Free;
  end;

  WriteLn('PASS isolated instances and abbreviated callbacks');
end;
{$ENDIF}

begin
  {$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}
  TestRouter(False);
  TestRouter(True);
  TestFallbacks(False);
  TestFallbacks(True);
  TestCoreAdapters;
  TestReentrantNext(False);
  TestReentrantNext(True);
  TestInstances;
  WriteLn('Checks: ', Checks, '; captured interfaces released: ', Destroyed);
  {$ELSE}
  WriteLn('Function-reference cases require -dHORSE_FPC_FUNCTIONREFERENCES; legacy suites run separately.');
  {$ENDIF}
end.
