program ExceptionPropagationCheck;

{$MODE DELPHI}{$H+}

uses
  cthreads, SysUtils, fpjson, jsonparser,
  Horse, Horse.Core.Router.Contract, Horse.Core.RouterTree,
  Horse.Core.Router.Radix, Horse.Callback, Horse.Commons,
  Horse.Exception, Horse.Exception.Interrupted, Horse.Proc,
  Horse.Request, Horse.Response;

var
  Kind, Mode: Integer;
  Caught, HookCalled, Continued: Boolean;
  Total, Failures: Integer;

procedure Check(Condition: Boolean; const MessageText: string);
begin
  if not Condition then
    raise Exception.Create(MessageText);
end;

procedure ErrorHook(const Req: THorseRequest; const Res: THorseResponse;
  const E: Exception);
begin
  HookCalled := True;
  Res.Send('hook').Status(THTTPStatus.ServiceUnavailable);
end;

procedure CatchError(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  try
    Next;
  except
    on E: Exception do
    begin
      Caught := True;
      if Mode = 3 then
        raise;
      Res.ContentType('application/json').Send('{"handled":true}')
        .Status(THTTPStatus.UnprocessableEntity);
    end;
  end;
end;

procedure PassThrough(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Next;
end;

procedure ThrowError(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  if Mode = 5 then
    Res.Send('existing error').Status(THTTPStatus.Conflict);
  if Kind = 1 then
    raise EHorseException.New.Error('route error').Status(THTTPStatus.BadRequest);
  raise Exception.Create('route error');
end;

procedure Interrupt(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('stopped').Status(THTTPStatus.Accepted);
  raise EHorseCallbackInterrupted.Create;
end;

procedure MustNotContinue(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Continued := True;
end;

procedure RunCase(UseRadix: Boolean; AKind, AMode, Source: Integer);
var
  Router: IHorseRouter;
  Req: THorseRequest;
  Res: THorseResponse;
  Interrupted: Boolean;
  LabelText: string;
  ErrorJSON: TJSONData;
begin
  Inc(Total);
  if UseRadix then
    LabelText := 'Radix'
  else
    LabelText := 'Tree';
  LabelText := Format('%s kind=%d mode=%d source=%d',
    [LabelText, AKind, AMode, Source]);
  Kind := AKind;
  Mode := AMode;
  Caught := False;
  HookCalled := False;
  Continued := False;
  THorse.OnError(nil);
  if UseRadix then
    Router := THorseRadixRouter.Create
  else
    Router := THorseRouterTree.Create;
  Req := THorseRequest.Create(nil);
  Res := THorseResponse.Create(nil);
  try
    try
      if Mode in [2, 4, 7] then
        THorse.OnError(@ErrorHook);
      if Mode in [1, 3, 4] then
        Router.RegisterMiddleware(Pointer(@CatchError));
      if Source = 1 then
        Router.RegisterRouteMiddleware(mtGet, '/error', Pointer(@PassThrough));
      if Source = 2 then
        Router.RegisterMiddleware(Pointer(@ThrowError));
      if Mode in [6, 7] then
      begin
        Router.RegisterRouteMiddleware(mtGet, '/error', Pointer(@Interrupt));
        Router.RegisterRoute(mtGet, '/error', Pointer(@MustNotContinue));
      end
      else
        Router.RegisterRoute(mtGet, '/error', Pointer(@ThrowError));
      Req.Populate('GET', mtGet, '/error', '', '');
      Interrupted := False;
      try
        Check(Router.Execute(Req, Res), 'Execute must report a handled request');
      except
        on EHorseCallbackInterrupted do
          Interrupted := True;
      end;
      if Mode in [6, 7] then
      begin
        Check(Interrupted, 'FPC must propagate callback interruption');
        Check(not HookCalled, 'Interruption must not invoke OnError');
        Check(not Continued, 'Interruption must stop subsequent callbacks');
        Check(Res.Status = 202, 'Interruption must preserve status');
        Check(Res.BodyText = 'stopped', 'Interruption must preserve body');
      end
      else
      begin
        Check(not Interrupted, 'Unexpected interruption');
        Check(Caught = (Mode in [1, 3, 4]), 'Middleware must see the exception');
        Check(HookCalled = ((Mode = 2) and (Kind = 0)), 'OnError fallback precedence');
        if Mode in [1, 4] then
        begin
          Check(Res.Status = 422, 'Middleware response status');
          Check(Res.BodyText = '{"handled":true}', 'Middleware response body');
          Check(Res.CSContentType = 'application/json', 'Middleware content type');
        end
        else if Mode = 5 then
        begin
          Check(Res.Status = 409, 'Existing error status');
          Check(Res.BodyText = 'existing error', 'Existing error body');
        end
        else if Kind = 1 then
        begin
          Check(Res.Status = 400, 'Horse exception status');
          ErrorJSON := GetJSON(Res.BodyText);
          try
            Check(ErrorJSON.FindPath('error') <> nil, 'Horse exception JSON error field');
            Check(ErrorJSON.FindPath('error').AsString = 'route error', 'Horse exception JSON message');
          finally
            ErrorJSON.Free;
          end;
          Check(Res.CSContentType = 'application/json; charset=UTF-8', 'JSON content type');
        end
        else if Mode = 2 then
        begin
          Check(Res.Status = 503, 'OnError response status');
          Check(Res.BodyText = 'hook', 'OnError response body');
        end
        else
        begin
          Check(Res.Status = 500, 'Default fallback status');
          Check(Res.BodyText = 'Internal Application Error: route error', 'Default fallback body');
        end;
      end;
      Writeln('PASS ', LabelText);
    except
      on E: Exception do
      begin
        Inc(Failures);
        Writeln('FAIL ', LabelText, ': ', E.ClassName, ': ', E.Message);
      end;
    end;
  finally
    THorse.OnError(nil);
    Res.Free;
    Req.Free;
    Router := nil;
  end;
end;

var
  Radix: Boolean;
  K, M: Integer;
begin
  for Radix := False to True do
  begin
    for K := 0 to 1 do
    begin
      for M := 0 to 4 do
        RunCase(Radix, K, M, 0);
      RunCase(Radix, K, 1, 1);
      RunCase(Radix, K, 1, 2);
    end;
    RunCase(Radix, 0, 5, 0);
    RunCase(Radix, 0, 6, 0);
    RunCase(Radix, 0, 7, 0);
  end;
  Writeln('Total: ', Total, '; passed: ', Total - Failures, '; failed: ', Failures);
  if Failures <> 0 then
    Halt(1);
end.
