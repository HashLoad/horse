unit Horse.Core.RouterTree.NextCaller;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Generics.Collections,
  fpHTTP,
  httpprotocol,
{$ELSE}
  Web.HTTPApp,
  System.Generics.Collections,
{$ENDIF}
  Horse.Request,
  Horse.Response,
  Horse.Callback,
  Horse.Controller,
  Horse.Commons;

type
  TNextCaller = class
  private
    FIndex: Integer;
    FIndexCallback: Integer;
    FPath: TQueue<string>;
    FHTTPType: TMethodType;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
    FMiddleware: TList<THorseCallback>;
    FCallBack: TObjectDictionary<TMethodType, TList<THorseCallback>>;
    FControllers: TObjectDictionary<TMethodType, TControllerActionMap>;
    FControllerExecuted: Boolean;
    FCallNextPath: TCallNextPath;
    FIsGroup: Boolean;
    FTag: string;
    FIsParamsKey: Boolean;
    FFound: ^Boolean;
    procedure ExecuteController(const AAction: TControllerActionMap);
  public
    function Init: TNextCaller;
    function SetControllers(const AControllers: TObjectDictionary<TMethodType, TControllerActionMap>): TNextCaller;
    function SetCallback(const ACallback: TObjectDictionary<TMethodType, TList<THorseCallback>>): TNextCaller;
    function SetPath(const APath: TQueue<string>): TNextCaller;
    function SetHTTPType(const AHTTPType: TMethodType): TNextCaller;
    function SetRequest(const ARequest: THorseRequest): TNextCaller;
    function SetResponse(const AResponse: THorseResponse): TNextCaller;
    function SetIsGroup(const AIsGroup: Boolean): TNextCaller;
    function SetMiddleware(const AMiddleware: TList<THorseCallback>): TNextCaller;
    function SetTag(const ATag: string): TNextCaller;
    function SetIsParamsKey(const AIsParamsKey: Boolean): TNextCaller;
    function SetOnCallNextPath(const ACallNextPath: TCallNextPath): TNextCaller;
    function SetFound(var AFound: Boolean): TNextCaller;
    procedure Next;
  end;

implementation

uses
{$IF DEFINED(FPC)}
  SysUtils,
{$ELSE}
  System.SysUtils,
  System.NetEncoding,
{$ENDIF}
  Horse.Exception,
  Horse.Exception.Interrupted;

function TNextCaller.Init: TNextCaller;
var
  LCurrent: string;
begin
  Result := Self;
  if not FIsGroup then
    LCurrent := FPath.Dequeue;
  FIndex := -1;
  FIndexCallback := -1;
  if FIsParamsKey then
    FRequest.Params.Dictionary.AddOrSetValue(FTag, {$IF DEFINED(FPC)}HTTPDecode(LCurrent){$ELSE}TNetEncoding.URL.Decode(LCurrent){$ENDIF});
end;

procedure TNextCaller.Next;
var
  LCallback: TList<THorseCallback>;
  LAction: TControllerActionMap;
begin
  Inc(FIndex);
  if (FMiddleware.Count > FIndex) then
  begin
    FFound^ := True;
    Self.FMiddleware.Items[FIndex](FRequest, FResponse, Next);
    if (FMiddleware.Count > FIndex) then
      Next;
  end
  else if (FPath.Count = 0) and Assigned(FCallBack) then
  begin
    Inc(FIndexCallback);
    if FCallBack.TryGetValue(FHTTPType, LCallback) then
    begin
      if (LCallback.Count > FIndexCallback) then
      begin
        try
          FFound^ := True;
          LCallback.Items[FIndexCallback](FRequest, FResponse, Next);
        except
          on E: Exception do
          begin
            if (not(E is EHorseCallbackInterrupted)) and
               (not(E is EHorseException)) and
               (FResponse.Status < Integer(THTTPStatus.BadRequest))
            then
              FResponse.Send('Internal Application Error').Status(THTTPStatus.InternalServerError);
            raise;
          end;
        end;
        Next;
      end;
    end
    else if Assigned(FControllers) and FControllers.TryGetValue(FHTTPType, LAction) then
    begin
      if not FControllerExecuted then
      begin
        FControllerExecuted := True;
        FFound^ := True;
        ExecuteController(LAction);
        Next;
      end;
    end
    else if Assigned(FControllers) and FControllers.TryGetValue(mtAny, LAction) then
    begin
      if not FControllerExecuted then
      begin
        FControllerExecuted := True;
        FFound^ := True;
        ExecuteController(LAction);
        Next;
      end;
    end
    else
    begin
      if (FCallBack.Count > 0) or (Assigned(FControllers) and (FControllers.Count > 0)) then
      begin
        FFound^ := True;
        FResponse.Send('Method Not Allowed').Status(THTTPStatus.MethodNotAllowed);
      end
      else
        FResponse.Send('Not Found').Status(THTTPStatus.NotFound)
    end;
  end
  else
    FFound^ := FCallNextPath(FPath, FHTTPType, FRequest, FResponse);
  
  if not FFound^ then
  begin
    if Assigned(FControllers) and FControllers.TryGetValue(FHTTPType, LAction) then
    begin
      if not FControllerExecuted then
      begin
        FControllerExecuted := True;
        FFound^ := True;
        ExecuteController(LAction);
        Next;
      end;
    end
    else if Assigned(FControllers) and FControllers.TryGetValue(mtAny, LAction) then
    begin
      if not FControllerExecuted then
      begin
        FControllerExecuted := True;
        FFound^ := True;
        ExecuteController(LAction);
        Next;
      end;
    end
    else
      FResponse.Send('Not Found').Status(THTTPStatus.NotFound);
  end;
end;

procedure TNextCaller.ExecuteController(const AAction: TControllerActionMap);
type
  TControllerAction = procedure of object;
var
  LController: THorseController;
  LMethod: TMethod;
  LActionProc: TControllerAction;
begin
  LController := AAction.ControllerClass.Create(FRequest, FResponse, Next);
  try
    {$IF DEFINED(FPC)}
    LMethod.Code := Pointer(LController.MethodAddress(AAction.MethodName));
    {$ELSE}
    LMethod.Code := LController.MethodAddress(AAction.MethodName);
    {$ENDIF}
    
    if LMethod.Code <> nil then
    begin
      LMethod.Data := Pointer(LController);
      LActionProc := TControllerAction(LMethod);
      LActionProc();
      Exit;
    end
    else
      raise Exception.CreateFmt('Method "%s" not found in controller "%s"', [AAction.MethodName, LController.ClassName]);
      
    LController.Execute;
  finally
    LController.Free;
  end;
end;

function TNextCaller.SetControllers(const AControllers: TObjectDictionary<TMethodType, TControllerActionMap>): TNextCaller;
begin
  FControllers := AControllers;
  Result := Self;
end;

function TNextCaller.SetCallback(const ACallback: TObjectDictionary<TMethodType, TList<THorseCallback>>): TNextCaller;
begin
  FCallBack := ACallback;
  Result := Self;
end;

function TNextCaller.SetFound(var AFound: Boolean): TNextCaller;
begin
  FFound := @AFound;
  Result := Self;
end;

function TNextCaller.SetHTTPType(const AHTTPType: TMethodType): TNextCaller;
begin
  FHTTPType := AHTTPType;
  Result := Self;
end;

function TNextCaller.SetIsGroup(const AIsGroup: Boolean): TNextCaller;
begin
  FIsGroup := AIsGroup;
  Result := Self;
end;

function TNextCaller.SetIsParamsKey(const AIsParamsKey: Boolean): TNextCaller;
begin
  FIsParamsKey := AIsParamsKey;
  Result := Self;
end;

function TNextCaller.SetMiddleware(const AMiddleware: TList<THorseCallback>): TNextCaller;
begin
  FMiddleware := AMiddleware;
  Result := Self;
end;

function TNextCaller.SetOnCallNextPath(const ACallNextPath: TCallNextPath): TNextCaller;
begin
  FCallNextPath := ACallNextPath;
  Result := Self;
end;

function TNextCaller.SetPath(const APath: TQueue<string>): TNextCaller;
begin
  FPath := APath;
  Result := Self;
end;

function TNextCaller.SetRequest(const ARequest: THorseRequest): TNextCaller;
begin
  FRequest := ARequest;
  Result := Self;
end;

function TNextCaller.SetResponse(const AResponse: THorseResponse): TNextCaller;
begin
  FResponse := AResponse;
  Result := Self;
end;

function TNextCaller.SetTag(const ATag: string): TNextCaller;
begin
  FTag := ATag;
  Result := Self;
end;

end.
