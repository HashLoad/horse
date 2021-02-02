unit Horse.Core.RouterTree;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Generics.Collections, fpHTTP, httpprotocol,
{$ELSE}
  System.SysUtils, System.NetEncoding, Web.HTTPApp, System.Generics.Collections,
{$ENDIF}
  Horse.HTTP, Horse.Proc, Horse.Commons;

type
{$IF DEFINED(FPC)}
  THorseCallback = procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc);
  TCallNextPath = function(var APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest; AResponse: THorseResponse): Boolean of object;
{$ELSE}
  THorseCallback = reference to procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc);
  TCallNextPath = reference to function(var APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest; AResponse: THorseResponse): Boolean;
{$ENDIF}

  PHorseRouterTree = ^THorseRouterTree;

  THorseRouterTree = class
  strict private
    FPrefix: string;
    FIsInitialized: Boolean;
    function GetQueuePath(APath: string; AUsePrefix: Boolean = True): TQueue<string>;
    function ForcePath(APath: string): THorseRouterTree;
  private
    FPart: string;
    FTag: string;
    FIsRegex: Boolean;
    FMiddleware: TList<THorseCallback>;
    FRegexedKeys: TList<string>;
    FCallBack: TObjectDictionary<TMethodType, TList<THorseCallback>>;
    FRoute: TObjectDictionary<string, THorseRouterTree>;
    procedure RegisterInternal(AHTTPType: TMethodType; var APath: TQueue<string>; ACallback: THorseCallback);
    procedure RegisterMiddlewareInternal(var APath: TQueue<string>; AMiddleware: THorseCallback);
    function ExecuteInternal(APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest; AResponse: THorseResponse; AIsGroup: Boolean = False): Boolean;
    function CallNextPath(var APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest; AResponse: THorseResponse): Boolean;
    function HasNext(AMethod: TMethodType; APaths: TArray<string>; AIndex: Integer = 0): Boolean;
  public
    function CreateRouter(APath: string): THorseRouterTree;
    function GetPrefix(): string;
    procedure Prefix(APrefix: string);
    procedure RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
    procedure RegisterMiddleware(APath: string; AMiddleware: THorseCallback); overload;
    procedure RegisterMiddleware(AMiddleware: THorseCallback); overload;
    function Execute(ARequest: THorseRequest; AResponse: THorseResponse): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;


  TNextCaller = class
  private
    { private declarations }
    FIndex: Integer;
    FIndexCallback: Integer;
    FPath: TQueue<string>;
    FHTTPType: TMethodType;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
    FMiddleware: TList<THorseCallback>;
    FCallBack: TObjectDictionary<TMethodType, TList<THorseCallback>>;
    FCallNextPath: TCallNextPath;
    FIsGroup: Boolean;
    FTag: string;
    FIsRegex: Boolean;
    FFound: ^Boolean;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create;
    function Init: TNextCaller;
    function SetCallback(ACallback: TObjectDictionary < TMethodType, TList < THorseCallback >> ): TNextCaller;
    function SetPath(APath: TQueue<string>): TNextCaller;
    function SetHTTPType(AHTTPType: TMethodType): TNextCaller;
    function SetRequest(ARequest: THorseRequest): TNextCaller;
    function SetResponse(AResponse: THorseResponse): TNextCaller;
    function SetIsGroup(AIsGroup: Boolean): TNextCaller;
    function SetMiddleware(AMiddleware: TList<THorseCallback>): TNextCaller;
    function SetTag(ATag: string): TNextCaller;
    function SetIsRegex(AIsRegex: Boolean): TNextCaller;
    function SetOnCallNextPath(ACallNextPath: TCallNextPath): TNextCaller;
    function SetFound(var AFound: Boolean): TNextCaller;
    procedure Next;
  end;

implementation

{ THorseRouterTree }

uses Horse.Exception;

procedure THorseRouterTree.RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
var
  LPathChain: TQueue<string>;
begin
  LPathChain := GetQueuePath(APath);
  try
    RegisterInternal(AHTTPType, LPathChain, ACallback);
  finally
    LPathChain.Free;
  end;
end;

function THorseRouterTree.CallNextPath(var APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest;
  AResponse: THorseResponse): Boolean;
var
  LCurrent: string;
  LAcceptable: THorseRouterTree;
  LFound: Boolean;
  LKey: string;
  LPathOrigin: TQueue<string>;
  LIsGroup: Boolean;
begin
  LIsGroup := False;
  LPathOrigin := APath;
  LCurrent := APath.Peek;
  LFound := FRoute.TryGetValue(LCurrent, LAcceptable);
  if (not LFound) then
  begin
    LFound := FRoute.TryGetValue(EmptyStr, LAcceptable);
    if (LFound) then
      APath := LPathOrigin;
    LIsGroup := LFound;
  end;
  if (not LFound) and (FRegexedKeys.Count > 0) then
  begin
    for LKey in FRegexedKeys do
    begin
      FRoute.TryGetValue(LKey, LAcceptable);
      if LAcceptable.HasNext(AHTTPType, APath.ToArray) then
      begin
        LFound := LAcceptable.ExecuteInternal(APath, AHTTPType, ARequest, AResponse);
        Break;
      end;
    end;
  end
  else if LFound then
    LFound := LAcceptable.ExecuteInternal(APath, AHTTPType, ARequest, AResponse, LIsGroup);
  Result := LFound;
end;

constructor THorseRouterTree.Create;
begin
  FMiddleware := TList<THorseCallback>.Create;
  FRoute := TObjectDictionary<string, THorseRouterTree>.Create([doOwnsValues]);
  FRegexedKeys := TList<string>.Create;
  FCallBack := TObjectDictionary < TMethodType, TList < THorseCallback >>.Create([doOwnsValues]);
  FPrefix := '';
end;

destructor THorseRouterTree.Destroy;
begin
  FMiddleware.Free;
  FreeAndNil(FRoute);
  FRegexedKeys.Clear;
  FRegexedKeys.Free;
  FCallBack.Free;
  inherited;
end;

function THorseRouterTree.Execute(ARequest: THorseRequest; AResponse: THorseResponse): Boolean;
var
  LQueue: TQueue<string>;
begin
  LQueue := GetQueuePath({$IF DEFINED(FPC)}ARequest.RawWebRequest.PathInfo{$ELSE}ARequest.RawWebRequest.RawPathInfo{$ENDIF}, False);
  try
    Result := ExecuteInternal(LQueue, {$IF DEFINED(FPC)} StringCommandToMethodType(ARequest.RawWebRequest.Method)
      {$ELSE} ARequest.RawWebRequest.MethodType{$ENDIF}, ARequest, AResponse);
  finally
    LQueue.Free;
  end;
end;

function THorseRouterTree.ExecuteInternal(APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest;
  AResponse: THorseResponse; AIsGroup: Boolean = False): Boolean;
var
  LNextCaller: TNextCaller;
  LFound: Boolean;
begin
  LFound := False;
  LNextCaller := TNextCaller.Create;
  try
    LNextCaller.SetCallback(FCallBack);
    LNextCaller.SetPath(APath);
    LNextCaller.SetHTTPType(AHTTPType);
    LNextCaller.SetRequest(ARequest);
    LNextCaller.SetResponse(AResponse);
    LNextCaller.SetIsGroup(AIsGroup);
    LNextCaller.SetMiddleware(FMiddleware);
    LNextCaller.SetTag(FTag);
    LNextCaller.SetIsRegex(FIsRegex);
    LNextCaller.SetOnCallNextPath(CallNextPath);
    LNextCaller.SetFound(LFound);
    LNextCaller.Init;
    LNextCaller.Next;
  finally
    LNextCaller.Free;
    Result := LFound;
  end;
end;

function THorseRouterTree.ForcePath(APath: string): THorseRouterTree;
begin
  if not FRoute.TryGetValue(APath, Result) then
  begin
    Result := THorseRouterTree.Create;
    FRoute.Add(APath, Result);
  end;
end;

function THorseRouterTree.CreateRouter(APath: string): THorseRouterTree;
begin
  Result := ForcePath(APath);
end;

procedure THorseRouterTree.Prefix(APrefix: string);
begin
  FPrefix := '/' + APrefix.Trim(['/']);
end;

function THorseRouterTree.GetPrefix(): string;
begin
  Result := FPrefix;
end;

function THorseRouterTree.GetQueuePath(APath: string; AUsePrefix: Boolean = True): TQueue<string>;
var
  LPart: string;
  LSplitedPath: TArray<string>;
begin
  Result := TQueue<string>.Create;
  if AUsePrefix then
    APath := FPrefix + APath;
  LSplitedPath := APath.Split(['/']);
  for LPart in LSplitedPath do
  begin
    if (Result.Count > 0) and LPart.IsEmpty then
      Continue;
    Result.Enqueue(LPart);
  end;
end;

function THorseRouterTree.HasNext(AMethod: TMethodType; APaths: TArray<string>; AIndex: Integer = 0): Boolean;
var
  LNext: string;
  LNextRoute: THorseRouterTree;
  LKey: string;
begin
  Result := False;
  if (Length(APaths) <= AIndex) then
    Exit(False);
  if (Length(APaths) - 1 = AIndex) and ((APaths[AIndex] = FPart) or (FIsRegex)) then
    Exit(FCallBack.ContainsKey(AMethod) or (AMethod = mtAny));

  LNext := APaths[AIndex + 1];
  inc(AIndex);
  if FRoute.TryGetValue(LNext, LNextRoute) then
  begin
    Result := LNextRoute.HasNext(AMethod, APaths, AIndex);
  end
  else
  begin
    for LKey in FRegexedKeys do
    begin
      if FRoute.Items[LKey].HasNext(AMethod, APaths, AIndex) then
        Exit(True);
    end;
  end;
end;

procedure THorseRouterTree.RegisterInternal(AHTTPType: TMethodType; var APath: TQueue<string>; ACallback: THorseCallback);
var
  LNextPart: string;
  LCallbacks: TList<THorseCallback>;
begin
  if not FIsInitialized then
  begin
    FPart := APath.Dequeue;
    FIsRegex := FPart.StartsWith(':');
    FTag := FPart.Substring(1, Length(FPart) - 1);
    FIsInitialized := True;
  end
  else
    APath.Dequeue;

  if APath.Count = 0 then
  begin
    if not FCallBack.TryGetValue(AHTTPType, LCallbacks) then
    begin
      LCallbacks := TList<THorseCallback>.Create;
      FCallBack.Add(AHTTPType, LCallbacks);
    end;
    LCallbacks.Add(ACallback)
  end;

  if APath.Count > 0 then
  begin
    LNextPart := APath.Peek;
    ForcePath(LNextPart).RegisterInternal(AHTTPType, APath, ACallback);
    if ForcePath(LNextPart).FIsRegex then
      FRegexedKeys.Add(LNextPart);
  end;
end;

procedure THorseRouterTree.RegisterMiddleware(AMiddleware: THorseCallback);
begin
  FMiddleware.Add(AMiddleware);
end;

procedure THorseRouterTree.RegisterMiddleware(APath: string; AMiddleware: THorseCallback);
var
  LPathChain: TQueue<string>;
begin
  LPathChain := GetQueuePath(APath);
  try
    RegisterMiddlewareInternal(LPathChain, AMiddleware);
  finally
    LPathChain.Free;
  end;
end;

procedure THorseRouterTree.RegisterMiddlewareInternal(var APath: TQueue<string>; AMiddleware: THorseCallback);
begin
  APath.Dequeue;
  if APath.Count = 0 then
    FMiddleware.Add(AMiddleware)
  else
    ForcePath(APath.Peek).RegisterMiddlewareInternal(APath, AMiddleware);
end;

{ TNextCaller }

constructor TNextCaller.Create;
begin

end;

function TNextCaller.Init: TNextCaller;
var
  LCurrent: string;
begin
  Result := Self;
  if not FIsGroup then
    LCurrent := FPath.Dequeue;
  FIndex := -1;
  FIndexCallback := -1;
  if FIsRegex then
    FRequest.Params.Add(FTag, {$IF DEFINED(FPC)}HTTPDecode(LCurrent){$ELSE}TNetEncoding.URL.Decode(LCurrent){$ENDIF});
end;

procedure TNextCaller.Next;
var
  LCallback: TList<THorseCallback>;
begin
  inc(FIndex);
  if (FMiddleware.Count > FIndex) then
  begin
    FFound^ := True;
    Self.FMiddleware.Items[FIndex](FRequest, FResponse, Next);
    if (FMiddleware.Count > FIndex) then
      Next;
  end
  else if (FPath.Count = 0) and assigned(FCallBack) then
  begin
    inc(FIndexCallback);
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
            if (not(E is EHorseCallbackInterrupted)) and (not(E is EHorseException)) then
              FResponse.Send('Internal Application Error').Status(THTTPStatus.InternalServerError);
            raise;
          end;
        end;
        Next;
      end;
    end
    else
      FFound^ := False
  end
  else
    FFound^ := FCallNextPath(FPath, FHTTPType, FRequest, FResponse);
  if not FFound^ then
    FResponse.Send('Not Found').Status(THTTPStatus.NotFound);
end;

function TNextCaller.SetCallback(ACallback: TObjectDictionary < TMethodType, TList < THorseCallback >> ): TNextCaller;
begin
  Result := Self;
  FCallBack := ACallback;
end;

function TNextCaller.SetFound(var AFound: Boolean): TNextCaller;
begin
  Result := Self;
  FFound := @AFound;
end;

function TNextCaller.SetHTTPType(AHTTPType: TMethodType): TNextCaller;
begin
  Result := Self;
  FHTTPType := AHTTPType;
end;

function TNextCaller.SetIsGroup(AIsGroup: Boolean): TNextCaller;
begin
  Result := Self;
  FIsGroup := AIsGroup;
end;

function TNextCaller.SetIsRegex(AIsRegex: Boolean): TNextCaller;
begin
  Result := Self;
  FIsRegex := AIsRegex;
end;

function TNextCaller.SetMiddleware(AMiddleware: TList<THorseCallback>): TNextCaller;
begin
  Result := Self;
  FMiddleware := AMiddleware;
end;

function TNextCaller.SetOnCallNextPath(ACallNextPath: TCallNextPath): TNextCaller;
begin
  Result := Self;
  FCallNextPath := ACallNextPath;
end;

function TNextCaller.SetPath(APath: TQueue<string>): TNextCaller;
begin
  Result := Self;
  FPath := APath;
end;

function TNextCaller.SetRequest(ARequest: THorseRequest): TNextCaller;
begin
  Result := Self;
  FRequest := ARequest;
end;

function TNextCaller.SetResponse(AResponse: THorseResponse): TNextCaller;
begin
  Result := Self;
  FResponse := AResponse;
end;

function TNextCaller.SetTag(ATag: string): TNextCaller;
begin
  Result := Self;
  FTag := ATag;
end;

end.
