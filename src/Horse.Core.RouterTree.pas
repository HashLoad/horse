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
  Horse.Request, Horse.Response, Horse.Proc, Horse.Commons, Horse.Callback;

type
  PHorseRouterTree = ^THorseRouterTree;

  THorseRouterTree = class
  strict private
    FPrefix: string;
    FIsInitialized: Boolean;
    function GetQueuePath(APath: string; const AUsePrefix: Boolean = True): TQueue<string>;
    function ForcePath(const APath: string): THorseRouterTree;
  private
    FPart: string;
    FTag: string;
    FIsRegex: Boolean;
    FMiddleware: TMiddlewares;
    FRegexedKeys: TList<string>;
    FCallBack: TObjectDictionary<TMethodType, TMiddlewares>;
    FRoute: TObjectDictionary<string, THorseRouterTree>;
    procedure RegisterInternal(const AHTTPType: TMethodType; var APath: TQueue<string>; const ACallback: THorseCallback);
    procedure RegisterMiddlewareInternal(var APath: TQueue<string>; const AMiddleware: THorseCallback; ACallbackName: String);
    function ExecuteInternal(const APath: TQueue<string>; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse; const AIsGroup: Boolean = False): Boolean;
    function CallNextPath(var APath: TQueue<string>; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
    function HasNext(const AMethod: TMethodType; const APaths: TArray<string>; AIndex: Integer = 0): Boolean;
  public
    function CreateRouter(const APath: string): THorseRouterTree;
    function GetPrefix: string;
    procedure Prefix(const APrefix: string);
    procedure RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
    procedure RegisterMiddleware(const APath: string; const AMiddleware: THorseCallback; ACallbackName: String = ''); overload;
    procedure RegisterMiddleware(const AMiddleware: THorseCallback); overload;
    function Execute(const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Horse.Exception, Horse.Core.RouterTree.NextCaller;

procedure THorseRouterTree.RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
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

function THorseRouterTree.CallNextPath(var APath: TQueue<string>; const AHTTPType: TMethodType; const ARequest: THorseRequest;
  const AResponse: THorseResponse): Boolean;
var
  LCurrent, LKey: string;
  LAcceptable: THorseRouterTree;
  LFound, LIsGroup: Boolean;
  LPathOrigin: TQueue<string>;
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
  FMiddleware := TMiddlewares.Create;
  FRoute := TObjectDictionary<string, THorseRouterTree>.Create([doOwnsValues]);
  FRegexedKeys := TList<string>.Create;
  FCallBack := TObjectDictionary<TMethodType, TMiddlewares>.Create([doOwnsValues]);
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

function THorseRouterTree.Execute(const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
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

function THorseRouterTree.ExecuteInternal(const APath: TQueue<string>; const AHTTPType: TMethodType; const ARequest: THorseRequest;
  const AResponse: THorseResponse; const AIsGroup: Boolean = False): Boolean;
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

function THorseRouterTree.ForcePath(const APath: string): THorseRouterTree;
begin
  if not FRoute.TryGetValue(APath, Result) then
  begin
    Result := THorseRouterTree.Create;
    FRoute.Add(APath, Result);
  end;
end;

function THorseRouterTree.CreateRouter(const APath: string): THorseRouterTree;
begin
  Result := ForcePath(APath);
end;

procedure THorseRouterTree.Prefix(const APrefix: string);
begin
  FPrefix := '/' + APrefix.Trim(['/']);
end;

function THorseRouterTree.GetPrefix: string;
begin
  Result := FPrefix;
end;

function THorseRouterTree.GetQueuePath(APath: string; const AUsePrefix: Boolean = True): TQueue<string>;
var
  LPart: string;
  LSplitedPath: TArray<string>;
begin
  Result := TQueue<string>.Create;
  if AUsePrefix then
    if not APath.StartsWith('/') then
      APath := (FPrefix + '/' + APath)
    else
      APath := (FPrefix + APath);
  LSplitedPath := APath.Split(['/']);
  for LPart in LSplitedPath do
  begin
    if (Result.Count > 0) and LPart.IsEmpty then
      Continue;
    Result.Enqueue(LPart);
  end;
end;

function THorseRouterTree.HasNext(const AMethod: TMethodType; const APaths: TArray<string>; AIndex: Integer = 0): Boolean;
var
  LNext, LKey: string;
  LNextRoute: THorseRouterTree;
begin
  Result := False;
  if (Length(APaths) <= AIndex) then
    Exit(False);
  if (Length(APaths) - 1 = AIndex) and ((APaths[AIndex] = FPart) or (FIsRegex)) then
    Exit(FCallBack.ContainsKey(AMethod) or (AMethod = mtAny));

  LNext := APaths[AIndex + 1];
  Inc(AIndex);
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

procedure THorseRouterTree.RegisterInternal(const AHTTPType: TMethodType; var APath: TQueue<string>; const ACallback: THorseCallback);
var
  LNextPart: string;
  LCallbacks: TMiddlewares;
  LKey: String;
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
      LCallbacks := TMiddlewares.Create;
      LKey := 'KEY-' + (LCallbacks.Count + 1).ToString;
      LCallbacks.Add(LKey, ACallback);
      FCallBack.Add(AHTTPType, LCallbacks);
    end;
//    LKey := 'KEY-' + (LCallbacks.Count + 1).ToString;
//    LCallbacks.Add(LKey, ACallback);
  end;

  if APath.Count > 0 then
  begin
    LNextPart := APath.Peek;
    ForcePath(LNextPart).RegisterInternal(AHTTPType, APath, ACallback);
    if ForcePath(LNextPart).FIsRegex then
      FRegexedKeys.Add(LNextPart);
  end;
end;

procedure THorseRouterTree.RegisterMiddleware(const AMiddleware: THorseCallback);
var
  LKey: String;
begin
  LKey := 'KEY-' + (FMiddleware.Count + 1).ToString;
  FMiddleware.Add(LKey, AMiddleware);
end;

procedure THorseRouterTree.RegisterMiddleware(const APath: string;
  const AMiddleware: THorseCallback; ACallbackName: String);
var
  LPathChain: TQueue<string>;
begin
  LPathChain := GetQueuePath(APath);
  try
    RegisterMiddlewareInternal(LPathChain, AMiddleware, ACallbackName);
  finally
    LPathChain.Free;
  end;
end;

procedure THorseRouterTree.RegisterMiddlewareInternal(var APath: TQueue<string>;
  const AMiddleware: THorseCallback; ACallbackName: String);
begin
  APath.Dequeue;
  if APath.Count = 0 then
    FMiddleware.Add(ACallbackName, AMiddleware)
  else
    ForcePath(APath.Peek).RegisterMiddlewareInternal(APath, AMiddleware, ACallbackName);
end;

end.