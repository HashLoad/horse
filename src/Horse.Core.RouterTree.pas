unit Horse.Core.RouterTree;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Generics.Collections,
  httpdefs,
  httpprotocol,
  RegExpr,
{$ELSE}
  Web.HTTPApp,
  System.Generics.Collections,
{$ENDIF}
  Horse.Request,
  Horse.Response,
  Horse.Callback,
  Horse.Commons;

type
  PHorseRouterTree = ^THorseRouterTree;

  THorseRouterTree = class
  strict private
    FPrefix: string;
    FIsInitialized: Boolean;
    function GetQueuePath(APath: string; const AUsePrefix: Boolean = True): TQueue<string>;
    function ForcePath(const APath: string): THorseRouterTree;
    procedure PopulateQueuePath(AQueue: TQueue<string>; APath: string; const AUsePrefix: Boolean = True);
  private
    FPart: string;
    FTag: string;
    FIsParamsKey: Boolean;
    FRouterRegex: string;
    FIsRouterRegex: Boolean;
    FMiddleware: TList<THorseCallback>;
    FRegexedKeys: TList<string>;
    FCallBack: TObjectDictionary<TMethodType, TList<THorseCallback>>;
    FHandlerMethods: TList<TMethodType>;
    FRoute: TObjectDictionary<string, THorseRouterTree>;
    procedure RegisterInternal(const AHTTPType: TMethodType; var APath: TQueue<string>; const ACallback: THorseCallback; const AFullPath: string; const AIsMiddleware: Boolean = False);
    procedure RegisterMiddlewareInternal(var APath: TQueue<string>; const AMiddleware: THorseCallback);
    function GetArrayPath(APath: string; const AUsePrefix: Boolean = True): TArray<string>;
    function ExecuteInternal(const ASegments: TArray<string>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse; const AIsGroup: Boolean = False): Boolean;
    function CallNextPath(const ASegments: TArray<string>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
    function HasNext(const AMethod: TMethodType; const APaths: TArray<string>; AIndex: Integer = 0): Boolean;
    function CountLiteralSegments(const AMethod: TMethodType; const APaths: TArray<string>; AIndex: Integer = 0): Integer;
    class function NormalizeParamKey(const APart: string): string; static;
  public
    function CreateRouter(const APath: string): THorseRouterTree;
    function GetPrefix: string;
    procedure Prefix(const APrefix: string);
    procedure RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
    procedure RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
    procedure RegisterMiddleware(const APath: string; const AMiddleware: THorseCallback); overload;
    procedure RegisterMiddleware(const AMiddleware: THorseCallback); overload;
    function Execute(const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
    constructor Create;
    destructor Destroy; override;

    property Route: TObjectDictionary<string, THorseRouterTree> read FRoute;
  end;

implementation

uses
{$IF DEFINED(FPC)}
  SysUtils,
  SyncObjs,
{$ELSE}
  System.SysUtils,
  System.RegularExpressions,
  System.SyncObjs,
{$ENDIF}
  Horse.Core.RouterTree.NextCaller;

threadvar
  TlsNextCaller: TNextCaller;

type
  TQueueString = TQueue<string>;
  TQueueStringList = TList<TQueueString>;

var
  GAllNextCallers: TList<TNextCaller> = nil;
  GAllNextCallersCS: TCriticalSection = nil;
  GQueuePool: TQueueStringList = nil;
  GQueuePoolCS: TCriticalSection = nil;
  GNextCallerPool: TList<TNextCaller> = nil;
  GNextCallerPoolCS: TCriticalSection = nil;

function GetNextCaller: TNextCaller;
begin
  if TlsNextCaller = nil then
  begin
    TlsNextCaller := TNextCaller.Create;
    GAllNextCallersCS.Enter;
    try
      GAllNextCallers.Add(TlsNextCaller);
    finally
      GAllNextCallersCS.Leave;
    end;
  end;
  Result := TlsNextCaller;
end;

function StringCommandToMethodType(const ACommand: string): TMethodType;
begin
  Result := TMethodType.mtAny;
  if ACommand = 'GET' then
    Result := TMethodType.mtGet
  else if ACommand = 'POST' then
    Result := TMethodType.mtPost
  else if ACommand = 'PUT' then
    Result := TMethodType.mtPut
  else if ACommand = 'HEAD' then
    Result := TMethodType.mtHead;
end;

class function THorseRouterTree.NormalizeParamKey(const APart: string): string;
begin
  if APart.StartsWith(':') then
    Result := ':_param'
  else
    Result := APart;
end;

procedure THorseRouterTree.RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
var
  LPathChain: TQueue<string>;
begin
  LPathChain := GetQueuePath(APath);
  try
    RegisterInternal(AHTTPType, LPathChain, ACallback, APath, False);
  finally
    LPathChain.Free;
  end;
end;

procedure THorseRouterTree.RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
var
  LPathChain: TQueue<string>;
begin
  LPathChain := GetQueuePath(APath);
  try
    RegisterInternal(AHTTPType, LPathChain, ACallback, APath, True);
  finally
    LPathChain.Free;
  end;
end;

function THorseRouterTree.CallNextPath(const ASegments: TArray<string>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest;
  const AResponse: THorseResponse): Boolean;
var
  LCurrent, LKey: string;
  LAcceptable: THorseRouterTree;
  LFound, LIsGroup: Boolean;
  LBestAcceptable: THorseRouterTree;
  LBestScore, LScore: Integer;
begin
  LIsGroup := False;
  LCurrent := ASegments[AIndex];
  LFound := FRoute.TryGetValue(LCurrent, LAcceptable);
  if (not LFound) then
  begin
    LFound := FRoute.TryGetValue(EmptyStr, LAcceptable);
    LIsGroup := LFound;
  end;
  if (not LFound) and (FRegexedKeys.Count > 0) then
  begin
    LBestAcceptable := nil;
    LBestScore := -1;
    for LKey in FRegexedKeys do
    begin
      FRoute.TryGetValue(LKey, LAcceptable);
      if LAcceptable.HasNext(AHTTPType, ASegments, AIndex) then
      begin
        LScore := LAcceptable.CountLiteralSegments(AHTTPType, ASegments, AIndex);
        if (LBestAcceptable = nil) or (LScore > LBestScore) then
        begin
          LBestAcceptable := LAcceptable;
          LBestScore := LScore;
        end;
      end;
    end;
    if LBestAcceptable <> nil then
      LFound := LBestAcceptable.ExecuteInternal(ASegments, AIndex, AHTTPType, ARequest, AResponse);
  end
  else if LFound then
    LFound := LAcceptable.ExecuteInternal(ASegments, AIndex, AHTTPType, ARequest, AResponse, LIsGroup);
  Result := LFound;
end;

constructor THorseRouterTree.Create;
begin
  FMiddleware := TList<THorseCallback>.Create;
  FRoute := TObjectDictionary<string, THorseRouterTree>.Create([doOwnsValues]);
  FRegexedKeys := TList<string>.Create;
  FCallBack := TObjectDictionary < TMethodType, TList < THorseCallback >>.Create([doOwnsValues]);
  FHandlerMethods := TList<TMethodType>.Create;
  FPrefix := '';
  FIsRouterRegex := False;
end;

destructor THorseRouterTree.Destroy;
begin
  FMiddleware.Free;
  FreeAndNil(FRoute);
  FRegexedKeys.Clear;
  FRegexedKeys.Free;
  FCallBack.Free;
  FHandlerMethods.Free;
  inherited;
end;

function THorseRouterTree.Execute(const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
var
  LPathInfo: string;
  LSegments, LSegmentsNotFound: TArray<string>;
  LMethodType: TMethodType;
  LRawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
begin
  LRawWebRequest := ARequest.RawWebRequest;
  if not Assigned(LRawWebRequest) then
  begin
    LPathInfo   := ARequest.RawPathInfo;
    LMethodType := ARequest.MethodType;
  end
  else
  begin
    LPathInfo := {$IF DEFINED(FPC)}LRawWebRequest.PathInfo
                 {$ELSE}LRawWebRequest.RawPathInfo{$ENDIF};
    LMethodType := StringCommandToMethodType(LRawWebRequest.Method);
  end;
  if LPathInfo.IsEmpty then
    LPathInfo := '/';
  LSegments := GetArrayPath(LPathInfo, False);
  Result := ExecuteInternal(LSegments, 0, LMethodType, ARequest, AResponse);
  if not Result then
  begin
    LSegmentsNotFound := GetArrayPath('/*', False);
    Result := ExecuteInternal(LSegmentsNotFound, 0, LMethodType, ARequest, AResponse);
    if Result and (AResponse.Status = THTTPStatus.MethodNotAllowed.ToInteger) then
      AResponse.Send('Not Found').Status(THTTPStatus.NotFound);
  end;
  AResponse.FlushCookiesToWebResponse;
end;

function THorseRouterTree.ExecuteInternal(const ASegments: TArray<string>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest;
  const AResponse: THorseResponse; const AIsGroup: Boolean = False): Boolean;
var
  LNextCaller: TNextCaller;
  LFound: Boolean;
begin
  LFound := False;
  LNextCaller := TNextCaller.Create;
  try
    LNextCaller.Configure(
      FCallBack,
      ASegments,
      AIndex,
      AHTTPType,
      ARequest,
      AResponse,
      AIsGroup,
      FMiddleware,
      FTag,
      FIsParamsKey,
      CallNextPath,
      LFound
    );
    LNextCaller.Init;
    LNextCaller.Next;
  finally
    LNextCaller.Free;
  end;
  Result := LFound;
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
begin
  Result := TQueue<string>.Create;
  PopulateQueuePath(Result, APath, AUsePrefix);
end;

procedure THorseRouterTree.PopulateQueuePath(AQueue: TQueue<string>; APath: string; const AUsePrefix: Boolean = True);
var
  LStart, LLen, LPathLen: Integer;
  LPart: string;
begin
  if AUsePrefix then
  begin
    if not APath.StartsWith('/') then
      APath := (FPrefix + '/' + APath)
    else
      APath := (FPrefix + APath);
  end;

  if APath.StartsWith('/') then
    AQueue.Enqueue(EmptyStr);

  LPathLen := Length(APath);
  LStart := 1;
  while LStart <= LPathLen do
  begin
    while (LStart <= LPathLen) and (APath[LStart] = '/') do
      Inc(LStart);
    
    if LStart > LPathLen then
      Break;
      
    LLen := 0;
    while (LStart + LLen <= LPathLen) and (APath[LStart + LLen] <> '/') do
      Inc(LLen);
      
    if LLen > 0 then
    begin
      LPart := Copy(APath, LStart, LLen);
      AQueue.Enqueue(LPart);
      LStart := LStart + LLen;
    end;
  end;
end;

function THorseRouterTree.GetArrayPath(APath: string; const AUsePrefix: Boolean = True): TArray<string>;
var
  LStart, LLen, LPathLen: Integer;
  LPart: string;
  LList: TList<string>;
begin
  if AUsePrefix then
  begin
    if not APath.StartsWith('/') then
      APath := (FPrefix + '/' + APath)
    else
      APath := (FPrefix + APath);
  end;

  LList := TList<string>.Create;
  try
    if APath.StartsWith('/') then
      LList.Add(EmptyStr);

    LPathLen := Length(APath);
    LStart := 1;
    while LStart <= LPathLen do
    begin
      while (LStart <= LPathLen) and (APath[LStart] = '/') do
        Inc(LStart);
      
      if LStart > LPathLen then
        Break;
        
      LLen := 0;
      while (LStart + LLen <= LPathLen) and (APath[LStart + LLen] <> '/') do
        Inc(LLen);
        
      if LLen > 0 then
      begin
        LPart := Copy(APath, LStart, LLen);
        LList.Add(LPart);
        LStart := LStart + LLen;
      end;
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

function THorseRouterTree.CountLiteralSegments(const AMethod: TMethodType; const APaths: TArray<string>; AIndex: Integer = 0): Integer;
var
  LNext, LKey: string;
  LNextRoute: THorseRouterTree;
begin
  Result := 0;
  if (Length(APaths) <= AIndex) then
    Exit;

  if (Length(APaths) - 1 = AIndex) then
  begin
    if not FIsParamsKey then
      Result := 1;
    Exit;
  end;

  LNext := APaths[AIndex + 1];
  Inc(AIndex);

  if FRoute.TryGetValue(LNext, LNextRoute) then
  begin
    Result := 1 + LNextRoute.CountLiteralSegments(AMethod, APaths, AIndex);
    Exit;
  end;

  for LKey in FRegexedKeys do
  begin
    if FRoute.Items[LKey].HasNext(AMethod, APaths, AIndex) then
    begin
      Result := FRoute.Items[LKey].CountLiteralSegments(AMethod, APaths, AIndex);
      Exit;
    end;
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
  if (Length(APaths) - 1 = AIndex) and ((APaths[AIndex] = FPart) or FIsParamsKey) then
    Exit(FCallBack.ContainsKey(AMethod) or (AMethod = mtAny));

{$IFNDEF FPC}
  if FIsRouterRegex then
  begin
    Result := TRegEx.IsMatch(APaths[AIndex], Format('^%s$', [FRouterRegex]));
    Exit;
  end;
{$ENDIF}
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

procedure THorseRouterTree.RegisterInternal(const AHTTPType: TMethodType; var APath: TQueue<string>; const ACallback: THorseCallback; const AFullPath: string; const AIsMiddleware: Boolean = False);
var
  LNextPart: string;
  LNormalizedNextPart: string;
  LCallbacks: TList<THorseCallback>;
  LForceRouter: THorseRouterTree;
  LRawPart: string;
begin
  if not FIsInitialized then
  begin
    LRawPart := APath.Dequeue;
    FPart := LRawPart;

    FIsParamsKey := FPart.StartsWith(':');
    FTag := FPart.Substring(1, Length(FPart) - 1);

    FIsRouterRegex := FPart.StartsWith('(') and FPart.EndsWith(')');
    FRouterRegex := FPart;

    FIsInitialized := True;
  end
  else
    APath.Dequeue;

  if APath.Count = 0 then
  begin
    if (not AIsMiddleware) and (FHandlerMethods.IndexOf(AHTTPType) >= 0) then
      raise Exception.Create(Format('Duplicate route detected: [%s] %s',
        [AHTTPType.ToString.ToUpper, AFullPath]));

    if not FCallBack.TryGetValue(AHTTPType, LCallbacks) then
    begin
      LCallbacks := TList<THorseCallback>.Create;
      FCallBack.Add(AHTTPType, LCallbacks);
    end;
    LCallbacks.Add(ACallback);

    if not AIsMiddleware then
      FHandlerMethods.Add(AHTTPType);
  end;

  if APath.Count > 0 then
  begin
    LNextPart := APath.Peek;
    LNormalizedNextPart := NormalizeParamKey(LNextPart);

    LForceRouter := ForcePath(LNormalizedNextPart);

    LForceRouter.RegisterInternal(AHTTPType, APath, ACallback, AFullPath, AIsMiddleware);
    if LForceRouter.FIsParamsKey or LForceRouter.FIsRouterRegex then
    begin
      if not FRegexedKeys.Contains(LNormalizedNextPart) then
        FRegexedKeys.Add(LNormalizedNextPart);
    end;
  end;
end;

procedure THorseRouterTree.RegisterMiddleware(const AMiddleware: THorseCallback);
begin
  FMiddleware.Add(AMiddleware);
end;

procedure THorseRouterTree.RegisterMiddleware(const APath: string; const AMiddleware: THorseCallback);
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

procedure THorseRouterTree.RegisterMiddlewareInternal(var APath: TQueue<string>; const AMiddleware: THorseCallback);
begin
  APath.Dequeue;
  if APath.Count = 0 then
    FMiddleware.Add(AMiddleware)
  else
    ForcePath(APath.Peek).RegisterMiddlewareInternal(APath, AMiddleware);
end;

initialization
  GQueuePool := TQueueStringList.Create;
  GQueuePoolCS := TCriticalSection.Create;
  GNextCallerPool := TList<TNextCaller>.Create;
  GNextCallerPoolCS := TCriticalSection.Create;
  GAllNextCallers := TList<TNextCaller>.Create;
  GAllNextCallersCS := TCriticalSection.Create;

finalization
  if Assigned(GQueuePool) then
  begin
    while GQueuePool.Count > 0 do
    begin
      GQueuePool.Items[GQueuePool.Count - 1].Free;
      GQueuePool.Delete(GQueuePool.Count - 1);
    end;
    GQueuePool.Free;
  end;
  GQueuePoolCS.Free;

  if Assigned(GNextCallerPool) then
  begin
    while GNextCallerPool.Count > 0 do
    begin
      GNextCallerPool.Items[GNextCallerPool.Count - 1].Free;
      GNextCallerPool.Delete(GNextCallerPool.Count - 1);
    end;
    GNextCallerPool.Free;
  end;
  GNextCallerPoolCS.Free;

  if Assigned(GAllNextCallers) then
  begin
    GAllNextCallersCS.Enter;
    try
      while GAllNextCallers.Count > 0 do
      begin
        GAllNextCallers.Items[GAllNextCallers.Count - 1].Free;
        GAllNextCallers.Delete(GAllNextCallers.Count - 1);
      end;
      GAllNextCallers.Free;
    finally
      GAllNextCallersCS.Leave;
    end;
  end;
  GAllNextCallersCS.Free;

end.
