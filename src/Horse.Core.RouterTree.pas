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
  Horse.Core.Router.Contract,
  Horse.Commons;

type
  PHorseRouterTree = ^THorseRouterTree;

  THorseRouterTree = class(TInterfacedObject, IHorseRouter)
  strict private
    FPrefix: string;
    FIsInitialized: Boolean;
    function GetQueuePath(APath: string; const AUsePrefix: Boolean = True): TQueue<string>;
    function ForcePath(const APath: string): THorseRouterTree;
    procedure PopulateQueuePath(AQueue: TQueue<string>; APath: string; const AUsePrefix: Boolean = True);
  private
    FPart: string;
    FTag: string;
    FFullPath: string;
    FIsParamsKey: Boolean;
    FRouterRegex: string;
    FIsRouterRegex: Boolean;
    {$IF DEFINED(FPC)}
    FMiddleware: TList<THorseCallback>;
    FRegexedKeys: TList<string>;
    FCallBack: TObjectDictionary<TMethodType, TList<THorseCallback>>;
    {$ELSE}
    FMiddleware: TArray<THorseCallback>;
    FRegexedKeys: TList<string>;
    FCallBack: TDictionary<TMethodType, TArray<THorseCallback>>;
    {$ENDIF}
    FHandlerMethods: TList<TMethodType>;
    FRoute: TObjectDictionary<string, THorseRouterTree>;
    procedure RegisterInternal(const AHTTPType: TMethodType; var APath: TQueue<string>; const ACallback: THorseCallback; const AFullPath: string; const AIsMiddleware: Boolean = False);
    procedure RegisterMiddlewareInternal(var APath: TQueue<string>; const AMiddleware: THorseCallback);
    function ExecuteInternal(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse; const AIsGroup: Boolean = False): Boolean;
    function CallNextPath(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
    function HasNext(const AMethod: TMethodType; const APaths: TArray<THorseBufferSlice>; AIndex: Integer = 0): Boolean;
    function CountLiteralSegments(const AMethod: TMethodType; const APaths: TArray<THorseBufferSlice>; AIndex: Integer = 0): Integer;
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
  protected
    function _AddRef: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  public
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

function THorseRouterTree.CallNextPath(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest;
  const AResponse: THorseResponse): Boolean;
var
  LCurrent: THorseBufferSlice;
  LKey: string;
  LAcceptable: THorseRouterTree;
  LFound, LIsGroup: Boolean;
  LBestAcceptable: THorseRouterTree;
  LBestScore, LScore: Integer;
  LPair: TPair<string, THorseRouterTree>;
begin
  LIsGroup := False;
  LCurrent := ASegments[AIndex];
  
  LFound := False;
  LAcceptable := nil;
  for LPair in FRoute do
  begin
    if (LPair.Key <> '*') and LCurrent.Compare(LPair.Key) then
    begin
      LAcceptable := LPair.Value;
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    LFound := FRoute.TryGetValue('*', LAcceptable);
  end;

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

function THorseRouterTree._AddRef: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function THorseRouterTree._Release: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

constructor THorseRouterTree.Create;
begin
  {$IF DEFINED(FPC)}
  FMiddleware := TList<THorseCallback>.Create;
  FCallBack := TObjectDictionary<TMethodType, TList<THorseCallback>>.Create([doOwnsValues]);
  {$ELSE}
  FMiddleware := nil;
  FCallBack := TDictionary<TMethodType, TArray<THorseCallback>>.Create;
  {$ENDIF}
  FRoute := TObjectDictionary<string, THorseRouterTree>.Create([doOwnsValues]);
  FRegexedKeys := TList<string>.Create;
  FHandlerMethods := TList<TMethodType>.Create;
  FPrefix := '';
  FIsRouterRegex := False;
end;

destructor THorseRouterTree.Destroy;
begin
  {$IF DEFINED(FPC)}
  FMiddleware.Free;
  FreeAndNil(FCallBack);
  {$ELSE}
  FMiddleware := nil;
  FreeAndNil(FCallBack);
  {$ENDIF}
  FreeAndNil(FRoute);
  FRegexedKeys.Clear;
  FRegexedKeys.Free;
  FHandlerMethods.Clear;
  FHandlerMethods.Free;
  inherited;
end;

function THorseRouterTree.Execute(const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
var
  LSegments, LSegmentsNotFound: TArray<THorseBufferSlice>;
  LMethodType: TMethodType;
  LRawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
  LBufferNotFound: TBytes;
begin
  LRawWebRequest := ARequest.RawWebRequest;
  if not Assigned(LRawWebRequest) then
  begin
    LMethodType := ARequest.MethodType;
  end
  else
  begin
    LMethodType := TMethodType.FromString(LRawWebRequest.Method);
  end;
  LSegments := ARequest.GetPathSegments;
  Result := ExecuteInternal(LSegments, 0, LMethodType, ARequest, AResponse);
  if not Result then
  begin
    SetLength(LSegmentsNotFound, 2);
    LBufferNotFound := TEncoding.UTF8.GetBytes('/*');
    LSegmentsNotFound[0] := THorseBufferSlice.Create(LBufferNotFound, 0, 0);
    LSegmentsNotFound[1] := THorseBufferSlice.Create(LBufferNotFound, 1, 1);
    
    Result := ExecuteInternal(LSegmentsNotFound, 0, LMethodType, ARequest, AResponse);
    if Result and (AResponse.Status = THTTPStatus.MethodNotAllowed.ToInteger) then
      AResponse.Send('Not Found').Status(THTTPStatus.NotFound);
  end;
  AResponse.FlushCookiesToWebResponse;
end;

function THorseRouterTree.ExecuteInternal(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest;
  const AResponse: THorseResponse; const AIsGroup: Boolean = False): Boolean;
var
  LNextCaller: TNextCaller;
  LFound: Boolean;
begin
  if FFullPath <> '' then
    ARequest.MatchedRoute := FFullPath;
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
      FPart,
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



function THorseRouterTree.CountLiteralSegments(const AMethod: TMethodType; const APaths: TArray<THorseBufferSlice>; AIndex: Integer = 0): Integer;
var
  LNext: THorseBufferSlice;
  LKey: string;
  LNextRoute: THorseRouterTree;
  LPair: TPair<string, THorseRouterTree>;
  LFound: Boolean;
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

  LFound := False;
  LNextRoute := nil;
  for LPair in FRoute do
  begin
    if LNext.Compare(LPair.Key) or (LPair.Key = '*') then
    begin
      LNextRoute := LPair.Value;
      LFound := True;
      Break;
    end;
  end;

  if LFound then
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

function THorseRouterTree.HasNext(const AMethod: TMethodType; const APaths: TArray<THorseBufferSlice>; AIndex: Integer = 0): Boolean;
var
  LNext: THorseBufferSlice;
  LKey: string;
  LNextRoute: THorseRouterTree;
  LPair: TPair<string, THorseRouterTree>;
  LFound: Boolean;
begin
  Result := False;
  if (Length(APaths) <= AIndex) then
    Exit(False);
  if (Length(APaths) - 1 = AIndex) and (APaths[AIndex].Compare(FPart) or FIsParamsKey) then
    Exit(FCallBack.ContainsKey(AMethod) or (AMethod = mtAny));

{$IFNDEF FPC}
  if FIsRouterRegex then
  begin
    Result := TRegEx.IsMatch(APaths[AIndex].ToString, Format('^%s$', [FRouterRegex]));
    Exit;
  end;
{$ENDIF}
  LNext := APaths[AIndex + 1];
  Inc(AIndex);
  
  LFound := False;
  LNextRoute := nil;
  for LPair in FRoute do
  begin
    if LNext.Compare(LPair.Key) or (LPair.Key = '*') then
    begin
      LNextRoute := LPair.Value;
      LFound := True;
      Break;
    end;
  end;

  if LFound then
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
  {$IF DEFINED(FPC)}
  LCallbacks: TList<THorseCallback>;
  {$ELSE}
  LCallbacks: TArray<THorseCallback>;
  {$ENDIF}
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

    {$IF DEFINED(FPC)}
    if not FCallBack.TryGetValue(AHTTPType, LCallbacks) then
    begin
      LCallbacks := TList<THorseCallback>.Create;
      FCallBack.Add(AHTTPType, LCallbacks);
    end;
    LCallbacks.Add(ACallback);
    {$ELSE}
    if not FCallBack.TryGetValue(AHTTPType, LCallbacks) then
    begin
      LCallbacks := nil;
    end;
    SetLength(LCallbacks, Length(LCallbacks) + 1);
    LCallbacks[Length(LCallbacks) - 1] := ACallback;
    FCallBack.AddOrSetValue(AHTTPType, LCallbacks);
    {$ENDIF}

    if not AIsMiddleware then
      FHandlerMethods.Add(AHTTPType);
    FFullPath := '/' + AFullPath.Trim(['/']);
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
  {$IF DEFINED(FPC)}
  FMiddleware.Add(AMiddleware);
  {$ELSE}
  SetLength(FMiddleware, Length(FMiddleware) + 1);
  FMiddleware[Length(FMiddleware) - 1] := AMiddleware;
  {$ENDIF}
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
    {$IF DEFINED(FPC)}
    FMiddleware.Add(AMiddleware)
    {$ELSE}
    begin
      SetLength(FMiddleware, Length(FMiddleware) + 1);
      FMiddleware[Length(FMiddleware) - 1] := AMiddleware;
    end
    {$ENDIF}
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
