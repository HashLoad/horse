unit Horse.Router;

interface

uses
  Web.HTTPApp, Horse.HTTP, System.SysUtils, System.Generics.Collections;

type

  THorseCallback = reference to procedure(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc);

  THorseRouterTree = class
  strict private
    FIsInitialized: Boolean;
    function GetQueuePath(APath: string): TQueue<String>;
    function ForcePath(APath: String): THorseRouterTree;
  private
    FPart: String;
    FTag: String;
    FIsRegex: Boolean;
    FMiddleware: TList<THorseCallback>;
    FRegexedKeys: TList<String>;
    FCallBack: TDictionary<TMethodType, THorseCallback>;
    FRoute: TDictionary<string, THorseRouterTree>;
    procedure RegisterInternal(AHTTPType: TMethodType; var APath: TQueue<string>; ACallback: THorseCallback);
    procedure RegisterMiddlewareInternal(var APath: TQueue<string>; AMiddleware: THorseCallback);
    procedure ExecuteInternal(var APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest;
      AResponse: THorseResponse);

    Procedure CallNextPath(var APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest;
      AResponse: THorseResponse);

    function HasNext(AMethod: TMethodType; APaths: TArray<String>; AIndex: Integer = 0): Boolean;
  public
    procedure RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
    procedure RegisterMiddleware(APath: string; AMiddleware: THorseCallback); overload;
    procedure RegisterMiddleware(AMiddleware: THorseCallback); overload;
    procedure Execute(ARequest: THorseRequest; AResponse: THorseResponse);

    function CanExecute(ARequest: THorseRequest): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ THorseRouterTree }

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

procedure THorseRouterTree.CallNextPath(var APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest;
  AResponse: THorseResponse);
var
  LCurrent: string;
  LAcceptable: THorseRouterTree;
  LFound: Boolean;
  LKey: String;
begin
  LCurrent := APath.Peek;
  LFound := FRoute.TryGetValue(LCurrent, LAcceptable);
  if (not LFound) and (FRegexedKeys.Count > 0) then
  begin
    for LKey in FRegexedKeys do
    begin
      FRoute.TryGetValue(LKey, LAcceptable);
      if LAcceptable.HasNext(AHTTPType, APath.ToArray) then
        Break;
    end;
  end;
  LAcceptable.ExecuteInternal(APath, AHTTPType, ARequest, AResponse);
end;

function THorseRouterTree.CanExecute(ARequest: THorseRequest): Boolean;
var
  LQueue: TQueue<string>;
begin
  LQueue := GetQueuePath(THorseHackRequest(ARequest).GetWebRequest.PathInfo);
  try
    Result := Self.HasNext(THorseHackRequest(ARequest).GetWebRequest.MethodType, LQueue.ToArray);
  finally
    LQueue.Free;
  end;
end;

constructor THorseRouterTree.Create;
begin
  FMiddleware := TList<THorseCallback>.Create;
  FRoute := TObjectDictionary<string, THorseRouterTree>.Create;
  FRegexedKeys := TList<String>.Create;
  FCallBack := TDictionary<TMethodType, THorseCallback>.Create;
end;

destructor THorseRouterTree.Destroy;
begin
  FMiddleware.Free;
  FRoute.Free;
  FRegexedKeys.Free;
  FCallBack.Free;
  inherited;
end;

procedure THorseRouterTree.Execute(ARequest: THorseRequest; AResponse: THorseResponse);
var
  LQueue: TQueue<string>;
begin
  LQueue := GetQueuePath(THorseHackRequest(ARequest).GetWebRequest.PathInfo);
  try
    ExecuteInternal(LQueue, THorseHackRequest(ARequest).GetWebRequest.MethodType, ARequest, AResponse);
  finally
    LQueue.Free;
  end;
end;

procedure THorseRouterTree.ExecuteInternal(var APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest;
  AResponse: THorseResponse);
var
  LCurrent: string;
  LIndex: Integer;
  LNext: TProc;
  LHack: TQueue<string>;
begin
  LCurrent := APath.Dequeue;
  LHack := APath;
  
  LIndex := -1;
  if Self.FIsRegex then
    ARequest.Params.Add(FTag, LCurrent);

  LNext := procedure
    begin
      inc(LIndex);
      if (FMiddleware.Count > LIndex) then
        self.FMiddleware.Items[LIndex](ARequest, AResponse, LNext)
      else if (LHack.Count = 0) and assigned(FCallBack) then
        FCallBack.Items[AHTTPType](ARequest, AResponse, LNext)
      else
        CallNextPath(LHack, AHTTPType, ARequest, AResponse);
    end;
  LNext;
end;

function THorseRouterTree.ForcePath(APath: String): THorseRouterTree;
begin
  if not FRoute.TryGetValue(APath, Result) then
  begin
    Result := THorseRouterTree.Create;
    FRoute.Add(APath, Result);
  end;
end;

function THorseRouterTree.GetQueuePath(APath: string): TQueue<String>;
var
  LPart: String;
begin
  Result := TQueue<string>.Create;
  for LPart in APath.Split(['/']) do
  begin
    Result.Enqueue(LPart);
  end;
end;

function THorseRouterTree.HasNext(AMethod: TMethodType; APaths: TArray<String>; AIndex: Integer = 0): Boolean;
var
  LNext: string;
  LNextRoute: THorseRouterTree;
  LKey: String;
begin
  Result := False;
  if (Length(APaths) <= AIndex) then
    Exit(False);

  if (Length(APaths) - 1 = AIndex) and ((APaths[AIndex] = FPart) or (FIsRegex)) then
    Exit(FCallBack.ContainsKey(AMethod));

  LNext := APaths[AIndex + 1];

  inc(AIndex);

  if FRoute.TryGetValue(LNext, LNextRoute) then
    Result := LNextRoute.HasNext(AMethod, APaths, AIndex)
  else
  begin
    for LKey in FRegexedKeys do
    begin
      if FRoute.Items[LKey].HasNext(AMethod, APaths, AIndex) then
        Exit(true);
    end;
  end;
end;

procedure THorseRouterTree.RegisterInternal(AHTTPType: TMethodType; var APath: TQueue<string>;
  ACallback: THorseCallback);
var
  ANextPart: String;
begin
  if not FIsInitialized then
  begin
    FPart := APath.Dequeue;
    FIsRegex := FPart.StartsWith(':');
    FTag := FPart.Substring(1, Length(FPart) - 1);
    if APath.Count = 0 then
    begin
      FCallBack.Add(AHTTPType, ACallback);
    end;
    FIsInitialized := true;
  end
  else
    APath.Dequeue;
  if APath.Count > 0 then
  begin
    ANextPart := APath.Peek;
    ForcePath(ANextPart).RegisterInternal(AHTTPType, APath, ACallback);
    if ForcePath(ANextPart).FIsRegex then
      FRegexedKeys.Add(ANextPart);
  end;
end;

procedure THorseRouterTree.RegisterMiddleware(AMiddleware: THorseCallback);
begin
  FMiddleware.Add(AMiddleware);
end;

procedure THorseRouterTree.RegisterMiddleware(APath: string; AMiddleware: THorseCallback);
var
  LPathChain: TQueue<String>;
begin
  LPathChain := GetQueuePath(APath);
  try
    RegisterMiddlewareInternal(LPathChain, AMiddleware);
  finally
    LPathChain.Free;
  end;
end;

procedure THorseRouterTree.RegisterMiddlewareInternal(var APath: TQueue<string>; AMiddleware: THorseCallback);
var
  FCurrent: string;
begin
  FCurrent := APath.Dequeue;
  if APath.Count = 0 then
    FMiddleware.Add(AMiddleware)
  else
    ForcePath(APath.Peek).RegisterMiddlewareInternal(APath, AMiddleware);
end;

end.
