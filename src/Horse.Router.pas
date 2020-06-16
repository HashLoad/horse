unit Horse.Router;

interface

uses Web.HTTPApp, Horse.HTTP, System.SysUtils, System.Generics.Collections;

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
    FCallBack: TObjectDictionary<TMethodType, TList<THorseCallback>>;
    FRoute: TDictionary<string, THorseRouterTree>;
    procedure RegisterInternal(AHTTPType: TMethodType; var APath: TQueue<string>; ACallback: THorseCallback);
    procedure RegisterMiddlewareInternal(var APath: TQueue<string>; AMiddleware: THorseCallback);
    procedure ExecuteInternal(APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest; AResponse: THorseResponse);
    procedure CallNextPath(var APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest; AResponse: THorseResponse);
    function HasNext(AMethod: TMethodType; APaths: TArray<String>; AIndex: Integer = 0): Boolean;
  public
    procedure RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
    procedure RegisterMiddleware(APath: string; AMiddleware: THorseCallback); overload;
    procedure RegisterMiddleware(AMiddleware: THorseCallback); overload;
    procedure Execute(ARequest: THorseRequest; AResponse: THorseResponse);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ THorseRouterTree }

uses Horse.Commons, Horse.Exception;

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
      begin
        LAcceptable.ExecuteInternal(APath, AHTTPType, ARequest, AResponse);
        Break;
      end;
    end;
  end
  else if LFound then
    LAcceptable.ExecuteInternal(APath, AHTTPType, ARequest, AResponse);
end;

constructor THorseRouterTree.Create;
begin
  FMiddleware := TList<THorseCallback>.Create;
  FRoute := TObjectDictionary<string, THorseRouterTree>.Create([doOwnsValues]);
  FRegexedKeys := TList<String>.Create;
  FCallBack := TObjectDictionary<TMethodType, TList<THorseCallback>>.Create([doOwnsValues]);
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

procedure THorseRouterTree.Execute(ARequest: THorseRequest; AResponse: THorseResponse);
var
  LQueue: TQueue<string>;
begin
  LQueue := GetQueuePath(THorseHackRequest(ARequest).GetWebRequest.PathInfo);
  try
    ExecuteInternal(LQueue, THorseHackRequest(ARequest).GetWebRequest.MethodType, ARequest,
      AResponse);
  finally
    LQueue.Free;
  end;
end;

procedure THorseRouterTree.ExecuteInternal(APath: TQueue<string>; AHTTPType: TMethodType; ARequest: THorseRequest;
  AResponse: THorseResponse);
var
  LCurrent: string;
  LIndex, LIndexCallback: Integer;
  LNext: TProc;
  LCallback: TList<THorseCallback>;
begin
  LCurrent := APath.Dequeue;

  LIndex := -1;
  LIndexCallback := -1;
  if Self.FIsRegex then
    ARequest.Params.Add(FTag, LCurrent);

  LNext := procedure
    begin
      inc(LIndex);
      if (FMiddleware.Count > LIndex) then
      begin
        Self.FMiddleware.Items[LIndex](ARequest, AResponse, LNext);
        if (FMiddleware.Count > LIndex) then
          LNext;
      end
      else if (APath.Count = 0) and assigned(FCallBack) then
      begin
        inc(LIndexCallback);
        if FCallBack.TryGetValue(AHTTPType, LCallback) then
        begin
          if (LCallback.Count > LIndexCallback) then
          begin
            if AResponse.Status = THTTPStatus.NotFound.ToInteger then
              AResponse.Send('');
            try
              LCallback.Items[LIndexCallback](ARequest, AResponse, LNext);
            except
              on E: Exception do
              begin
                if (not (E is EHorseCallbackInterrupted)) and (not (E is EHorseException)) then
                  AResponse.Send('Internal Application Error').Status(THTTPStatus.InternalServerError);
                raise;
              end;
            end;
            if (LCallback.Count > LIndexCallback) then
              LNext;
          end;
        end
        else
          AResponse.Send('Method Not Allowed').Status(THTTPStatus.MethodNotAllowed);
      end
      else
        CallNextPath(APath, AHTTPType, ARequest, AResponse);
    end;
  try
    LNext;
  finally
    LNext := nil;
  end;
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
  LSplitedPath: TArray<string>;
begin
  Result := TQueue<string>.Create;
  LSplitedPath := APath.Split(['/']);
  for LPart in LSplitedPath do
  begin
    if (Result.Count > 0) and LPart.IsEmpty then
      Continue;
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
    Exit(FCallBack.ContainsKey(AMethod) or (Amethod = mtAny));

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

procedure THorseRouterTree.RegisterInternal(AHTTPType: TMethodType; var APath: TQueue<string>; ACallback: THorseCallback);
var
  LNextPart: String;
  LCallbacks: TList<THorseCallback>;
begin
  if not FIsInitialized then
  begin
    FPart := APath.Dequeue;
    FIsRegex := FPart.StartsWith(':');
    FTag := FPart.Substring(1, Length(FPart) - 1);
    FIsInitialized := true;
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
