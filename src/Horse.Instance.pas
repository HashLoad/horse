unit Horse.Instance;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
  Classes, SysUtils, Generics.Collections, syncobjs,
  {$ELSE}
  System.Classes, System.SysUtils, System.Generics.Collections, System.SyncObjs,
  {$ENDIF}
  Horse.Core.Base,
  Horse.Core.Router.Contract,
  Horse.Core.Route.Contract,
  Horse.Core.Group.Contract,
  Horse.Callback,
  Horse.Request,
  Horse.Response,
  Horse.Proc,
  Horse.Commons,
  Horse.Core;

type
  THorseInstance = class;

  {$IF DEFINED(FPC)}
  THorseServerLifecycleProc = procedure(const AInstance: THorseInstance);
  {$ELSE}
  THorseServerLifecycleProc = reference to procedure(const AInstance: THorseInstance);
  {$ENDIF}
  THorseServerLifecycleMethod = procedure(const AInstance: THorseInstance) of object;

  IHorseStartup = interface
    ['{8A9C128B-0B8A-4217-BA0E-A8FA98642289}']
    procedure Configure(const AInstance: THorseInstance);
  end;

  THorseInstance = class(THorseCoreBase)
  private
    FRoutes: IHorseRouter;
    FCallbacks: TList<THorseCallback>;
    FOnRequest: TList<THorseCallback>;
    FPreParsing: TList<THorseCallback>;
    FPreValidation: TList<THorseCallback>;
    FOnSendString: TList<THorseOnSendString>;
    FOnSendBytes: TList<THorseOnSendBytes>;
    FOnResponse: TList<THorseCallback>;
    FOnError: THorseOnError;
    FActiveRequests: Integer;
    FIsShuttingDown: Boolean;

    FOnBeforeListen: TList<THorseServerLifecycleProc>;
    FOnAfterListen: TList<THorseServerLifecycleProc>;
    FOnBeforeStop: TList<THorseServerLifecycleProc>;
    FOnAfterStop: TList<THorseServerLifecycleProc>;

    FOnBeforeListenMethod: TList<THorseServerLifecycleMethod>;
    FOnAfterListenMethod: TList<THorseServerLifecycleMethod>;
    FOnBeforeStopMethod: TList<THorseServerLifecycleMethod>;
    FOnAfterStopMethod: TList<THorseServerLifecycleMethod>;

    FPort: Integer;
    FHost: string;
    FRunning: Boolean;
    procedure EmptyNext;

    class function TrimPath(const APath: string): string; static;
    function RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback): THorseInstance;
    function RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback): THorseInstance;
    function RegisterCallbacksRoute(const AMethod: TMethodType; const APath: string): THorseInstance;

    function GetCallback(const ACallbackRequest: THorseCallbackRequestResponse): THorseCallback; overload;
    function GetCallback(const ACallbackRequest: THorseCallbackRequest): THorseCallback; overload;
    {$IFNDEF FPC}
    function GetCallback(const ACallbackResponse: THorseCallbackResponse): THorseCallback; overload;
    {$ENDIF}

    {$IF DEFINED(FPC)}
    function GetCallbacks: TList<THorseCallback>;
    {$ELSE}
    function GetCallbacks: TArray<THorseCallback>;
    {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure TriggerBeforeListen;
    procedure TriggerAfterListen;
    procedure TriggerBeforeStop;
    procedure TriggerAfterStop;

    function GetRoutes: IHorseRouter; override;

    // Métodos de Roteamento Fluídos
    function AddCallback(const ACallback: THorseCallback): THorseInstance; overload;
    {$IF DEFINED(FPC)}
    function AddCallbacks(const ACallbacks: TList<THorseCallback>): THorseInstance; overload;
    {$ELSE}
    function AddCallbacks(const ACallbacks: TArray<THorseCallback>): THorseInstance; overload;
    {$ENDIF}

    function Group: IHorseCoreGroup<THorseInstance>;
    function Route(const APath: string): IHorseCoreRoute<THorseInstance>; overload;

    procedure OnError(const ACallback: THorseOnError);
    function HasOnError: Boolean;
    procedure ExecuteOnError(const ARequest: THorseRequest; const AResponse: THorseResponse; const AException: Exception);

    procedure AddOnRequest(const ACallback: THorseCallback);
    procedure AddPreParsing(const ACallback: THorseCallback);
    procedure AddPreValidation(const ACallback: THorseCallback);
    procedure AddOnSend(const ACallback: THorseOnSendString); overload;
    procedure AddOnSend(const ACallback: THorseOnSendBytes); overload;
    procedure AddOnResponse(const ACallback: THorseCallback);
    procedure ResetHooks;

    function GetActiveRequests: Integer;
    procedure IncrementActiveRequests;
    procedure DecrementActiveRequests;
    procedure DoIncrementActiveRequests; override;
    procedure DoDecrementActiveRequests; override;
    function GetIsShuttingDown: Boolean;
    procedure SetIsShuttingDown(const AValue: Boolean);

    procedure ExecuteOnRequest(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
    procedure ExecutePreParsing(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
    procedure ExecutePreValidation(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
    procedure ExecuteOnSend(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: string); overload;
    procedure ExecuteOnSend(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: TBytes); overload;
    procedure ExecuteOnResponse(const ARequest: THorseRequest; const AResponse: THorseResponse);

    // Roteamento
    function Use(const APath: string; const ACallback: THorseCallback): THorseInstance; overload;
    function Use(const ACallback: THorseCallback): THorseInstance; overload;
    function Use(const APath: string; const ACallbacks: array of THorseCallback): THorseInstance; overload;
    function Use(const ACallbacks: array of THorseCallback): THorseInstance; overload;

    function All(const APath: string; const ACallback: THorseCallback): THorseInstance; overload;
    function All(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance; overload;
    function All(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance; overload;
    {$IFNDEF FPC}
    function All(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance; overload;
    {$ENDIF}

    function Get(const APath: string; const ACallback: THorseCallback): THorseInstance; overload;
    function Get(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance; overload;
    function Get(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance; overload;
    {$IFNDEF FPC}
    function Get(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance; overload;
    {$ENDIF}

    function Put(const APath: string; const ACallback: THorseCallback): THorseInstance; overload;
    function Put(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance; overload;
    function Put(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance; overload;
    {$IFNDEF FPC}
    function Put(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance; overload;
    {$ENDIF}

    // Post
    function Post(const APath: string; const ACallback: THorseCallback): THorseInstance; overload;
    function Post(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance; overload;
    function Post(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance; overload;
    {$IFNDEF FPC}
    function Post(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance; overload;
    {$ENDIF}

    function Delete(const APath: string; const ACallback: THorseCallback): THorseInstance; overload;
    function Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance; overload;
    function Delete(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance; overload;
    {$IFNDEF FPC}
    function Delete(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance; overload;
    {$ENDIF}

    function Patch(const APath: string; const ACallback: THorseCallback): THorseInstance; overload;
    function Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance; overload;
    function Patch(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance; overload;
    {$IFNDEF FPC}
    function Patch(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance; overload;
    {$ENDIF}

    function Head(const APath: string; const ACallback: THorseCallback): THorseInstance; overload;
    function Head(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance; overload;
    function Head(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance; overload;
    {$IFNDEF FPC}
    function Head(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance; overload;
    {$ENDIF}

    // Sobrecargas com Middlewares
    function Get(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance; overload;
    function Put(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance; overload;
    function Post(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance; overload;
    function Delete(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance; overload;
    function Patch(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance; overload;
    function Head(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance; overload;

    // Métodos de Roteamento virtuais de base herdados de THorseCoreBase para fluência de middlewares
    function BaseAddCallback(const ACallback: THorseCallback): THorseCoreBase; override;
    {$IF DEFINED(FPC)}
    function BaseAddCallbacks(const ACallbacks: TList<THorseCallback>): THorseCoreBase; override;
    {$ELSE}
    function BaseAddCallbacks(const ACallbacks: TArray<THorseCallback>): THorseCoreBase; override;
    {$ENDIF}
    function BaseUse(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; override;
    function BaseUse(const ACallback: THorseCallback): THorseCoreBase; overload; override;
    function BaseUse(const APath: string; const ACallbacks: array of THorseCallback): THorseCoreBase; overload; override;
    function BaseUse(const ACallbacks: array of THorseCallback): THorseCoreBase; overload; override;

    function BaseRoute(const APath: string): IInterface; override;

    function BaseAll(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; override;
    function BaseAll(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; override;
    function BaseAll(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; override;
    {$IFNDEF FPC}
    function BaseAll(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; override;
    {$ENDIF}

    function BaseGet(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; override;
    function BaseGet(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; override;
    function BaseGet(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; override;
    {$IFNDEF FPC}
    function BaseGet(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; override;
    {$ENDIF}

    function BasePut(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; override;
    function BasePut(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; override;
    function BasePut(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; override;
    {$IFNDEF FPC}
    function BasePut(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; override;
    {$ENDIF}

    function BaseHead(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; override;
    function BaseHead(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; override;
    function BaseHead(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; override;
    {$IFNDEF FPC}
    function BaseHead(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; override;
    {$ENDIF}

    function BasePost(const APath: string; const ACallback: THorseCallback): THorseCoreBase; override;
    function BasePost(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; override;
    function BasePost(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; override;
    {$IFNDEF FPC}
    function BasePost(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; override;
    {$ENDIF}

    function BaseDelete(const APath: string; const ACallback: THorseCallback): THorseCoreBase; override;
    function BaseDelete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; override;
    function BaseDelete(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; override;
    {$IFNDEF FPC}
    function BaseDelete(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; override;
    {$ENDIF}

    // Patch
    function BasePatch(const APath: string; const ACallback: THorseCallback): THorseCoreBase; override;
    function BasePatch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; override;
    function BasePatch(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; override;
    {$IFNDEF FPC}
    function BasePatch(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; override;
    {$ENDIF}

    // Ganchos de Ciclo de Vida do Servidor & UseStartup
    function UseStartup(const AStartup: IHorseStartup): THorseInstance;
    function AddOnBeforeListen(const ACallback: THorseServerLifecycleProc): THorseInstance; overload;
    function AddOnAfterListen(const ACallback: THorseServerLifecycleProc): THorseInstance; overload;
    function AddOnBeforeStop(const ACallback: THorseServerLifecycleProc): THorseInstance; overload;
    function AddOnAfterStop(const ACallback: THorseServerLifecycleProc): THorseInstance; overload;

    function AddOnBeforeListen(const ACallback: THorseServerLifecycleMethod): THorseInstance; overload;
    function AddOnAfterListen(const ACallback: THorseServerLifecycleMethod): THorseInstance; overload;
    function AddOnBeforeStop(const ACallback: THorseServerLifecycleMethod): THorseInstance; overload;
    function AddOnAfterStop(const ACallback: THorseServerLifecycleMethod): THorseInstance; overload;

    // Controle do ciclo de escuta física delegada
    procedure Listen(const APort: Integer = 9000; const AHost: string = '0.0.0.0'; const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); overload;
    procedure Listen(const APort: Integer; const ACallbackListen: TProc; const ACallbackStopListen: TProc = nil); overload;
    procedure Listen(const AHost: string; const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); overload;
    procedure Listen(const ACallbackListen: TProc; const ACallbackStopListen: TProc = nil); overload;
    procedure StopListen;
    procedure StopListenGraceful(const ATimeoutMS: Integer = 5000);

    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Running: Boolean read FRunning write FRunning;
  end;

threadvar
  GCurrentBuildingInstance: THorseCoreBase;

function ResolveBuildingInstance: THorseCoreBase;

implementation

uses
  Horse.Core.RouterTree,
  Horse.Core.Group,
  Horse.Core.Route,
  Horse;

function ResolveBuildingInstance: THorseCoreBase;
begin
  if GCurrentBuildingInstance <> nil then
    Result := GCurrentBuildingInstance
  else
    Result := THorseCore.GetInstance;
end;

type
  IHorseLifecycleExecutor = interface
    ['{69A45BBE-C54D-4158-9A3E-9457DE85D833}']
    procedure Next;
  end;

  THorseLifecycleExecutor = class(TInterfacedObject, IHorseLifecycleExecutor)
  private
    FCallbacks: TList<THorseCallback>;
    FIndex: Integer;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
    FOnComplete: TProc;
  public
    constructor Create(const ACallbacks: TList<THorseCallback>; const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
    procedure Next;
  end;

{ THorseLifecycleExecutor }

constructor THorseLifecycleExecutor.Create(const ACallbacks: TList<THorseCallback>; const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
begin
  FCallbacks := ACallbacks;
  FIndex := -1;
  FRequest := ARequest;
  FResponse := AResponse;
  FOnComplete := AOnComplete;
end;

procedure THorseLifecycleExecutor.Next;
begin
  if FResponse.Aborted then
    Exit;

  Inc(FIndex);
  if (FCallbacks <> nil) and (FIndex < FCallbacks.Count) then
  begin
    {$IF DEFINED(FPC)}
    THorseCallbackProc(FCallbacks[FIndex])(FRequest, FResponse, Next);
    {$ELSE}
    FCallbacks[FIndex](FRequest, FResponse, Next);
    {$ENDIF}
  end
  else if Assigned(FOnComplete) then
  begin
    FOnComplete();
  end;
end;

{ THorseInstance }

procedure THorseInstance.EmptyNext;
begin
end;

constructor THorseInstance.Create;
begin
  inherited Create;
  FRoutes := THorseRouterTree.Create;
  FHost := '0.0.0.0';
  FPort := 9000;
  FRunning := False;
end;

destructor THorseInstance.Destroy;
begin
  UnregisterHorseInstance(FPort);
  if FCallbacks <> nil then
    FreeAndNil(FCallbacks);
  if FOnRequest <> nil then
    FreeAndNil(FOnRequest);
  if FPreParsing <> nil then
    FreeAndNil(FPreParsing);
  if FPreValidation <> nil then
    FreeAndNil(FPreValidation);
  if FOnSendString <> nil then
    FreeAndNil(FOnSendString);
  if FOnSendBytes <> nil then
    FreeAndNil(FOnSendBytes);
  if FOnResponse <> nil then
    FreeAndNil(FOnResponse);

  if FOnBeforeListen <> nil then
    FreeAndNil(FOnBeforeListen);
  if FOnAfterListen <> nil then
    FreeAndNil(FOnAfterListen);
  if FOnBeforeStop <> nil then
    FreeAndNil(FOnBeforeStop);
  if FOnAfterStop <> nil then
    FreeAndNil(FOnAfterStop);

  if FOnBeforeListenMethod <> nil then
    FreeAndNil(FOnBeforeListenMethod);
  if FOnAfterListenMethod <> nil then
    FreeAndNil(FOnAfterListenMethod);
  if FOnBeforeStopMethod <> nil then
    FreeAndNil(FOnBeforeStopMethod);
  if FOnAfterStopMethod <> nil then
    FreeAndNil(FOnAfterStopMethod);

  inherited;
end;

function THorseInstance.GetRoutes: IHorseRouter;
begin
  Result := FRoutes;
end;

class function THorseInstance.TrimPath(const APath: string): string;
begin
  Result := '/' + APath.Trim(['/']);
end;

function THorseInstance.RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback): THorseInstance;
begin
  Result := Self;
  FRoutes.RegisterRoute(AHTTPType, TrimPath(APath), ACallback);
end;

function THorseInstance.RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback): THorseInstance;
begin
  Result := Self;
  FRoutes.RegisterRouteMiddleware(AHTTPType, TrimPath(APath), ACallback);
end;

function THorseInstance.RegisterCallbacksRoute(const AMethod: TMethodType; const APath: string): THorseInstance;
var
  LCallback: THorseCallback;
  {$IF DEFINED(FPC)}
  LCallbacks: TList<THorseCallback>;
  {$ELSE}
  LCallbacks: TArray<THorseCallback>;
  {$ENDIF}
begin
  Result := Self;
  LCallbacks := GetCallbacks;
  {$IF DEFINED(FPC)}
  try
    if Assigned(LCallbacks) then
    begin
      for LCallback in LCallbacks do
        FRoutes.RegisterRouteMiddleware(AMethod, TrimPath(APath), LCallback);
    end;
  finally
    LCallbacks.Free;
  end;
  {$ELSE}
  for LCallback in LCallbacks do
    FRoutes.RegisterRouteMiddleware(AMethod, TrimPath(APath), LCallback);
  {$ENDIF}
end;

{$IF DEFINED(FPC)}
function THorseInstance.GetCallbacks: TList<THorseCallback>;
begin
  Result := FCallbacks;
  FCallbacks := nil;
end;
{$ELSE}
function THorseInstance.GetCallbacks: TArray<THorseCallback>;
begin
  Result := [];
  if Assigned(FCallbacks) then
  begin
    Result := FCallbacks.ToArray;
    FCallbacks.Clear;
  end;
end;
{$ENDIF}

function THorseInstance.AddCallback(const ACallback: THorseCallback): THorseInstance;
begin
  Result := Self;
  if FCallbacks = nil then
    FCallbacks := TList<THorseCallback>.Create;
  FCallbacks.Add(ACallback);
end;

{$IF DEFINED(FPC)}
function THorseInstance.AddCallbacks(const ACallbacks: TList<THorseCallback>): THorseInstance;
var
  LCallback: THorseCallback;
begin
  if Assigned(ACallbacks) then
  begin
    for LCallback in ACallbacks do
      AddCallback(LCallback);
    ACallbacks.Free;
  end;
  Result := Self;
end;
{$ELSE}
function THorseInstance.AddCallbacks(const ACallbacks: TArray<THorseCallback>): THorseInstance;
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    AddCallback(LCallback);
  Result := Self;
end;
{$ENDIF}

function THorseInstance.Group: IHorseCoreGroup<THorseInstance>;
var
  LOldInstance: THorseCoreBase;
begin
  LOldInstance := GCurrentBuildingInstance;
  GCurrentBuildingInstance := Self;
  try
    Result := THorseCoreGroup<THorseInstance>.Create;
  finally
    GCurrentBuildingInstance := LOldInstance;
  end;
end;

function THorseInstance.Route(const APath: string): IHorseCoreRoute<THorseInstance>;
var
  LOldInstance: THorseCoreBase;
begin
  LOldInstance := GCurrentBuildingInstance;
  GCurrentBuildingInstance := Self;
  try
    Result := THorseCoreRoute<THorseInstance>.Create(APath);
  finally
    GCurrentBuildingInstance := LOldInstance;
  end;
end;

procedure THorseInstance.OnError(const ACallback: THorseOnError);
begin
  FOnError := ACallback;
end;

function THorseInstance.HasOnError: Boolean;
begin
  Result := Assigned(FOnError);
end;

procedure THorseInstance.ExecuteOnError(const ARequest: THorseRequest; const AResponse: THorseResponse; const AException: Exception);
begin
  if Assigned(FOnError) then
    FOnError(ARequest, AResponse, AException);
end;

procedure THorseInstance.AddOnRequest(const ACallback: THorseCallback);
begin
  if FOnRequest = nil then
    FOnRequest := TList<THorseCallback>.Create;
  FOnRequest.Add(ACallback);
end;

procedure THorseInstance.AddPreParsing(const ACallback: THorseCallback);
begin
  if FPreParsing = nil then
    FPreParsing := TList<THorseCallback>.Create;
  FPreParsing.Add(ACallback);
end;

procedure THorseInstance.AddPreValidation(const ACallback: THorseCallback);
begin
  if FPreValidation = nil then
    FPreValidation := TList<THorseCallback>.Create;
  FPreValidation.Add(ACallback);
end;

procedure THorseInstance.AddOnSend(const ACallback: THorseOnSendString);
begin
  if FOnSendString = nil then
    FOnSendString := TList<THorseOnSendString>.Create;
  FOnSendString.Add(ACallback);
end;

procedure THorseInstance.AddOnSend(const ACallback: THorseOnSendBytes);
begin
  if FOnSendBytes = nil then
    FOnSendBytes := TList<THorseOnSendBytes>.Create;
  FOnSendBytes.Add(ACallback);
end;

procedure THorseInstance.AddOnResponse(const ACallback: THorseCallback);
begin
  if FOnResponse = nil then
    FOnResponse := TList<THorseCallback>.Create;
  FOnResponse.Add(ACallback);
end;

procedure THorseInstance.ResetHooks;
begin
  if FOnRequest <> nil then
    FOnRequest.Clear;
  if FPreParsing <> nil then
    FPreParsing.Clear;
  if FPreValidation <> nil then
    FPreValidation.Clear;
  if FOnSendString <> nil then
    FOnSendString.Clear;
  if FOnSendBytes <> nil then
    FOnSendBytes.Clear;
  if FOnResponse <> nil then
    FOnResponse.Clear;
  FOnError := nil;
end;

function THorseInstance.GetActiveRequests: Integer;
begin
  Result := FActiveRequests;
end;

procedure THorseInstance.IncrementActiveRequests;
begin
  {$IF DEFINED(FPC)}
  InterlockedIncrement(FActiveRequests);
  {$ELSE}
  TInterlocked.Increment(FActiveRequests);
  {$ENDIF}
end;

procedure THorseInstance.DecrementActiveRequests;
begin
  {$IF DEFINED(FPC)}
  InterlockedDecrement(FActiveRequests);
  {$ELSE}
  TInterlocked.Decrement(FActiveRequests);
  {$ENDIF}
end;

procedure THorseInstance.DoIncrementActiveRequests;
begin
  IncrementActiveRequests;
end;

procedure THorseInstance.DoDecrementActiveRequests;
begin
  DecrementActiveRequests;
end;

function THorseInstance.GetIsShuttingDown: Boolean;
begin
  Result := FIsShuttingDown;
end;

procedure THorseInstance.SetIsShuttingDown(const AValue: Boolean);
begin
  FIsShuttingDown := AValue;
end;

procedure THorseInstance.ExecuteOnRequest(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
var
  LExecutor: IHorseLifecycleExecutor;
begin
  if Assigned(ARequest) and (FOnRequest <> nil) and (FOnRequest.Count > 0) then
  begin
    LExecutor := THorseLifecycleExecutor.Create(FOnRequest, ARequest, AResponse, AOnComplete);
    LExecutor.Next;
  end
  else
    AOnComplete();
end;

procedure THorseInstance.ExecutePreParsing(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
var
  LExecutor: IHorseLifecycleExecutor;
begin
  if Assigned(ARequest) and (FPreParsing <> nil) and (FPreParsing.Count > 0) then
  begin
    LExecutor := THorseLifecycleExecutor.Create(FPreParsing, ARequest, AResponse, AOnComplete);
    LExecutor.Next;
  end
  else
    AOnComplete();
end;

procedure THorseInstance.ExecutePreValidation(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
var
  LExecutor: IHorseLifecycleExecutor;
begin
  if Assigned(ARequest) and (FPreValidation <> nil) and (FPreValidation.Count > 0) then
  begin
    LExecutor := THorseLifecycleExecutor.Create(FPreValidation, ARequest, AResponse, AOnComplete);
    LExecutor.Next;
  end
  else
    AOnComplete();
end;

procedure THorseInstance.ExecuteOnSend(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: string);
var
  LHook: THorseOnSendString;
begin
  if Assigned(ARequest) and (FOnSendString <> nil) then
  begin
    for LHook in FOnSendString do
      LHook(ARequest, AResponse, AContent);
  end;
end;

procedure THorseInstance.ExecuteOnSend(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: TBytes);
var
  LHook: THorseOnSendBytes;
begin
  if Assigned(ARequest) and (FOnSendBytes <> nil) then
  begin
    for LHook in FOnSendBytes do
      LHook(ARequest, AResponse, AContent);
  end;
end;

procedure THorseInstance.ExecuteOnResponse(const ARequest: THorseRequest; const AResponse: THorseResponse);
var
  LCallback: THorseCallback;
begin
  if Assigned(ARequest) and (FOnResponse <> nil) then
  begin
    for LCallback in FOnResponse do
    begin
      try
        {$IF DEFINED(FPC)}
        THorseCallbackProc(LCallback)(ARequest, AResponse, EmptyNext);
        {$ELSE}
        LCallback(ARequest, AResponse, procedure begin end);
        {$ENDIF}
      except
      end;
    end;
  end;
end;

// Roteamento
function THorseInstance.Use(const APath: string; const ACallback: THorseCallback): THorseInstance;
begin
  Result := Self;
  FRoutes.RegisterMiddleware(TrimPath(APath), ACallback);
end;

function THorseInstance.Use(const ACallback: THorseCallback): THorseInstance;
begin
  Result := Self;
  FRoutes.RegisterMiddleware('/', ACallback);
end;

function THorseInstance.Use(const APath: string; const ACallbacks: array of THorseCallback): THorseInstance;
var
  LCallback: THorseCallback;
begin
  Result := Self;
  for LCallback in ACallbacks do
    Use(APath, LCallback);
end;

function THorseInstance.Use(const ACallbacks: array of THorseCallback): THorseInstance;
var
  LCallback: THorseCallback;
begin
  Result := Self;
  for LCallback in ACallbacks do
    Use(LCallback);
end;

function THorseInstance.All(const APath: string; const ACallback: THorseCallback): THorseInstance;
var
  LMethodType: TMethodType;
begin
  Result := Self;
  for LMethodType := Low(TMethodType) to High(TMethodType) do
  begin
    RegisterCallbacksRoute(LMethodType, APath);
    RegisterRoute(LMethodType, APath, ACallback);
  end;
end;

function THorseInstance.All(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance;
begin
  Result := All(APath, GetCallback(ACallback));
end;

function THorseInstance.All(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance;
begin
  Result := All(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
function THorseInstance.All(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance;
begin
  Result := All(APath, GetCallback(ACallback));
end;
{$ENDIF}

// Get
function THorseInstance.Get(const APath: string; const ACallback: THorseCallback): THorseInstance;
begin
  Result := RegisterCallbacksRoute(mtGet, APath);
  RegisterRoute(mtGet, APath, ACallback);
end;

function THorseInstance.Get(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance;
begin
  Result := Get(APath, GetCallback(ACallback));
end;

function THorseInstance.Get(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance;
begin
  Result := Get(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
function THorseInstance.Get(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance;
begin
  Result := Get(APath, GetCallback(ACallback));
end;
{$ENDIF}

// Put
function THorseInstance.Put(const APath: string; const ACallback: THorseCallback): THorseInstance;
begin
  Result := RegisterCallbacksRoute(mtPut, APath);
  RegisterRoute(mtPut, APath, ACallback);
end;

function THorseInstance.Put(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance;
begin
  Result := Put(APath, GetCallback(ACallback));
end;

function THorseInstance.Put(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance;
begin
  Result := Put(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
function THorseInstance.Put(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance;
begin
  Result := Put(APath, GetCallback(ACallback));
end;
{$ENDIF}

// Post
function THorseInstance.Post(const APath: string; const ACallback: THorseCallback): THorseInstance;
begin
  Result := RegisterCallbacksRoute(mtPost, APath);
  RegisterRoute(mtPost, APath, ACallback);
end;

function THorseInstance.Post(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance;
begin
  Result := Post(APath, GetCallback(ACallback));
end;

function THorseInstance.Post(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance;
begin
  Result := Post(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
function THorseInstance.Post(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance;
begin
  Result := Post(APath, GetCallback(ACallback));
end;
{$ENDIF}

// Delete
function THorseInstance.Delete(const APath: string; const ACallback: THorseCallback): THorseInstance;
begin
  Result := RegisterCallbacksRoute(mtDelete, APath);
  RegisterRoute(mtDelete, APath, ACallback);
end;

function THorseInstance.Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance;
begin
  Result := Delete(APath, GetCallback(ACallback));
end;

function THorseInstance.Delete(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance;
begin
  Result := Delete(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
function THorseInstance.Delete(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance;
begin
  Result := Delete(APath, GetCallback(ACallback));
end;
{$ENDIF}

// Patch
function THorseInstance.Patch(const APath: string; const ACallback: THorseCallback): THorseInstance;
begin
  Result := RegisterCallbacksRoute(mtPatch, APath);
  RegisterRoute(mtPatch, APath, ACallback);
end;

function THorseInstance.Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance;
begin
  Result := Patch(APath, GetCallback(ACallback));
end;

function THorseInstance.Patch(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance;
begin
  Result := Patch(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
function THorseInstance.Patch(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance;
begin
  Result := Patch(APath, GetCallback(ACallback));
end;
{$ENDIF}

// Head
function THorseInstance.Head(const APath: string; const ACallback: THorseCallback): THorseInstance;
begin
  Result := RegisterCallbacksRoute(mtHead, APath);
  RegisterRoute(mtHead, APath, ACallback);
end;

function THorseInstance.Head(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseInstance;
begin
  Result := Head(APath, GetCallback(ACallback));
end;

function THorseInstance.Head(const APath: string; const ACallback: THorseCallbackRequest): THorseInstance;
begin
  Result := Head(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
function THorseInstance.Head(const APath: string; const ACallback: THorseCallbackResponse): THorseInstance;
begin
  Result := Head(APath, GetCallback(ACallback));
end;
{$ENDIF}

// Sobrecargas com middlewares
function THorseInstance.Get(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtGet, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtGet, APath, LMiddleware);
  RegisterRoute(mtGet, APath, ACallback);
end;

function THorseInstance.Put(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtPut, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtPut, APath, LMiddleware);
  RegisterRoute(mtPut, APath, ACallback);
end;

function THorseInstance.Post(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtPost, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtPost, APath, LMiddleware);
  RegisterRoute(mtPost, APath, ACallback);
end;

function THorseInstance.Delete(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtDelete, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtDelete, APath, LMiddleware);
  RegisterRoute(mtDelete, APath, ACallback);
end;

function THorseInstance.Patch(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtPatch, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtPatch, APath, LMiddleware);
  RegisterRoute(mtPatch, APath, ACallback);
end;

// Head overload
function THorseInstance.Head(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseInstance;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtHead, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtHead, APath, LMiddleware);
  RegisterRoute(mtHead, APath, ACallback);
end;

// Métodos virtuais de base herdados de THorseCoreBase
function THorseInstance.BaseAddCallback(const ACallback: THorseCallback): THorseCoreBase;
begin
  AddCallback(ACallback);
  Result := Self;
end;

{$IF DEFINED(FPC)}
function THorseInstance.BaseAddCallbacks(const ACallbacks: TList<THorseCallback>): THorseCoreBase;
begin
  AddCallbacks(ACallbacks);
  Result := Self;
end;
{$ELSE}
function THorseInstance.BaseAddCallbacks(const ACallbacks: TArray<THorseCallback>): THorseCoreBase;
begin
  AddCallbacks(ACallbacks);
  Result := Self;
end;
{$ENDIF}

function THorseInstance.BaseUse(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  Use(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BaseUse(const ACallback: THorseCallback): THorseCoreBase;
begin
  Use(ACallback);
  Result := Self;
end;

function THorseInstance.BaseUse(const APath: string; const ACallbacks: array of THorseCallback): THorseCoreBase;
begin
  Use(APath, ACallbacks);
  Result := Self;
end;

function THorseInstance.BaseUse(const ACallbacks: array of THorseCallback): THorseCoreBase;
begin
  Use(ACallbacks);
  Result := Self;
end;

function THorseInstance.BaseRoute(const APath: string): IInterface;
begin
  Result := Route(APath);
end;

function THorseInstance.BaseAll(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  All(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BaseAll(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  All(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BaseAll(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  All(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseInstance.BaseAll(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  All(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseInstance.BaseGet(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  Get(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BaseGet(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  Get(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BaseGet(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  Get(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseInstance.BaseGet(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  Get(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseInstance.BasePut(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  Put(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BasePut(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  Put(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BasePut(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  Put(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseInstance.BasePut(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  Put(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseInstance.BaseHead(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  Head(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BaseHead(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  Head(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BaseHead(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  Head(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseInstance.BaseHead(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  Head(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseInstance.BasePost(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  Post(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BasePost(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  Post(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BasePost(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  Post(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseInstance.BasePost(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  Post(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseInstance.BaseDelete(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  Delete(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BaseDelete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  Delete(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BaseDelete(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  Delete(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseInstance.BaseDelete(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  Delete(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseInstance.BasePatch(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  Patch(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BasePatch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  Patch(APath, ACallback);
  Result := Self;
end;

function THorseInstance.BasePatch(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  Patch(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseInstance.BasePatch(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  Patch(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseInstance.UseStartup(const AStartup: IHorseStartup): THorseInstance;
begin
  Result := Self;
  if AStartup <> nil then
    AStartup.Configure(Self);
end;

function THorseInstance.AddOnBeforeListen(const ACallback: THorseServerLifecycleProc): THorseInstance;
begin
  Result := Self;
  if FOnBeforeListen = nil then
    FOnBeforeListen := TList<THorseServerLifecycleProc>.Create;
  FOnBeforeListen.Add(ACallback);
end;

function THorseInstance.AddOnAfterListen(const ACallback: THorseServerLifecycleProc): THorseInstance;
begin
  Result := Self;
  if FOnAfterListen = nil then
    FOnAfterListen := TList<THorseServerLifecycleProc>.Create;
  FOnAfterListen.Add(ACallback);
end;

function THorseInstance.AddOnBeforeStop(const ACallback: THorseServerLifecycleProc): THorseInstance;
begin
  Result := Self;
  if FOnBeforeStop = nil then
    FOnBeforeStop := TList<THorseServerLifecycleProc>.Create;
  FOnBeforeStop.Add(ACallback);
end;

function THorseInstance.AddOnAfterStop(const ACallback: THorseServerLifecycleProc): THorseInstance;
begin
  Result := Self;
  if FOnAfterStop = nil then
    FOnAfterStop := TList<THorseServerLifecycleProc>.Create;
  FOnAfterStop.Add(ACallback);
end;

function THorseInstance.AddOnBeforeListen(const ACallback: THorseServerLifecycleMethod): THorseInstance;
begin
  Result := Self;
  if FOnBeforeListenMethod = nil then
    FOnBeforeListenMethod := TList<THorseServerLifecycleMethod>.Create;
  FOnBeforeListenMethod.Add(ACallback);
end;

function THorseInstance.AddOnAfterListen(const ACallback: THorseServerLifecycleMethod): THorseInstance;
begin
  Result := Self;
  if FOnAfterListenMethod = nil then
    FOnAfterListenMethod := TList<THorseServerLifecycleMethod>.Create;
  FOnAfterListenMethod.Add(ACallback);
end;

function THorseInstance.AddOnBeforeStop(const ACallback: THorseServerLifecycleMethod): THorseInstance;
begin
  Result := Self;
  if FOnBeforeStopMethod = nil then
    FOnBeforeStopMethod := TList<THorseServerLifecycleMethod>.Create;
  FOnBeforeStopMethod.Add(ACallback);
end;

function THorseInstance.AddOnAfterStop(const ACallback: THorseServerLifecycleMethod): THorseInstance;
begin
  Result := Self;
  if FOnAfterStopMethod = nil then
    FOnAfterStopMethod := TList<THorseServerLifecycleMethod>.Create;
  FOnAfterStopMethod.Add(ACallback);
end;

procedure THorseInstance.TriggerBeforeListen;
var
  LCallback: THorseServerLifecycleProc;
  LMethod: THorseServerLifecycleMethod;
begin
  if FOnBeforeListen <> nil then
  begin
    for LCallback in FOnBeforeListen do
      LCallback(Self);
  end;
  if FOnBeforeListenMethod <> nil then
  begin
    for LMethod in FOnBeforeListenMethod do
      LMethod(Self);
  end;
end;

procedure THorseInstance.TriggerAfterListen;
var
  LCallback: THorseServerLifecycleProc;
  LMethod: THorseServerLifecycleMethod;
begin
  if FOnAfterListen <> nil then
  begin
    for LCallback in FOnAfterListen do
      LCallback(Self);
  end;
  if FOnAfterListenMethod <> nil then
  begin
    for LMethod in FOnAfterListenMethod do
      LMethod(Self);
  end;
end;

procedure THorseInstance.TriggerBeforeStop;
var
  LCallback: THorseServerLifecycleProc;
  LMethod: THorseServerLifecycleMethod;
begin
  if FOnBeforeStop <> nil then
  begin
    for LCallback in FOnBeforeStop do
      LCallback(Self);
  end;
  if FOnBeforeStopMethod <> nil then
  begin
    for LMethod in FOnBeforeStopMethod do
      LMethod(Self);
  end;
end;

procedure THorseInstance.TriggerAfterStop;
var
  LCallback: THorseServerLifecycleProc;
  LMethod: THorseServerLifecycleMethod;
begin
  if FOnAfterStop <> nil then
  begin
    for LCallback in FOnAfterStop do
      LCallback(Self);
  end;
  if FOnAfterStopMethod <> nil then
  begin
    for LMethod in FOnAfterStopMethod do
      LMethod(Self);
  end;
end;

// Callback converters
{$IF DEFINED(FPC)}
function THorseInstance.GetCallback(const ACallbackRequest: THorseCallbackRequestResponse): THorseCallback;
begin
  Result := THorseCallback(ACallbackRequest);
end;

function THorseInstance.GetCallback(const ACallbackRequest: THorseCallbackRequest): THorseCallback;
begin
  Result := THorseCallback(ACallbackRequest);
end;
{$ELSE}
function THorseInstance.GetCallback(const ACallbackRequest: THorseCallbackRequestResponse): THorseCallback;
begin
  Result := procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc)
    begin
      ACallbackRequest(AReq, ARes);
      ANext();
    end;
end;

function THorseInstance.GetCallback(const ACallbackRequest: THorseCallbackRequest): THorseCallback;
begin
  Result := procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc)
    begin
      ACallbackRequest(AReq);
      ANext();
    end;
end;

function THorseInstance.GetCallback(const ACallbackResponse: THorseCallbackResponse): THorseCallback;
begin
  Result := procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc)
    begin
      ACallbackResponse(ARes);
      ANext();
    end;
end;
{$ENDIF}

// Controle de Escuta física delegando para a fachada THorse
procedure THorseInstance.Listen(const APort: Integer; const AHost: string; const ACallbackListen: TProc; const ACallbackStopListen: TProc);
begin
  FPort := APort;
  FHost := AHost;
  RegisterHorseInstance(APort, Self);
  FRunning := True;
  try
    THorseProvider.Listen(APort, AHost, ACallbackListen, ACallbackStopListen);
  except
    FRunning := False;
    raise;
  end;
end;

procedure THorseInstance.Listen(const APort: Integer; const ACallbackListen: TProc; const ACallbackStopListen: TProc);
begin
  Listen(APort, '0.0.0.0', ACallbackListen, ACallbackStopListen);
end;

procedure THorseInstance.Listen(const AHost: string; const ACallbackListen: TProc; const ACallbackStopListen: TProc);
begin
  Listen(9000, AHost, ACallbackListen, ACallbackStopListen);
end;

procedure THorseInstance.Listen(const ACallbackListen: TProc; const ACallbackStopListen: TProc);
begin
  Listen(9000, '0.0.0.0', ACallbackListen, ACallbackStopListen);
end;

procedure THorseInstance.StopListen;
begin
  THorseProvider.StopListen;
  UnregisterHorseInstance(FPort);
  FRunning := False;
end;

procedure THorseInstance.StopListenGraceful(const ATimeoutMS: Integer);
begin
  SetIsShuttingDown(True);
  try
    StopListen;
  finally
    SetIsShuttingDown(False);
  end;
end;

initialization
  GetHorseCoreInstance := @ResolveBuildingInstance;

end.
