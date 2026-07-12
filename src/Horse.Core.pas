unit Horse.Core;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Generics.Collections,
  SyncObjs,
{$ELSE}
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs,
  Web.HTTPApp,
{$ENDIF}
  Horse.Core.RouterTree,
  Horse.Callback,
  Horse.Proc,
  Horse.Core.Group.Contract,
  Horse.Core.Route.Contract,
  Horse.Commons,
  Horse.Core.Router.Contract,
  Horse.Core.Base,
  Horse.Request,
  Horse.Response;

type
  THorseOnError = procedure(const ARequest: THorseRequest; const AResponse: THorseResponse; const AException: Exception);
  {$IF DEFINED(FPC)}
  THorseOnSendString = procedure(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: string);
  THorseOnSendBytes = procedure(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: TBytes);
  THorseOnTelemetry = procedure(const ARequest: THorseRequest; const AResponse: THorseResponse; const AExecutionTimeMS: Double);
  {$ELSE}
  THorseOnSendString = reference to procedure(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: string);
  THorseOnSendBytes = reference to procedure(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: TBytes);
  THorseOnTelemetry = reference to procedure(const ARequest: THorseRequest; const AResponse: THorseResponse; const AExecutionTimeMS: Double);
  {$ENDIF}

  THorseCore = class;
  PHorseCore = ^THorseCore;
  PHorseModule = ^THorseModule;

  THorseModule = record
  private
    FSelfInstance: PHorseCore;
    FDefaultHorseCoreInstance: PHorseCore;
    FHorseRouter: PHorseRouter;
    function GetSelfInstance: PHorseCore;
    function GetDefaultHorseCoreInstance: PHorseCore;
    function GetHorseRouter: PHorseRouter;
  public
    function ToHorse: THorseCore;
    constructor Create(const ASelfInstance, ADefaultHorseCoreInstance: PHorseCore; const AHorseRouter: PHorseRouter);
  end;

  THorseCore = class(THorseCoreBase)
  private
    class var FRoutes: IHorseRouter;
    class var FCallbacks: TList<THorseCallback>;
    class var FOnRequest: TList<THorseCallback>;
    class var FPreParsing: TList<THorseCallback>;
    class var FPreValidation: TList<THorseCallback>;
    class var FOnSendString: TList<THorseOnSendString>;
    class var FOnSendBytes: TList<THorseOnSendBytes>;
    class var FOnResponse: TList<THorseCallback>;
    class var FOnTelemetry: TList<THorseOnTelemetry>;
    class function TrimPath(const APath: string): string;
    class function RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback): THorseCore;
    class function RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback): THorseCore;
    class var FDefaultHorse: THorseCore;
    class var FOnError: THorseOnError;
    class var FActiveRequests: Integer;
    class var FIsShuttingDown: Boolean;

    function InternalRoute(const APath: string): IHorseCoreRoute<THorseCore>;
    function InternalGroup: IHorseCoreGroup<THorseCore>;
    function InternalGetRoutes: IHorseRouter;
    procedure InternalSetRoutes(const AValue: IHorseRouter);
    class function GetStaticRoutes: IHorseRouter; static;
    class procedure SetStaticRoutes(const AValue: IHorseRouter); static;
    class function MakeHorseModule: THorseModule;

    class function GetCallback(const ACallbackRequest: THorseCallbackRequestResponse): THorseCallback; overload;
    class function GetCallback(const ACallbackRequest: THorseCallbackRequest): THorseCallback; overload;
{$IFNDEF FPC}
    class function GetCallback(const ACallbackResponse: THorseCallbackResponse): THorseCallback; overload;
{$ENDIF}
    {$IF DEFINED(FPC)}
    class function GetCallbacks: TList<THorseCallback>;
    {$ELSE}
    class function GetCallbacks: TArray<THorseCallback>;
    {$ENDIF}
    class function RegisterCallbacksRoute(const AMethod: TMethodType; const APath: string): THorseCore;
    procedure EmptyNext;
  public
    constructor Create; virtual;
    class function ToModule: THorseModule;
    class destructor UnInitialize; {$IFNDEF FPC}virtual; {$ENDIF}
    class function AddCallback(const ACallback: THorseCallback): THorseCore; overload;
    {$IF DEFINED(FPC)}
    class function AddCallbacks(const ACallbacks: TList<THorseCallback>): THorseCore; overload;
    {$ELSE}
    class function AddCallbacks(const ACallbacks: TArray<THorseCallback>): THorseCore; overload;
    {$ENDIF}

    class function Group: IHorseCoreGroup<THorseCore>;
    class function Route(const APath: string): IHorseCoreRoute<THorseCore>; overload;

    class procedure OnError(const ACallback: THorseOnError); static;
    class function HasOnError: Boolean; static;
    class procedure ExecuteOnError(const ARequest: THorseRequest; const AResponse: THorseResponse; const AException: Exception); static;

    class procedure AddOnRequest(const ACallback: THorseCallback); static;
    class procedure AddPreParsing(const ACallback: THorseCallback); static;
    class procedure AddPreValidation(const ACallback: THorseCallback); static;
    class procedure AddOnSend(const ACallback: THorseOnSendString); overload; static;
    class procedure AddOnSend(const ACallback: THorseOnSendBytes); overload; static;
    class procedure AddOnResponse(const ACallback: THorseCallback); static;
    class procedure AddOnTelemetry(const ACallback: THorseOnTelemetry); static;
    class procedure ResetHooks; static;

    class procedure ExecuteOnTelemetry(const ARequest: THorseRequest; const AResponse: THorseResponse; const AExecutionTimeMS: Double); static;

    class function GetActiveRequests: Integer; static;
    class procedure IncrementActiveRequests; static;
    class procedure DecrementActiveRequests; static;
    class function GetIsShuttingDown: Boolean; static;
    class procedure SetIsShuttingDown(const AValue: Boolean); static;

    class procedure ExecuteOnRequest(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc); static;
    class procedure ExecutePreParsing(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc); static;
    class procedure ExecutePreValidation(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc); static;
    class procedure ExecuteOnSend(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: string); overload; static;
    class procedure ExecuteOnSend(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: TBytes); overload; static;
    class procedure ExecuteOnResponse(const ARequest: THorseRequest; const AResponse: THorseResponse); static;

    class function Use(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    class function Use(const ACallback: THorseCallback): THorseCore; overload;
    class function Use(const APath: string; const ACallbacks: array of THorseCallback): THorseCore; overload;
    class function Use(const ACallbacks: array of THorseCallback): THorseCore; overload;

    class function All(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    class function All(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function All(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function All(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}
    class function Get(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    class function Get(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Get(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Get(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}
    class function Put(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    class function Put(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Put(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Put(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}
{$IF (DEFINED(FPC) or (CompilerVersion > 27.0))}
    class function Patch(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    class function Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Patch(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Patch(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}
{$IFEND}
    class function Head(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    class function Head(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Head(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Head(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}
    class function Post(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    class function Post(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Post(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Post(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}
{$IF (defined(fpc) or (CompilerVersion > 27.0))}
    class function Delete(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    class function Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Delete(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Delete(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}
{$IFEND}
    class function Query(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    class function Query(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Query(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Query(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}

    class function All(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore; overload;
    class function All(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function All(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function All(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}

    class function Get(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore; overload;
    class function Get(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Get(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Get(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}

    class function Put(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore; overload;
    class function Put(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Put(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Put(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}

{$IF (DEFINED(FPC) or (CompilerVersion > 27.0))}
    class function Patch(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore; overload;
    class function Patch(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Patch(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Patch(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}
{$IFEND}

    class function Head(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore; overload;
    class function Head(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Head(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Head(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}

    class function Post(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore; overload;
    class function Post(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Post(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Post(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
    class function Delete(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore; overload;
    class function Delete(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Delete(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Delete(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}
{$IFEND}

    class function Query(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore; overload;
    class function Query(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Query(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore; overload;
{$IFNDEF FPC}
    class function Query(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore; overload;
{$IFEND}

    class property Routes: IHorseRouter read GetStaticRoutes write SetStaticRoutes;
    class function GetInstance: THorseCore;
    class function Version: string;
    function GetRoutes: IHorseRouter; override;
    procedure DoIncrementActiveRequests; override;
    procedure DoDecrementActiveRequests; override;

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

    function BasePost(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; override;
    function BasePost(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; override;
    function BasePost(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; override;
    {$IFNDEF FPC}
    function BasePost(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; override;
    {$ENDIF}

    function BaseDelete(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; override;
    function BaseDelete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; override;
    function BaseDelete(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; override;
    {$IFNDEF FPC}
    function BaseDelete(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; override;
    {$ENDIF}

    function BasePatch(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; override;
    function BasePatch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; override;
    function BasePatch(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; override;
    {$IFNDEF FPC}
    function BasePatch(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; override;
    {$ENDIF}
  end;

implementation

uses
  Horse.Core.Route,
  Horse.Core.Group,
  Horse.Constants,
  Horse.Instance
  {$IFNDEF FPC}
  , Horse.Core.Factory
  {$ENDIF}
  ;

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

{$I Horse.Core.Wrappers.inc}

class function THorseCore.AddCallback(const ACallback: THorseCallback): THorseCore;
begin
  Result := GetInstance;
  if FCallbacks = nil then
    FCallbacks := TList<THorseCallback>.Create;
  FCallbacks.Add(ACallback);
end;

{$IF DEFINED(FPC)}
class function THorseCore.AddCallbacks(const ACallbacks: TList<THorseCallback>): THorseCore;
var
  LCallback: THorseCallback;
begin
  if Assigned(ACallbacks) then
  begin
    for LCallback in ACallbacks do
      AddCallback(LCallback);
    ACallbacks.Free;
  end;
  Result := GetInstance;
end;
{$ELSE}
class function THorseCore.AddCallbacks(const ACallbacks: TArray<THorseCallback>): THorseCore;
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    AddCallback(LCallback);
  Result := GetInstance;
end;
{$ENDIF}

procedure THorseCore.EmptyNext;
begin
end;

constructor THorseCore.Create;
begin
  if FDefaultHorse <> nil then
    raise Exception.Create('The Horse instance has already been created');
  if FRoutes = nil then
    FRoutes := THorseRouterTree.Create;
  FDefaultHorse := Self;
end;

class function THorseCore.GetInstance: THorseCore;
begin
  if FDefaultHorse = nil then
    FDefaultHorse := THorseCore.Create;
  Result := FDefaultHorse;
end;

{$IF DEFINED(FPC)}
class function THorseCore.GetCallbacks: TList<THorseCallback>;
begin
  Result := FCallbacks;
  FCallbacks := nil;
end;
{$ELSE}
class function THorseCore.GetCallbacks: TArray<THorseCallback>;
begin
  Result := [];
  if Assigned(FCallbacks) then
  begin
    Result := FCallbacks.ToArray;
    FCallbacks.Clear;
  end;
end;
{$ENDIF}

class function THorseCore.RegisterCallbacksRoute(const AMethod: TMethodType; const APath: string): THorseCore;
var
  LCallback: THorseCallback;
  {$IF DEFINED(FPC)}
  LCallbacks: TList<THorseCallback>;
  {$ELSE}
  LCallbacks: TArray<THorseCallback>;
  {$ENDIF}
begin
  Result := GetInstance;
  LCallbacks := GetCallbacks;
  {$IF DEFINED(FPC)}
  try
    if Assigned(LCallbacks) then
    begin
      for LCallback in LCallbacks do
        Result.GetRoutes.RegisterRouteMiddleware(AMethod, TrimPath(APath), LCallback);
    end;
  finally
    LCallbacks.Free;
  end;
  {$ELSE}
  for LCallback in LCallbacks do
    Result.GetRoutes.RegisterRouteMiddleware(AMethod, TrimPath(APath), LCallback);
  {$ENDIF}
end;

class function THorseCore.GetStaticRoutes: IHorseRouter;
begin
  Result := GetInstance.InternalGetRoutes;
end;

class function THorseCore.Group: IHorseCoreGroup<THorseCore>;
begin
  Result := GetInstance.InternalGroup();
end;

class function THorseCore.RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback): THorseCore;
var
  LDefaultHorse: THorseCore;
begin
  LDefaultHorse := GetInstance;
  Result := LDefaultHorse;
  LDefaultHorse.GetRoutes.RegisterRoute(AHTTPType, TrimPath(APath), ACallback);
end;

class function THorseCore.RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback): THorseCore;
var
  LDefaultHorse: THorseCore;
begin
  LDefaultHorse := GetInstance;
  Result := LDefaultHorse;
  LDefaultHorse.GetRoutes.RegisterRouteMiddleware(AHTTPType, TrimPath(APath), ACallback);
end;

class function THorseCore.Route(const APath: string): IHorseCoreRoute<THorseCore>;
begin
  Result := GetInstance.InternalRoute(APath);
end;

class procedure THorseCore.SetStaticRoutes(const AValue: IHorseRouter);
begin
  GetInstance.InternalSetRoutes(AValue);
end;

class function THorseCore.ToModule: THorseModule;
begin
  Result := GetInstance.MakeHorseModule;
end;

class function THorseCore.TrimPath(const APath: string): string;
begin
  Result := '/' + APath.Trim(['/']);
end;

function THorseCore.GetRoutes: IHorseRouter;
begin
  Result := FRoutes;
end;

procedure THorseCore.DoIncrementActiveRequests;
begin
  THorseCore.IncrementActiveRequests;
end;

procedure THorseCore.DoDecrementActiveRequests;
begin
  THorseCore.DecrementActiveRequests;
end;

function THorseCore.InternalGetRoutes: IHorseRouter;
begin
  Result := FRoutes;
end;

function THorseCore.InternalGroup: IHorseCoreGroup<THorseCore>;
begin
  {$IF DEFINED(FPC)}
  Result := THorseCoreGroup<THorseCore>.Create;
  {$ELSE}
  Result := CreateHorseCoreGroup;
  {$ENDIF}
end;

function THorseCore.InternalRoute(const APath: string): IHorseCoreRoute<THorseCore>;
begin
  {$IF DEFINED(FPC)}
  Result := THorseCoreRoute<THorseCore>.Create(APath);
  {$ELSE}
  Result := CreateHorseCoreRoute(APath);
  {$ENDIF}
end;

procedure THorseCore.InternalSetRoutes(const AValue: IHorseRouter);
begin
  FRoutes := AValue;
end;

class function THorseCore.MakeHorseModule: THorseModule;
begin
  Result := THorseModule.Create(@Self, @FDefaultHorse, @FRoutes);
end;

class destructor THorseCore.UnInitialize;
begin
  if FDefaultHorse <> nil then
    FreeAndNil(FDefaultHorse);
  FRoutes := nil;
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
  if FOnTelemetry <> nil then
    FreeAndNil(FOnTelemetry);
end;
{$IF (defined(fpc) or (CompilerVersion > 27.0))}

class function THorseCore.Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Delete(APath, GetCallback(ACallback));
end;

class function THorseCore.Delete(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Delete(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Delete(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Delete(APath, GetCallback(ACallback));
end;
{$IFEND}

class function THorseCore.Delete(const APath: string; const ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterCallbacksRoute(mtDelete, APath);
  RegisterRoute(mtDelete, APath, ACallback);
end;
{$IFEND}

class function THorseCore.Query(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Query(APath, GetCallback(ACallback));
end;

class function THorseCore.Query(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Query(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Query(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Query(APath, GetCallback(ACallback));
end;
{$IFEND}

class function THorseCore.Query(const APath: string; const ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterCallbacksRoute(mtQuery, APath);
  RegisterRoute(mtQuery, APath, ACallback);
end;

class function THorseCore.Head(const APath: string; const ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterCallbacksRoute(mtHead, APath);
  RegisterRoute(mtHead, APath, ACallback);
end;

class function THorseCore.Get(const APath: string; const ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterCallbacksRoute(mtGet, APath);
  RegisterRoute(mtGet, APath, ACallback);
end;

class function THorseCore.Post(const APath: string; const ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterCallbacksRoute(mtPost, APath);
  RegisterRoute(mtPost, APath, ACallback);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
class function THorseCore.Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Patch(APath, GetCallback(ACallback));
end;

class function THorseCore.Patch(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Patch(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Patch(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Patch(APath, GetCallback(ACallback));
end;
{$IFEND}
class function THorseCore.Patch(const APath: string; const ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterCallbacksRoute(mtPatch, APath);
  RegisterRoute(mtPatch, APath, ACallback);
end;
{$IFEND}

class function THorseCore.Put(const APath: string; const ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterCallbacksRoute(mtPut, APath);
  RegisterRoute(mtPut, APath, ACallback);
end;

class function THorseCore.Use(const ACallbacks: array of THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetInstance;
  for LCallback in ACallbacks do
    Use(LCallback);
end;

class function THorseCore.All(const APath: string; const ACallback: THorseCallback): THorseCore;
var
  LMethodType: TMethodType;
begin
  for LMethodType := Low(TMethodType) to High(TMethodType) do
  begin
    Result := RegisterCallbacksRoute(LMethodType, APath);
    RegisterRoute(LMethodType, APath, ACallback);
  end;
end;

class function THorseCore.All(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := All(APath, GetCallback(ACallback));
end;

class function THorseCore.All(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := All(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.All(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := All(APath, GetCallback(ACallback));
end;
{$IFEND}

class function THorseCore.Version: string;
begin
  Result := HORSE_VERSION;
end;

class function THorseCore.Use(const APath: string; const ACallbacks: array of THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetInstance;
  for LCallback in ACallbacks do
    Use(APath, LCallback);
end;

class function THorseCore.Use(const ACallback: THorseCallback): THorseCore;
begin
  Result := GetInstance;
  Result.Routes.RegisterMiddleware('/', ACallback);
end;

class function THorseCore.Use(const APath: string; const ACallback: THorseCallback): THorseCore;
begin
  Result := GetInstance;
  Result.Routes.RegisterMiddleware(TrimPath(APath), ACallback);
end;

class function THorseCore.Head(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Head(APath, GetCallback(ACallback));
end;

class function THorseCore.Head(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Head(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Head(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Head(APath, GetCallback(ACallback));
end;
{$IFEND}

class function THorseCore.Get(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Get(APath, GetCallback(ACallback));
end;

class function THorseCore.GetCallback(const ACallbackRequest: THorseCallbackRequestResponse): THorseCallback;
begin
{$IFDEF FPC}
  {$IF DEFINED(FPC)}
  if GCallbacks2Count < 64 then
  begin
    GCallbacks2[GCallbacks2Count] := ACallbackRequest;
    Result := GWrapperList2[GCallbacks2Count];
    Inc(GCallbacks2Count);
  end
  else
    Result := Pointer(@ACallbackRequest);
  {$IFEND}
{$ELSE}
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      ACallbackRequest(Req, Res);
    end;
{$IFEND}
end;

{$IFNDEF FPC}
class function THorseCore.GetCallback(const ACallbackResponse: THorseCallbackResponse): THorseCallback;
begin
  Result :=
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      ACallbackResponse(Res);
    end;
end;
{$IFEND}

class function THorseCore.GetCallback(const ACallbackRequest: THorseCallbackRequest): THorseCallback;
begin
{$IFDEF FPC}
  {$IF DEFINED(FPC)}
  if GCallbacks1Count < 64 then
  begin
    GCallbacks1[GCallbacks1Count] := ACallbackRequest;
    Result := GWrapperList1[GCallbacks1Count];
    Inc(GCallbacks1Count);
  end
  else
    Result := Pointer(@ACallbackRequest);
  {$IFEND}
{$ELSE}
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Status(THTTPStatus.NoContent);
      ACallbackRequest(Req);
    end;
{$IFEND}
end;

{$IFNDEF FPC}
class function THorseCore.Get(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Get(APath, GetCallback(ACallback));
end;
{$IFEND}

class function THorseCore.Get(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Get(APath, GetCallback(ACallback));
end;

class function THorseCore.Post(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Post(APath, GetCallback(ACallback));
end;

class function THorseCore.Post(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Post(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Post(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Post(APath, GetCallback(ACallback));
end;
{$IFEND}

class function THorseCore.Put(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Put(APath, GetCallback(ACallback));
end;

class function THorseCore.Put(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Put(APath, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Put(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Put(APath, GetCallback(ACallback));
end;
{$IFEND}

class function THorseCore.All(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtAny, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtAny, APath, LMiddleware);
  RegisterRoute(mtAny, APath, ACallback);
end;

class function THorseCore.All(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := All(APath, AMiddlewares, GetCallback(ACallback));
end;

class function THorseCore.All(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := All(APath, AMiddlewares, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.All(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := All(APath, AMiddlewares, GetCallback(ACallback));
end;
{$IFEND}

class function THorseCore.Get(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtGet, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtGet, APath, LMiddleware);
  RegisterRoute(mtGet, APath, ACallback);
end;

class function THorseCore.Get(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Get(APath, AMiddlewares, GetCallback(ACallback));
end;

class function THorseCore.Get(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Get(APath, AMiddlewares, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Get(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Get(APath, AMiddlewares, GetCallback(ACallback));
end;
{$IFEND}

class function THorseCore.Put(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtPut, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtPut, APath, LMiddleware);
  RegisterRoute(mtPut, APath, ACallback);
end;

class function THorseCore.Put(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Put(APath, AMiddlewares, GetCallback(ACallback));
end;

class function THorseCore.Put(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Put(APath, AMiddlewares, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Put(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Put(APath, AMiddlewares, GetCallback(ACallback));
end;
{$IFEND}

{$IF (DEFINED(FPC) or (CompilerVersion > 27.0))}
class function THorseCore.Patch(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtPatch, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtPatch, APath, LMiddleware);
  RegisterRoute(mtPatch, APath, ACallback);
end;

class function THorseCore.Patch(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Patch(APath, AMiddlewares, GetCallback(ACallback));
end;

class function THorseCore.Patch(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Patch(APath, AMiddlewares, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Patch(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Patch(APath, AMiddlewares, GetCallback(ACallback));
end;
{$IFEND}
{$IFEND}

class function THorseCore.Head(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtHead, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtHead, APath, LMiddleware);
  RegisterRoute(mtHead, APath, ACallback);
end;

class function THorseCore.Head(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Head(APath, AMiddlewares, GetCallback(ACallback));
end;

class function THorseCore.Head(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Head(APath, AMiddlewares, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Head(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Head(APath, AMiddlewares, GetCallback(ACallback));
end;
{$IFEND}

class function THorseCore.Post(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtPost, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtPost, APath, LMiddleware);
  RegisterRoute(mtPost, APath, ACallback);
end;

class function THorseCore.Post(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Post(APath, AMiddlewares, GetCallback(ACallback));
end;

class function THorseCore.Post(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Post(APath, AMiddlewares, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Post(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Post(APath, AMiddlewares, GetCallback(ACallback));
end;
{$IFEND}

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
class function THorseCore.Delete(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtDelete, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtDelete, APath, LMiddleware);
  RegisterRoute(mtDelete, APath, ACallback);
end;

class function THorseCore.Delete(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Delete(APath, AMiddlewares, GetCallback(ACallback));
end;

class function THorseCore.Delete(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Delete(APath, AMiddlewares, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Delete(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Delete(APath, AMiddlewares, GetCallback(ACallback));
end;
{$IFEND}
{$IFEND}

class function THorseCore.Query(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): THorseCore;
var
  LMiddleware: THorseCallback;
begin
  Result := RegisterCallbacksRoute(mtQuery, APath);
  for LMiddleware in AMiddlewares do
    RegisterRouteMiddleware(mtQuery, APath, LMiddleware);
  RegisterRoute(mtQuery, APath, ACallback);
end;

class function THorseCore.Query(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Query(APath, AMiddlewares, GetCallback(ACallback));
end;

class function THorseCore.Query(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Query(APath, AMiddlewares, GetCallback(ACallback));
end;

{$IFNDEF FPC}
class function THorseCore.Query(const APath: string; const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Query(APath, AMiddlewares, GetCallback(ACallback));
end;
{$IFEND}

constructor THorseModule.Create(const ASelfInstance, ADefaultHorseCoreInstance: PHorseCore; const AHorseRouter: PHorseRouter);
begin
  FSelfInstance := ASelfInstance;
  FDefaultHorseCoreInstance := ADefaultHorseCoreInstance;
  FHorseRouter := AHorseRouter;
end;

function THorseModule.ToHorse: THorseCore;
begin
  Result := GetSelfInstance^;
  Result.FDefaultHorse := GetDefaultHorseCoreInstance^;
  Result.FRoutes := GetHorseRouter^;
end;

function THorseModule.GetDefaultHorseCoreInstance: PHorseCore;
begin
  Result := FDefaultHorseCoreInstance;
end;

function THorseModule.GetHorseRouter: PHorseRouter;
begin
  Result := FHorseRouter;
end;

function THorseModule.GetSelfInstance: PHorseCore;
begin
  Result := FSelfInstance;
end;
function THorseCore.BaseAddCallback(const ACallback: THorseCallback): THorseCoreBase;
begin
  THorseCore.AddCallback(ACallback);
  Result := Self;
end;

{$IF DEFINED(FPC)}
function THorseCore.BaseAddCallbacks(const ACallbacks: TList<THorseCallback>): THorseCoreBase;
begin
  THorseCore.AddCallbacks(ACallbacks);
  Result := Self;
end;
{$ELSE}
function THorseCore.BaseAddCallbacks(const ACallbacks: TArray<THorseCallback>): THorseCoreBase;
begin
  THorseCore.AddCallbacks(ACallbacks);
  Result := Self;
end;
{$ENDIF}

function THorseCore.BaseUse(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  THorseCore.Use(APath, ACallback);
  Result := Self;
end;

function THorseCore.BaseUse(const ACallback: THorseCallback): THorseCoreBase;
begin
  THorseCore.Use(ACallback);
  Result := Self;
end;

function THorseCore.BaseUse(const APath: string; const ACallbacks: array of THorseCallback): THorseCoreBase;
begin
  THorseCore.Use(APath, ACallbacks);
  Result := Self;
end;

function THorseCore.BaseUse(const ACallbacks: array of THorseCallback): THorseCoreBase;
begin
  THorseCore.Use(ACallbacks);
  Result := Self;
end;

function THorseCore.BaseRoute(const APath: string): IInterface;
begin
  Result := THorseCore.Route(APath);
end;

function THorseCore.BaseAll(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  THorseCore.All(APath, ACallback);
  Result := Self;
end;

function THorseCore.BaseAll(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  THorseCore.All(APath, ACallback);
  Result := Self;
end;

function THorseCore.BaseAll(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  THorseCore.All(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCore.BaseAll(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  THorseCore.All(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseCore.BaseGet(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  THorseCore.Get(APath, ACallback);
  Result := Self;
end;

function THorseCore.BaseGet(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  THorseCore.Get(APath, ACallback);
  Result := Self;
end;

function THorseCore.BaseGet(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  THorseCore.Get(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCore.BaseGet(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  THorseCore.Get(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseCore.BasePut(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  THorseCore.Put(APath, ACallback);
  Result := Self;
end;

function THorseCore.BasePut(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  THorseCore.Put(APath, ACallback);
  Result := Self;
end;

function THorseCore.BasePut(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  THorseCore.Put(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCore.BasePut(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  THorseCore.Put(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseCore.BaseHead(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  THorseCore.Head(APath, ACallback);
  Result := Self;
end;

function THorseCore.BaseHead(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  THorseCore.Head(APath, ACallback);
  Result := Self;
end;

function THorseCore.BaseHead(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  THorseCore.Head(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCore.BaseHead(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  THorseCore.Head(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

function THorseCore.BasePost(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  THorseCore.Post(APath, ACallback);
  Result := Self;
end;

function THorseCore.BasePost(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  THorseCore.Post(APath, ACallback);
  Result := Self;
end;

function THorseCore.BasePost(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  THorseCore.Post(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCore.BasePost(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  THorseCore.Post(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

// Delete
function THorseCore.BaseDelete(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  THorseCore.Delete(APath, ACallback);
  Result := Self;
end;

function THorseCore.BaseDelete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  THorseCore.Delete(APath, ACallback);
  Result := Self;
end;

function THorseCore.BaseDelete(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  THorseCore.Delete(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCore.BaseDelete(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  THorseCore.Delete(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

// Patch
function THorseCore.BasePatch(const APath: string; const ACallback: THorseCallback): THorseCoreBase;
begin
  THorseCore.Patch(APath, ACallback);
  Result := Self;
end;

function THorseCore.BasePatch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase;
begin
  THorseCore.Patch(APath, ACallback);
  Result := Self;
end;

function THorseCore.BasePatch(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase;
begin
  THorseCore.Patch(APath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCore.BasePatch(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase;
begin
  THorseCore.Patch(APath, ACallback);
  Result := Self;
end;
{$ENDIF}

class procedure THorseCore.OnError(const ACallback: THorseOnError);
begin
  FOnError := ACallback;
end;

class function THorseCore.HasOnError: Boolean;
var
  LInstance: THorseCoreBase;
begin
  Result := Assigned(FOnError);
  if not Result then
  begin
    GInstancesLock.Enter;
    try
      for LInstance in GInstances.Values do
      begin
        if (LInstance is THorseInstance) and THorseInstance(LInstance).HasOnError then
        begin
          Result := True;
          Break;
        end;
      end;
    finally
      GInstancesLock.Leave;
    end;
  end;
end;

class procedure THorseCore.ExecuteOnError(const ARequest: THorseRequest; const AResponse: THorseResponse; const AException: Exception);
var
  LInstance: THorseCoreBase;
  LPort: Integer;
  LHandler: THorseOnError;
begin
  LPort := 9000;
  if Assigned(ARequest) and (ARequest.RawWebRequest <> nil) then
    LPort := ARequest.RawWebRequest.ServerPort;

  LInstance := GetHorseInstanceByPort(LPort);
  LHandler := nil;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
  begin
    if THorseInstance(LInstance).HasOnError then
      LHandler := THorseInstance(LInstance).ErrorHandler;
  end;

  if not Assigned(LHandler) then
    LHandler := FOnError;

  if Assigned(LHandler) then
  begin
    try
      LHandler(ARequest, AResponse, AException);
    except
      on E: Exception do
      begin
        AResponse.Send('Internal Application Error in OnError: ' + E.Message).Status(THTTPStatus.InternalServerError);
      end;
    end;
  end
  else
  begin
    raise AException;
  end;
end;

class procedure THorseCore.AddOnRequest(const ACallback: THorseCallback);
begin
  if FOnRequest = nil then
    FOnRequest := TList<THorseCallback>.Create;
  FOnRequest.Add(ACallback);
end;

class procedure THorseCore.AddPreParsing(const ACallback: THorseCallback);
begin
  if FPreParsing = nil then
    FPreParsing := TList<THorseCallback>.Create;
  FPreParsing.Add(ACallback);
end;

class procedure THorseCore.AddPreValidation(const ACallback: THorseCallback);
begin
  if FPreValidation = nil then
    FPreValidation := TList<THorseCallback>.Create;
  FPreValidation.Add(ACallback);
end;

class procedure THorseCore.AddOnSend(const ACallback: THorseOnSendString);
begin
  if FOnSendString = nil then
    FOnSendString := TList<THorseOnSendString>.Create;
  FOnSendString.Add(ACallback);
end;

class procedure THorseCore.AddOnSend(const ACallback: THorseOnSendBytes);
begin
  if FOnSendBytes = nil then
    FOnSendBytes := TList<THorseOnSendBytes>.Create;
  FOnSendBytes.Add(ACallback);
end;

class procedure THorseCore.AddOnResponse(const ACallback: THorseCallback);
begin
  if FOnResponse = nil then
    FOnResponse := TList<THorseCallback>.Create;
  FOnResponse.Add(ACallback);
end;

class procedure THorseCore.AddOnTelemetry(const ACallback: THorseOnTelemetry);
begin
  if FOnTelemetry = nil then
    FOnTelemetry := TList<THorseOnTelemetry>.Create;
  FOnTelemetry.Add(ACallback);
end;

class procedure THorseCore.ResetHooks;
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
  if FOnTelemetry <> nil then
    FOnTelemetry.Clear;
end;

class function THorseCore.GetActiveRequests: Integer;
begin
  Result := FActiveRequests;
end;

class procedure THorseCore.IncrementActiveRequests;
begin
  {$IF DEFINED(FPC)}
  InterlockedIncrement(FActiveRequests);
  {$ELSE}
  TInterlocked.Increment(FActiveRequests);
  {$ENDIF}
end;

class procedure THorseCore.DecrementActiveRequests;
begin
  {$IF DEFINED(FPC)}
  InterlockedDecrement(FActiveRequests);
  {$ELSE}
  TInterlocked.Decrement(FActiveRequests);
  {$ENDIF}
end;

class function THorseCore.GetIsShuttingDown: Boolean;
begin
  Result := FIsShuttingDown;
end;

class procedure THorseCore.SetIsShuttingDown(const AValue: Boolean);
begin
  FIsShuttingDown := AValue;
end;

class procedure THorseCore.ExecuteOnRequest(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
var
  LExecutor: IHorseLifecycleExecutor;
  LInstance: THorseCoreBase;
  LPort: Integer;
begin
  LPort := 9000;
  if Assigned(ARequest) and (ARequest.RawWebRequest <> nil) then
    LPort := ARequest.RawWebRequest.ServerPort;

  LInstance := GetHorseInstanceByPort(LPort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
  begin
    THorseInstance(LInstance).ExecuteOnRequest(ARequest, AResponse, AOnComplete);
  end
  else
  begin
    if Assigned(ARequest) and (FOnRequest <> nil) and (FOnRequest.Count > 0) then
    begin
      LExecutor := THorseLifecycleExecutor.Create(FOnRequest, ARequest, AResponse, AOnComplete);
      LExecutor.Next;
    end
    else
      AOnComplete();
  end;
end;

class procedure THorseCore.ExecutePreParsing(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
var
  LExecutor: IHorseLifecycleExecutor;
  LInstance: THorseCoreBase;
  LPort: Integer;
begin
  LPort := 9000;
  if Assigned(ARequest) and (ARequest.RawWebRequest <> nil) then
    LPort := ARequest.RawWebRequest.ServerPort;

  LInstance := GetHorseInstanceByPort(LPort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
  begin
    THorseInstance(LInstance).ExecutePreParsing(ARequest, AResponse, AOnComplete);
  end
  else
  begin
    if Assigned(ARequest) and (FPreParsing <> nil) and (FPreParsing.Count > 0) then
    begin
      LExecutor := THorseLifecycleExecutor.Create(FPreParsing, ARequest, AResponse, AOnComplete);
      LExecutor.Next;
    end
    else
      AOnComplete();
  end;
end;

class procedure THorseCore.ExecutePreValidation(const ARequest: THorseRequest; const AResponse: THorseResponse; const AOnComplete: TProc);
var
  LExecutor: IHorseLifecycleExecutor;
  LInstance: THorseCoreBase;
  LPort: Integer;
begin
  LPort := 9000;
  if Assigned(ARequest) and (ARequest.RawWebRequest <> nil) then
    LPort := ARequest.RawWebRequest.ServerPort;

  LInstance := GetHorseInstanceByPort(LPort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
  begin
    THorseInstance(LInstance).ExecutePreValidation(ARequest, AResponse, AOnComplete);
  end
  else
  begin
    if Assigned(ARequest) and (FPreValidation <> nil) and (FPreValidation.Count > 0) then
    begin
      LExecutor := THorseLifecycleExecutor.Create(FPreValidation, ARequest, AResponse, AOnComplete);
      LExecutor.Next;
    end
    else
      AOnComplete();
  end;
end;

class procedure THorseCore.ExecuteOnSend(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: string);
var
  LHook: THorseOnSendString;
  LInstance: THorseCoreBase;
  LPort: Integer;
begin
  LPort := 9000;
  if Assigned(ARequest) and (ARequest.RawWebRequest <> nil) then
    LPort := ARequest.RawWebRequest.ServerPort;

  LInstance := GetHorseInstanceByPort(LPort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
  begin
    THorseInstance(LInstance).ExecuteOnSend(ARequest, AResponse, AContent);
  end
  else
  begin
    if Assigned(ARequest) and (FOnSendString <> nil) then
    begin
      for LHook in FOnSendString do
        LHook(ARequest, AResponse, AContent);
    end;
  end;
end;

class procedure THorseCore.ExecuteOnSend(const ARequest: THorseRequest; const AResponse: THorseResponse; var AContent: TBytes);
var
  LHook: THorseOnSendBytes;
  LInstance: THorseCoreBase;
  LPort: Integer;
begin
  LPort := 9000;
  if Assigned(ARequest) and (ARequest.RawWebRequest <> nil) then
    LPort := ARequest.RawWebRequest.ServerPort;

  LInstance := GetHorseInstanceByPort(LPort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
  begin
    THorseInstance(LInstance).ExecuteOnSend(ARequest, AResponse, AContent);
  end
  else
  begin
    if Assigned(ARequest) and (FOnSendBytes <> nil) then
    begin
      for LHook in FOnSendBytes do
        LHook(ARequest, AResponse, AContent);
    end;
  end;
end;

class procedure THorseCore.ExecuteOnResponse(const ARequest: THorseRequest; const AResponse: THorseResponse);
var
  LCallback: THorseCallback;
  LInstance: THorseCoreBase;
  LPort: Integer;
begin
  LPort := 9000;
  if Assigned(ARequest) and (ARequest.RawWebRequest <> nil) then
    LPort := ARequest.RawWebRequest.ServerPort;

  LInstance := GetHorseInstanceByPort(LPort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
  begin
    THorseInstance(LInstance).ExecuteOnResponse(ARequest, AResponse);
  end
  else
  begin
    if Assigned(ARequest) and (FOnResponse <> nil) then
    begin
      for LCallback in FOnResponse do
      begin
        try
          {$IF DEFINED(FPC)}
          THorseCallbackProc(LCallback)(ARequest, AResponse, GetInstance.EmptyNext);
          {$ELSE}
          LCallback(ARequest, AResponse, procedure begin end);
          {$ENDIF}
        except
          // Abafar exceções no onResponse para não crashar a finalização da thread de socket
        end;
      end;
    end;
  end;
end;

class procedure THorseCore.ExecuteOnTelemetry(const ARequest: THorseRequest; const AResponse: THorseResponse; const AExecutionTimeMS: Double);
var
  LCallback: THorseOnTelemetry;
  LInstance: THorseCoreBase;
  LPort: Integer;
begin
  LPort := 9000;
  if Assigned(ARequest) and (ARequest.RawWebRequest <> nil) then
    LPort := ARequest.RawWebRequest.ServerPort;

  LInstance := GetHorseInstanceByPort(LPort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
  begin
    THorseInstance(LInstance).ExecuteOnTelemetry(ARequest, AResponse, AExecutionTimeMS);
  end
  else
  begin
    if Assigned(ARequest) and (FOnTelemetry <> nil) then
    begin
      for LCallback in FOnTelemetry do
      begin
        try
          LCallback(ARequest, AResponse, AExecutionTimeMS);
        except
          // Abafar exceções no OnTelemetry para não crashar a requisição ou o socket
        end;
      end;
    end;
  end;
end;

initialization
  GetHorseCoreInstance := @THorseCore.GetInstance;

end.
