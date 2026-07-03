unit Horse.Core;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Generics.Collections,
  SyncObjs,
{$ELSE}
  System.Generics.Collections,
  System.SyncObjs,
  Web.HTTPApp,
{$ENDIF}
  Horse.Core.RouterTree,
  Horse.Callback,
  Horse.Core.Group.Contract,
  Horse.Core.Route.Contract,
  Horse.Commons,
  Horse.Core.Router.Contract,
  Horse.Core.Base;

type
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
    class function TrimPath(const APath: string): string;
    class function RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback): THorseCore;
    class var FDefaultHorse: THorseCore;

    function InternalRoute(const APath: string): IHorseCoreRoute<THorseCore>;
    function InternalGroup: IHorseCoreGroup<THorseCore>;
    function InternalGetRoutes: IHorseRouter;
    procedure InternalSetRoutes(const AValue: IHorseRouter);
    class function GetRoutes: IHorseRouter; static;
    class procedure SetRoutes(const AValue: IHorseRouter); static;
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
    class property Routes: IHorseRouter read GetRoutes write SetRoutes;
    class function GetInstance: THorseCore;
    class function Version: string;

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
{$IF DEFINED(FPC)}
  SysUtils,
{$ELSE}
  System.SysUtils,
{$ENDIF}
  Horse.Core.Route,
  Horse.Core.Group,
  Horse.Constants,
  Horse.Request,
  Horse.Response,
  Horse.Proc
  {$IFNDEF FPC}
  , Horse.Core.Factory
  {$ENDIF}
  ;

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

class function THorseCore.GetRoutes: IHorseRouter;
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

class function THorseCore.Route(const APath: string): IHorseCoreRoute<THorseCore>;
begin
  Result := GetInstance.InternalRoute(APath);
end;

class procedure THorseCore.SetRoutes(const AValue: IHorseRouter);
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

initialization
  GetHorseCoreInstance := @THorseCore.GetInstance;

end.
