unit Horse.Core;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Generics.Collections,
{$ELSE}
  System.SysUtils, System.Generics.Collections, Web.HTTPApp,
{$ENDIF}
  Horse.Core.RouterTree, Horse.Commons, Horse.Request, Horse.Response, Horse.Constants, Horse.Callback,
  Horse.Core.Group.Contract, Horse.Core.Route.Contract;

type
  THorseCore = class;
  PHorseCore = ^THorseCore;
  PHorseModule = ^THorseModule;

  THorseModule = record
  private
    FSelfInstance: PHorseCore;
    FDefaultHorseCoreInstance: PHorseCore;
    FHorseRouterTree: PHorseRouterTree;
    function GetSelfInstance: PHorseCore;
    function GetDefaultHorseCoreInstance: PHorseCore;
    function GetHorseRouterTree: PHorseRouterTree;
  public
    function ToHorse: THorseCore;
    constructor Create(const ASelfInstance, ADefaultHorseCoreInstance: PHorseCore; const AHorseRouterTree: PHorseRouterTree);
  end;

  THorseCore = class
  private
    class var FRoutes: THorseRouterTree;
    class var FCallbacks: TList<THorseCallback>;
    class function TrimPath(const APath: string): string;
    class function RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback): THorseCore;
    class var FDefaultHorse: THorseCore;

    function InternalRoute(const APath: string): IHorseCoreRoute<THorseCore>;
    function InternalGroup: IHorseCoreGroup<THorseCore>;
    function InternalGetRoutes: THorseRouterTree;
    procedure InternalSetRoutes(const AValue: THorseRouterTree);
    class function GetRoutes: THorseRouterTree; static;
    class procedure SetRoutes(const AValue: THorseRouterTree); static;
    class function MakeHorseModule: THorseModule;

    {$IFNDEF FPC}
    class function GetCallback(const ACallbackRequest: THorseCallbackRequestResponse): THorseCallback; overload;
    class function GetCallback(const ACallbackRequest: THorseCallbackRequest): THorseCallback; overload;
    class function GetCallback(const ACallbackResponse: THorseCallbackResponse): THorseCallback; overload;
    {$ENDIF}

    class function GetCallbacks: TArray<THorseCallback>;
    class function RegisterCallbacksRoute(const AMethod: TMethodType; const APath: string): THorseCore;
  public
    constructor Create; virtual;
    class function ToModule: THorseModule;
    class destructor UnInitialize; {$IFNDEF FPC}virtual;{$ENDIF}

    class function AddCallback(const ACallback: THorseCallback): THorseCore;
    class function AddCallbacks(const ACallbacks: TArray<THorseCallback>): THorseCore;

    class function Group: IHorseCoreGroup<THorseCore>;
    class function Route(const APath: string): IHorseCoreRoute<THorseCore>;

    class function Use(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    class function Use(const ACallback: THorseCallback): THorseCore; overload;
    class function Use(const APath: string; const ACallbacks: array of THorseCallback): THorseCore; overload;
    class function Use(const ACallbacks: array of THorseCallback): THorseCore; overload;

    class function Get(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    {$IFNDEF FPC}
    class function Get(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Get(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
    class function Get(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
    {$IFEND}

    class function Put(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    {$IFNDEF FPC}
    class function Put(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Put(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
    class function Put(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
    {$IFEND}

    {$IF (DEFINED(FPC) or (CompilerVersion > 27.0))}
    class function Patch(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
      {$IFNDEF FPC}
      class function Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
      class function Patch(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
      class function Patch(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
      {$IFEND}
    {$IFEND}

    class function Head(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    {$IFNDEF FPC}
    class function Head(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Head(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
    class function Head(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
    {$IFEND}

    class function Post(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
    {$IFNDEF FPC}
    class function Post(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Post(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
    class function Post(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
    {$IFEND}

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    class function Delete(const APath: string; const ACallback: THorseCallback): THorseCore; overload;
      {$IFNDEF FPC}
      class function Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore; overload;
      class function Delete(const APath: string; const ACallback: THorseCallbackRequest): THorseCore; overload;
      class function Delete(const APath: string; const ACallback: THorseCallbackResponse): THorseCore; overload;
      {$IFEND}
    {$IFEND}

    class property Routes: THorseRouterTree read GetRoutes write SetRoutes;
    class function GetInstance: THorseCore;
    class function Version: string;
  end;

implementation

uses Horse.Core.Route, Horse.Core.Group;

class function THorseCore.AddCallback(const ACallback: THorseCallback): THorseCore;
begin
  Result := GetInstance;
  if FCallbacks = nil then
    FCallbacks := TList<THorseCallback>.Create;
  FCallbacks.Add(ACallback);
end;

class function THorseCore.AddCallbacks(const ACallbacks: TArray<THorseCallback>): THorseCore;
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    AddCallback(LCallback);
  Result := GetInstance;
end;

constructor THorseCore.Create;
begin
  if FDefaultHorse <> nil then
    raise Exception.Create('The Horse instance has already been created');
  if FRoutes = nil then
    FRoutes := THorseRouterTree.Create;
  FDefaultHorse := Self
end;

class function THorseCore.GetInstance: THorseCore;
begin
  if FDefaultHorse = nil then
    FDefaultHorse := THorseCore.Create;
  Result := FDefaultHorse;
end;

class function THorseCore.GetCallbacks: TArray<THorseCallback>;
begin
  Result := [];
  if Assigned(FCallbacks) then
  begin
    Result := FCallbacks.ToArray;
    FCallbacks.Clear;
  end;
end;

class function THorseCore.RegisterCallbacksRoute(const AMethod: TMethodType; const APath: string): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetInstance;
  for LCallback in GetCallbacks do
    RegisterRoute(AMethod, APath, LCallback);
end;

class function THorseCore.GetRoutes: THorseRouterTree;
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

class procedure THorseCore.SetRoutes(const AValue: THorseRouterTree);
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

function THorseCore.InternalGetRoutes: THorseRouterTree;
begin
  Result := FRoutes;
end;

function THorseCore.InternalGroup: IHorseCoreGroup<THorseCore>;
begin
  Result := THorseCoreGroup<THorseCore>.Create;
end;

function THorseCore.InternalRoute(const APath: string): IHorseCoreRoute<THorseCore>;
begin
  Result := THorseCoreRoute<THorseCore>.Create(APath);
end;

procedure THorseCore.InternalSetRoutes(const AValue: THorseRouterTree);
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
  if FRoutes <> nil then
    FreeAndNil(FRoutes);
  if FCallbacks <> nil then
    FreeAndNil(FCallbacks);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
  {$IFNDEF FPC}
class function THorseCore.Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Delete(APath, GetCallback(ACallback));
end;

class function THorseCore.Delete(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Delete(APath, GetCallback(ACallback));
end;

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
  {$IFNDEF FPC}
class function THorseCore.Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Patch(APath, GetCallback(ACallback));
end;

class function THorseCore.Patch(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Patch(APath, GetCallback(ACallback));
end;

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

{$IFNDEF FPC}
class function THorseCore.Head(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Head(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Head(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Head(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Head(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Head(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Get(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Get(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.GetCallback(const ACallbackRequest: THorseCallbackRequestResponse): THorseCallback;
begin
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      ACallbackRequest(Req, Res);
    end;
end;
{$IFEND}

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

{$IFNDEF FPC}
class function THorseCore.GetCallback(const ACallbackRequest: THorseCallbackRequest): THorseCallback;
begin
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Status(THTTPStatus.NoContent);
      ACallbackRequest(Req);
    end;
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Get(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Get(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Get(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Get(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Post(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Post(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Post(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Post(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Post(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Post(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Put(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  Result := Put(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Put(const APath: string; const ACallback: THorseCallbackRequest): THorseCore;
begin
  Result := Put(APath, GetCallback(ACallback));
end;
{$IFEND}

{$IFNDEF FPC}
class function THorseCore.Put(const APath: string; const ACallback: THorseCallbackResponse): THorseCore;
begin
  Result := Put(APath, GetCallback(ACallback));
end;
{$IFEND}

constructor THorseModule.Create(const ASelfInstance, ADefaultHorseCoreInstance: PHorseCore; const AHorseRouterTree: PHorseRouterTree);
begin
  FSelfInstance := ASelfInstance;
  FDefaultHorseCoreInstance := ADefaultHorseCoreInstance;
  FHorseRouterTree := AHorseRouterTree;
end;

function THorseModule.ToHorse: THorseCore;
begin
  Result := GetSelfInstance^;
  Result.FDefaultHorse := GetDefaultHorseCoreInstance^;
  Result.FRoutes := GetHorseRouterTree^;
end;

function THorseModule.GetDefaultHorseCoreInstance: PHorseCore;
begin
  Result := FDefaultHorseCoreInstance;
end;

function THorseModule.GetHorseRouterTree: PHorseRouterTree;
begin
  Result := FHorseRouterTree;
end;

function THorseModule.GetSelfInstance: PHorseCore;
begin
  Result := FSelfInstance;
end;

end.
