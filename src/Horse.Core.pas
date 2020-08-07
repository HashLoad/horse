unit Horse.Core;

interface

uses
  System.SysUtils, Web.HTTPApp,
  Horse.Core.RouterTree, Horse.Core.Route, Horse.Core.Group,
  Horse.Core.Group.Contract, Horse.Core.Route.Contract;

type

  THorseCore = class;

  THorseCore = class
  private
    { private declarations }
    FRoutes: THorseRouterTree;
    class function RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback): THorseCore;
    class var FDefaultHorse: THorseCore;
    class function GetDefaultHorse: THorseCore;
    function InternalRoute(APath: string): IHorseCoreRoute<THorseCore>;
    function InternalGroup(): IHorseCoreGroup<THorseCore>;
    function InternalGetRoutes: THorseRouterTree;
    procedure InternalSetRoutes(const Value: THorseRouterTree);
    class function GetRoutes: THorseRouterTree; static;
    class procedure SetRoutes(const Value: THorseRouterTree); static;
  public
    { public declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    class destructor UnInitialize; virtual;
    class function Group(): IHorseCoreGroup<THorseCore>;
    class function Route(APath: string): IHorseCoreRoute<THorseCore>;
    class function Use(APath: string; ACallback: THorseCallback): THorseCore; overload;
    class function Use(ACallback: THorseCallback): THorseCore; overload;
    class function Use(APath: string; ACallbacks: array of THorseCallback): THorseCore; overload;
    class function Use(ACallbacks: array of THorseCallback): THorseCore; overload;

    class function Get(APath: string; ACallback: THorseCallback): THorseCore; overload;
    class function Get(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore; overload;
    class function Get(APath: string; ACallbacks: array of THorseCallback): THorseCore; overload;
    class function Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore; overload;

    class function Put(APath: string; ACallback: THorseCallback): THorseCore; overload;
    class function Put(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore; overload;
    class function Put(APath: string; ACallbacks: array of THorseCallback): THorseCore; overload;
    class function Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore; overload;

    class function Patch(APath: string; ACallback: THorseCallback): THorseCore; overload;
    class function Patch(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore; overload;
    class function Patch(APath: string; ACallbacks: array of THorseCallback): THorseCore; overload;
    class function Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore; overload;

    class function Head(APath: string; ACallback: THorseCallback): THorseCore; overload;
    class function Head(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore; overload;
    class function Head(APath: string; ACallbacks: array of THorseCallback): THorseCore; overload;
    class function Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore; overload;

    class function Post(APath: string; ACallback: THorseCallback): THorseCore; overload;
    class function Post(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore; overload;
    class function Post(APath: string; ACallbacks: array of THorseCallback): THorseCore; overload;
    class function Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore; overload;

    class function Delete(APath: string; ACallback: THorseCallback): THorseCore; overload;
    class function Delete(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore; overload;
    class function Delete(APath: string; ACallbacks: array of THorseCallback): THorseCore; overload;
    class function Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore; overload;

    class property Routes: THorseRouterTree read GetRoutes write SetRoutes;

    class function GetInstance: THorseCore;
  end;

implementation

{ THorseCore }

constructor THorseCore.Create;
begin
  FRoutes := THorseRouterTree.Create;
  if FDefaultHorse <> nil then
    raise Exception.Create('The Horse instance has already been created');
  FDefaultHorse := Self
end;

destructor THorseCore.Destroy;
begin
  FRoutes.Free;
  inherited;
end;

class function THorseCore.GetDefaultHorse: THorseCore;
begin
  if FDefaultHorse = nil then
    FDefaultHorse := THorseCore.Create;
  Result := FDefaultHorse;
end;

class function THorseCore.GetInstance: THorseCore;
begin
  Result := GetDefaultHorse;
end;

class function THorseCore.GetRoutes: THorseRouterTree;
begin
  Result := GetDefaultHorse.InternalGetRoutes;
end;

class function THorseCore.Group: IHorseCoreGroup<THorseCore>;
begin
  Result := GetDefaultHorse.InternalGroup();
end;

class function THorseCore.RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback): THorseCore;
var
  LDefaultHorse: THorseCore;
begin
  LDefaultHorse := GetDefaultHorse;
  Result := LDefaultHorse;

  if APath.EndsWith('/') then
    APath := APath.Remove(High(APath) - 1, 1);
  if not APath.StartsWith('/') then
    APath := '/' + APath;
  LDefaultHorse.GetRoutes.RegisterRoute(AHTTPType, APath, ACallback);
end;

class function THorseCore.Route(APath: string): IHorseCoreRoute<THorseCore>;
begin
  Result := GetDefaultHorse.InternalRoute(APath);
end;

class procedure THorseCore.SetRoutes(const Value: THorseRouterTree);
begin
  GetDefaultHorse.InternalSetRoutes(Value);
end;

function THorseCore.InternalGetRoutes: THorseRouterTree;
begin
  Result := FRoutes;
end;

function THorseCore.InternalGroup(): IHorseCoreGroup<THorseCore>;
begin
  Result := THorseCoreGroup<THorseCore>.Create(FRoutes);
end;

function THorseCore.InternalRoute(APath: string): IHorseCoreRoute<THorseCore>;
begin
  Result := THorseCoreRoute<THorseCore>.Create(APath, Self);
end;

procedure THorseCore.InternalSetRoutes(const Value: THorseRouterTree);
begin
  FRoutes := Value;
end;

class destructor THorseCore.UnInitialize;
begin
  FreeAndNil(FDefaultHorse);
end;

class function THorseCore.Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Delete(APath, LCallback);
  Delete(APath, ACallback);
end;

class function THorseCore.Delete(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore;
begin
  Result := Delete(APath, [AMiddleware, ACallback]);
end;

class function THorseCore.Delete(APath: string; ACallbacks: array of THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Delete(APath, LCallback);
end;

class function THorseCore.Delete(APath: string; ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterRoute(mtDelete, APath, ACallback);
end;

class function THorseCore.Head(APath: string; ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterRoute(mtHead, APath, ACallback);
end;

class function THorseCore.Get(APath: string; ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterRoute(mtGet, APath, ACallback);
end;

class function THorseCore.Head(APath: string; ACallbacks: array of THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Head(APath, LCallback);
end;

class function THorseCore.Get(APath: string; ACallbacks: array of THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Get(APath, LCallback);
end;

class function THorseCore.Head(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore;
begin
  Result := Head(APath, [AMiddleware, ACallback]);
end;

class function THorseCore.Get(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore;
begin
  Result := Get(APath, [AMiddleware, ACallback]);
end;

class function THorseCore.Post(APath: string; ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterRoute(mtPost, APath, ACallback);
end;

class function THorseCore.Patch(APath: string; ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterRoute(mtPatch, APath, ACallback);
end;

class function THorseCore.Put(APath: string; ACallback: THorseCallback): THorseCore;
begin
  Result := RegisterRoute(mtPut, APath, ACallback);
end;

class function THorseCore.Use(ACallbacks: array of THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Use(LCallback);
end;

class function THorseCore.Use(APath: string; ACallbacks: array of THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Use(APath, LCallback);
end;

class function THorseCore.Use(ACallback: THorseCallback): THorseCore;
begin
  Result := GetDefaultHorse;
  Result.Routes.RegisterMiddleware('/', ACallback);
end;

class function THorseCore.Use(APath: string; ACallback: THorseCallback): THorseCore;
begin
  Result := GetDefaultHorse;
  Result.Routes.RegisterMiddleware(APath, ACallback);
end;

class function THorseCore.Post(APath: string; ACallbacks: array of THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Post(APath, LCallback);
end;

class function THorseCore.Post(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore;
begin
  Result := Post(APath, [AMiddleware, ACallback]);
end;

class function THorseCore.Patch(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore;
begin
  Result := Patch(APath, [AMiddleware, ACallback]);
end;

class function THorseCore.Put(APath: string; AMiddleware, ACallback: THorseCallback): THorseCore;
begin
  Result := Put(APath, [AMiddleware, ACallback]);
end;

class function THorseCore.Patch(APath: string; ACallbacks: array of THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Patch(APath, LCallback);
end;

class function THorseCore.Put(APath: string; ACallbacks: array of THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Put(APath, LCallback);
end;

class function THorseCore.Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Head(APath, LCallback);
  Head(APath, ACallback);
end;

class function THorseCore.Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Get(APath, LCallback);
  Get(APath, ACallback);
end;

class function THorseCore.Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Post(APath, LCallback);
  Post(APath, ACallback);
end;

class function THorseCore.Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Patch(APath, LCallback);
  Patch(APath, ACallback);
end;

class function THorseCore.Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in ACallbacks do
    Put(APath, LCallback);
  Put(APath, ACallback);
end;

end.

