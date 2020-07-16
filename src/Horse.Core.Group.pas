unit Horse.Core.Group;

interface

uses
  Horse.Core.Group.Intf, Horse.Core.Route.Intf, Horse.Router, Horse.Core, Horse.Core.Route,
  System.SysUtils;

type
  THorseCoreGroup = class(TInterfacedObject, IHorseCoreGroup)
  private
    FHorseCore: THorseCore;
  public
    constructor Create(ARoutes: THorseRouterTree);
    destructor Destroy; override;

    function Prefix(APrefix: string): IHorseCoreGroup;
    function Route(APath: string): IHorseCoreRoute;

    function Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Get(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Get(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;

    function Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Put(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Put(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;

    function Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Patch(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Patch(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;

    function Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Head(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Head(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;

    function Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Post(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Post(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;

    function Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Delete(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Delete(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;
  end;

implementation

{ THorseCoreGroup }

constructor THorseCoreGroup.Create(ARoutes: THorseRouterTree);
begin
  FHorseCore := THorseCore.Create;
  FHorseCore.Routes := ARoutes.CreateRouter(ARoutes.GetPrefix().Trim(['/']));
  FHorseCore.Routes.Prefix(EmptyStr);
end;

destructor THorseCoreGroup.Destroy;
begin
  FHorseCore.Free;
  inherited;
end;

function THorseCoreGroup.Prefix(APrefix: string): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Routes.Prefix(APrefix);
end;

function THorseCoreGroup.Route(APath: string): IHorseCoreRoute;
begin
  Result := FHorseCore.Route(APath);
end;

function THorseCoreGroup.Get(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Get(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup.Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Get(APath, ACallback);
end;

function THorseCoreGroup.Get(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Get(APath, ACallbacks);
end;

function THorseCoreGroup.Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Get(APath, ACallbacks, ACallback);
end;

function THorseCoreGroup.Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Put(APath, ACallback);
end;

function THorseCoreGroup.Put(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Put(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup.Put(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Put(APath, ACallbacks);
end;

function THorseCoreGroup.Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Put(APath, ACallbacks, ACallback);
end;

function THorseCoreGroup.Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Patch(APath, ACallback);
end;

function THorseCoreGroup.Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Patch(APath, ACallbacks, ACallback);
end;

function THorseCoreGroup.Patch(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Patch(APath, ACallbacks);
end;

function THorseCoreGroup.Patch(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Patch(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup.Head(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Head(APath, ACallbacks);
end;

function THorseCoreGroup.Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Head(APath, ACallbacks, ACallback);
end;

function THorseCoreGroup.Head(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Head(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup.Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Head(APath, ACallback);
end;

function THorseCoreGroup.Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Post(APath, ACallback);
end;

function THorseCoreGroup.Post(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Post(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup.Post(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Post(APath, ACallbacks);
end;

function THorseCoreGroup.Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Post(APath, ACallbacks, ACallback);
end;

function THorseCoreGroup.Delete(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Delete(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup.Delete(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Delete(APath, ACallbacks);
end;

function THorseCoreGroup.Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Delete(APath, ACallback);
end;

function THorseCoreGroup.Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup;
begin
  Result := Self;
  FHorseCore.Delete(APath, ACallbacks, ACallback);
end;

end.
