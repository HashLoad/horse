unit Horse.Core;

interface

uses System.SysUtils, Web.HTTPApp, Horse.Router;

type
  THorseCore = class;

  IHorseCoreRoute = interface
    ['{39ABE601-E812-40B4-8857-F14C938B9773}']
    function Get(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Get(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Get(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Get(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;

    function Put(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Put(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Put(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Put(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;

    function Patch(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Patch(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Patch(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Patch(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;

    function Head(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Head(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Head(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Head(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;

    function Post(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Post(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Post(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Post(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;

    function Delete(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Delete(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Delete(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Delete(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;
  end;

  THorseCoreRoute = class(TInterfacedObject, IHorseCoreRoute)
  private
    FPath:string;
    FHorseCore:THorseCore;

  public
    constructor Create(APath:string; AHorseCore: THorseCore);

    function Get(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Get(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Get(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Get(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;

    function Put(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Put(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Put(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Put(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;

    function Patch(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Patch(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Patch(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Patch(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;

    function Head(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Head(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Head(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Head(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;

    function Post(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Post(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Post(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Post(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;

    function Delete(ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Delete(AMiddleware, ACallback: THorseCallback):IHorseCoreRoute; overload;
    function Delete(ACallbacks: array of THorseCallback):IHorseCoreRoute; overload;
    function Delete(ACallbacks: array of THorseCallback; ACallback: THorseCallback):IHorseCoreRoute; overload;
  end;

  THorseCore = class
  private
    FRoutes: THorseRouterTree;
    procedure RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
    class var FInstance: THorseCore;
  protected
    procedure Initialize; virtual;
  public
    destructor Destroy; override;
    constructor Create; overload;
    class destructor UnInitialize;

    property Routes: THorseRouterTree read FRoutes write FRoutes;
    function Route(APath:string):IHorseCoreRoute;

    procedure Use(APath: string; ACallback: THorseCallback); overload;
    procedure Use(ACallback: THorseCallback); overload;
    procedure Use(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Use(ACallbacks: array of THorseCallback); overload;

    procedure Get(APath: string; ACallback: THorseCallback); overload;
    procedure Get(APath: string; AMiddleware, ACallback: THorseCallback); overload;
    procedure Get(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback); overload;

    procedure Put(APath: string; ACallback: THorseCallback); overload;
    procedure Put(APath: string; AMiddleware, ACallback: THorseCallback); overload;
    procedure Put(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback); overload;

    procedure Patch(APath: string; ACallback: THorseCallback); overload;
    procedure Patch(APath: string; AMiddleware, ACallback: THorseCallback); overload;
    procedure Patch(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback); overload;

    procedure Head(APath: string; ACallback: THorseCallback); overload;
    procedure Head(APath: string; AMiddleware, ACallback: THorseCallback); overload;
    procedure Head(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback); overload;

    procedure Post(APath: string; ACallback: THorseCallback); overload;
    procedure Post(APath: string; AMiddleware, ACallback: THorseCallback); overload;
    procedure Post(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback); overload;

    procedure Delete(APath: string; ACallback: THorseCallback); overload;
    procedure Delete(APath: string; AMiddleware, ACallback: THorseCallback); overload;
    procedure Delete(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback); overload;

    procedure Start; virtual; abstract;
    class function GetInstance: THorseCore;
  end;

implementation

{ THorseCore }

constructor THorseCore.Create;
begin
  Initialize;
end;

procedure THorseCore.Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Delete(APath, LCallback);
  Delete(APath, ACallback);
end;

destructor THorseCore.Destroy;
begin
  FRoutes.free;
  inherited;
end;

procedure THorseCore.Delete(APath: string; AMiddleware, ACallback: THorseCallback);
begin
  Delete(APath, [AMiddleware, ACallback]);
end;

procedure THorseCore.Delete(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Delete(APath, LCallback);
end;

procedure THorseCore.Delete(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtDelete, APath, ACallback);
end;

procedure THorseCore.Initialize;
begin
  FInstance := Self;
  FRoutes := THorseRouterTree.Create;
end;

procedure THorseCore.Head(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtHead, APath, ACallback);
end;

procedure THorseCore.Get(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtGet, APath, ACallback);
end;

procedure THorseCore.Head(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Head(APath, LCallback);
end;

procedure THorseCore.Get(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Get(APath, LCallback);
end;

procedure THorseCore.Head(APath: string; AMiddleware, ACallback: THorseCallback);
begin
  Head(APath, [AMiddleware, ACallback]);
end;

procedure THorseCore.Get(APath: string; AMiddleware, ACallback: THorseCallback);
begin
  Get(APath, [AMiddleware, ACallback]);
end;

class function THorseCore.GetInstance: THorseCore;
begin
  Result := FInstance;
end;

procedure THorseCore.Post(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtPost, APath, ACallback);
end;

procedure THorseCore.Patch(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtPatch, APath, ACallback);
end;

procedure THorseCore.Put(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtPut, APath, ACallback);
end;

procedure THorseCore.RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
begin
  if APath.EndsWith('/') then
    APath := APath.Remove(High(APath) - 1, 1);
  if not APath.StartsWith('/') then
    APath := '/' + APath;
  FRoutes.RegisterRoute(AHTTPType, APath, ACallback);
end;

function THorseCore.Route(APath: string): IHorseCoreRoute;
begin
  Result:= THorseCoreRoute.Create(APath, Self);
end;

class destructor THorseCore.UnInitialize;
begin
  if Assigned(FInstance) then
     FInstance.Free;
end;

procedure THorseCore.Use(ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Use(LCallback);
end;

procedure THorseCore.Use(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Use(APath, LCallback);
end;

procedure THorseCore.Use(ACallback: THorseCallback);
begin
  FRoutes.RegisterMiddleware('/', ACallback);
end;

procedure THorseCore.Use(APath: string; ACallback: THorseCallback);
begin
  FRoutes.RegisterMiddleware(APath, ACallback);
end;

procedure THorseCore.Post(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Post(APath, LCallback);
end;

procedure THorseCore.Post(APath: string; AMiddleware, ACallback: THorseCallback);
begin
  Post(APath, [AMiddleware, ACallback]);
end;

procedure THorseCore.Patch(APath: string; AMiddleware, ACallback: THorseCallback);
begin
  Patch(APath, [AMiddleware, ACallback]);
end;

procedure THorseCore.Put(APath: string; AMiddleware, ACallback: THorseCallback);
begin
  Put(APath, [AMiddleware, ACallback]);
end;

procedure THorseCore.Patch(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Patch(APath, LCallback);
end;

procedure THorseCore.Put(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Put(APath, LCallback);
end;

procedure THorseCore.Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Head(APath, LCallback);
  Head(APath, ACallback);
end;

procedure THorseCore.Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Get(APath, LCallback);
  Get(APath, ACallback);
end;

procedure THorseCore.Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Post(APath, LCallback);
  Post(APath, ACallback);
end;

procedure THorseCore.Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Patch(APath, LCallback);
  Patch(APath, ACallback);
end;

procedure THorseCore.Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Put(APath, LCallback);
  Put(APath, ACallback);
end;

{ THorseCoreRoute }

constructor THorseCoreRoute.Create(APath:string; AHorseCore: THorseCore);
begin
  FPath      := APath;
  FHorseCore := AHorseCore;
end;

function THorseCoreRoute.Delete(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Delete(FPath, ACallbacks);
end;

function THorseCoreRoute.Delete(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Delete(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute.Delete(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Delete(FPath, ACallback);
end;

function THorseCoreRoute.Delete(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Delete(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute.Get(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Get(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute.Get(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Get(FPath, ACallback, ACallback);
end;

function THorseCoreRoute.Get(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Get(FPath, ACallbacks);
end;

function THorseCoreRoute.Get(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Get(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute.Head(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Head(FPath, ACallbacks);
end;

function THorseCoreRoute.Head(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Head(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute.Head(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Head(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute.Head(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Head(FPath, ACallback);
end;

function THorseCoreRoute.Patch(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Patch(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute.Patch(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Patch(FPath, ACallbacks);
end;

function THorseCoreRoute.Patch(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Patch(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute.Patch(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Patch(FPath, ACallback);
end;

function THorseCoreRoute.Post(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Post(FPath, ACallback);
end;

function THorseCoreRoute.Post(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Post(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute.Post(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Post(FPath, ACallbacks);
end;

function THorseCoreRoute.Post(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Post(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute.Put(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Put(FPath, ACallback);
end;

function THorseCoreRoute.Put(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Put(FPath, ACallback);
end;

function THorseCoreRoute.Put(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Put(FPath, ACallback);
end;

function THorseCoreRoute.Put(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result:= Self;
  FHorseCore.Put(FPath, ACallbacks);
end;

end.
