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
  Horse.Core.RouterTree, Horse.Commons, Horse.HTTP,
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
    constructor Create(ASelfInstance, ADefaultHorseCoreInstance: PHorseCore; AHorseRouterTree: PHorseRouterTree);
  end;

  THorseCore = class
  private
    { private declarations }
    class var FRoutes: THorseRouterTree;
    class var FCallbacks: TList<THorseCallback>;
    class function RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback): THorseCore;
    class var FDefaultHorse: THorseCore;

    function InternalRoute(APath: string): IHorseCoreRoute<THorseCore>;
    function InternalGroup(): IHorseCoreGroup<THorseCore>;
    function InternalGetRoutes: THorseRouterTree;
    procedure InternalSetRoutes(const Value: THorseRouterTree);
    class function GetRoutes: THorseRouterTree; static;
    class procedure SetRoutes(const Value: THorseRouterTree); static;
    class function MakeHorseModule: THorseModule;

    class function GetCallback(ACallbackRequest: THorseCallbackRequestResponse): THorseCallback; overload;
    class function GetCallback(ACallbackRequest: THorseCallbackRequest): THorseCallback; overload;
    class function GetCallback(ACallbackResponse: THorseCallbackResponse): THorseCallback; overload;
    class function GetCallbacks: TArray<THorseCallback>;

  protected
    class function GetDefaultHorse: THorseCore;
  public
    { public declarations }
    class function ToModule: THorseModule;
    constructor Create; virtual;
    class destructor UnInitialize; {$IFNDEF FPC}virtual; {$ENDIF}
    class function AddCallback(ACallback: THorseCallback): THorseCore;
    class function Group(): IHorseCoreGroup<THorseCore>;
    class function Route(APath: string): IHorseCoreRoute<THorseCore>;
    class function Use(APath: string; ACallback: THorseCallback): THorseCore; overload;
    class function Use(ACallback: THorseCallback): THorseCore; overload;
    class function Use(APath: string; ACallbacks: array of THorseCallback): THorseCore; overload;
    class function Use(ACallbacks: array of THorseCallback): THorseCore; overload;

    class function Get(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Get(APath: string; ACallback: THorseCallbackRequest): THorseCore; overload;
    class function Get(APath: string; ACallback: THorseCallbackResponse): THorseCore; overload;
    class function Get(APath: string; ACallback: THorseCallback): THorseCore; overload;

    class function Put(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Put(APath: string; ACallback: THorseCallbackRequest): THorseCore; overload;
    class function Put(APath: string; ACallback: THorseCallbackResponse): THorseCore; overload;
    class function Put(APath: string; ACallback: THorseCallback): THorseCore; overload;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    class function Patch(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Patch(APath: string; ACallback: THorseCallbackRequest): THorseCore; overload;
    class function Patch(APath: string; ACallback: THorseCallbackResponse): THorseCore; overload;
    class function Patch(APath: string; ACallback: THorseCallback): THorseCore; overload;
    {$IFEND}

    class function Head(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Head(APath: string; ACallback: THorseCallbackRequest): THorseCore; overload;
    class function Head(APath: string; ACallback: THorseCallbackResponse): THorseCore; overload;
    class function Head(APath: string; ACallback: THorseCallback): THorseCore; overload;

    class function Post(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Post(APath: string; ACallback: THorseCallbackRequest): THorseCore; overload;
    class function Post(APath: string; ACallback: THorseCallbackResponse): THorseCore; overload;
    class function Post(APath: string; ACallback: THorseCallback): THorseCore; overload;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    class function Delete(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore; overload;
    class function Delete(APath: string; ACallback: THorseCallbackRequest): THorseCore; overload;
    class function Delete(APath: string; ACallback: THorseCallbackResponse): THorseCore; overload;
    class function Delete(APath: string; ACallback: THorseCallback): THorseCore; overload;
    {$IFEND}

    class property Routes: THorseRouterTree read GetRoutes write SetRoutes;

    class function GetInstance: THorseCore;
  end;

implementation

uses
  Horse.Core.Route, Horse.Core.Group;

{ THorseCore }

class function THorseCore.AddCallback(ACallback: THorseCallback): THorseCore;
begin
  result := GetDefaultHorse;
  if FCallbacks = nil then
    FCallbacks := TList<THorseCallback>.create;
  FCallbacks.Add(ACallback);
end;

constructor THorseCore.Create;
begin
  if FDefaultHorse <> nil then
    raise Exception.Create('The Horse instance has already been created');
  if FRoutes = nil then
    FRoutes := THorseRouterTree.Create;
  FDefaultHorse := Self
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

class function THorseCore.GetCallbacks: TArray<THorseCallback>;
begin
  result := [];
  if Assigned(FCallbacks) then
  begin
    result := FCallbacks.ToArray;
    FCallbacks.Clear;
  end;
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
  APath := '/' + APath.Trim(['/']);
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

class function THorseCore.ToModule: THorseModule;
begin
  Result := GetDefaultHorse.MakeHorseModule;
end;

function THorseCore.InternalGetRoutes: THorseRouterTree;
begin
  Result := FRoutes;
end;

function THorseCore.InternalGroup(): IHorseCoreGroup<THorseCore>;
begin
  Result := THorseCoreGroup<THorseCore>.Create;
end;

function THorseCore.InternalRoute(APath: string): IHorseCoreRoute<THorseCore>;
begin
  Result := THorseCoreRoute<THorseCore>.Create(APath);
end;

procedure THorseCore.InternalSetRoutes(const Value: THorseRouterTree);
begin
  FRoutes := Value;
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
class function THorseCore.Delete(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  result := Delete(APath, GetCallback(ACallback));
end;

class function THorseCore.Delete(APath: string; ACallback: THorseCallbackRequest): THorseCore;
begin
  result := Delete(APath, GetCallback(ACallback));
end;

class function THorseCore.Delete(APath: string; ACallback: THorseCallbackResponse): THorseCore;
begin
  result := Delete(APath, GetCallback(ACallback));
end;

class function THorseCore.Delete(APath: string; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in GetCallbacks do
    RegisterRoute(mtDelete, APath, LCallback);
  RegisterRoute(mtDelete, APath, ACallback);
end;
{$IFEND}

class function THorseCore.Head(APath: string; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in GetCallbacks do
    RegisterRoute(mtHead, APath, LCallback);
  RegisterRoute(mtHead, APath, ACallback);
end;

class function THorseCore.Get(APath: string; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in GetCallbacks do
    RegisterRoute(mtGet, APath, LCallback);
  RegisterRoute(mtGet, APath, ACallback);
end;

class function THorseCore.Post(APath: string; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in GetCallbacks do
    RegisterRoute(mtPost, APath, LCallback);
  RegisterRoute(mtPost, APath, ACallback);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
class function THorseCore.Patch(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  result := Patch(APath, GetCallback(ACallback));
end;

class function THorseCore.Patch(APath: string; ACallback: THorseCallbackRequest): THorseCore;
begin
  result := Patch(APath, GetCallback(ACallback));
end;

class function THorseCore.Patch(APath: string; ACallback: THorseCallbackResponse): THorseCore;
begin
  result := Patch(APath, GetCallback(ACallback));
end;

class function THorseCore.Patch(APath: string; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in GetCallbacks do
    RegisterRoute(mtPatch, APath, LCallback);
  RegisterRoute(mtPatch, APath, ACallback);
end;
{$IFEND}

class function THorseCore.Put(APath: string; ACallback: THorseCallback): THorseCore;
var
  LCallback: THorseCallback;
begin
  Result := GetDefaultHorse;
  for LCallback in GetCallbacks do
    RegisterRoute(mtPut, APath, LCallback);
  RegisterRoute(mtPut, APath, ACallback);
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
  APath := '/' + APath.Trim(['/']);
  Result.Routes.RegisterMiddleware(APath, ACallback);
end;

class function THorseCore.Head(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  result := Head(APath, GetCallback(ACallback));
end;

class function THorseCore.Head(APath: string; ACallback: THorseCallbackRequest): THorseCore;
begin
  result := Head(APath, GetCallback(ACallback));
end;

class function THorseCore.Head(APath: string; ACallback: THorseCallbackResponse): THorseCore;
begin
  result := Head(APath, GetCallback(ACallback));
end;

class function THorseCore.Get(APath: string; ACallback: THorseCallbackRequest): THorseCore;
begin
  result := Get(APath, GetCallback(ACallback));
end;

class function THorseCore.GetCallback(ACallbackRequest: THorseCallbackRequestResponse): THorseCallback;
begin
  result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      ACallbackRequest(Req, Res);
    end;
end;

class function THorseCore.GetCallback(ACallbackResponse: THorseCallbackResponse): THorseCallback;
begin
  result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      ACallbackResponse(Res);
    end;
end;

class function THorseCore.GetCallback(ACallbackRequest: THorseCallbackRequest): THorseCallback;
begin
  result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Status(THTTPStatus.NoContent);
      ACallbackRequest(Req);
    end;
end;

class function THorseCore.Get(APath: string; ACallback: THorseCallbackResponse): THorseCore;
begin
  result := Get(APath, GetCallback(ACallback));
end;

class function THorseCore.Get(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  result := Get(APath, GetCallback(ACallback));
end;

class function THorseCore.Post(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  result := Post(APath, GetCallback(ACallback));
end;

class function THorseCore.Post(APath: string; ACallback: THorseCallbackRequest): THorseCore;
begin
  result := Post(APath, GetCallback(ACallback));
end;

class function THorseCore.Post(APath: string; ACallback: THorseCallbackResponse): THorseCore;
begin
  result := Post(APath, GetCallback(ACallback));
end;

class function THorseCore.Put(APath: string; ACallback: THorseCallbackRequestResponse): THorseCore;
begin
  result := Put(APath, GetCallback(ACallback));
end;

class function THorseCore.Put(APath: string; ACallback: THorseCallbackRequest): THorseCore;
begin
  result := Put(APath, GetCallback(ACallback));
end;

class function THorseCore.Put(APath: string; ACallback: THorseCallbackResponse): THorseCore;
begin
  result := Put(APath, GetCallback(ACallback));
end;

{ THorseModule }

constructor THorseModule.Create(ASelfInstance, ADefaultHorseCoreInstance: PHorseCore; AHorseRouterTree: PHorseRouterTree);
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
