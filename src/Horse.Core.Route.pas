unit Horse.Core.Route;

interface

uses
  Horse.Core.Route.Intf, Horse.Router, Horse.Core;

type
  THorseCoreRoute = class(TInterfacedObject, IHorseCoreRoute)
  private
    FPath: string;
    FHorseCore: THorseCore;
  public
    constructor Create(APath: string; AHorseCore: THorseCore);

    function Get(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Get(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Get(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Get(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;

    function Put(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Put(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Put(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Put(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;

    function Patch(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Patch(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Patch(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Patch(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;

    function Head(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Head(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Head(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Head(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;

    function Post(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Post(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Post(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Post(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;

    function Delete(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Delete(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Delete(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Delete(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;
  end;

implementation

{ THorseCoreRoute }

constructor THorseCoreRoute.Create(APath: string; AHorseCore: THorseCore);
begin
  FPath := APath;
  FHorseCore := AHorseCore;
end;

function THorseCoreRoute.Delete(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Delete(FPath, ACallbacks);
end;

function THorseCoreRoute.Delete(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Delete(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute.Delete(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Delete(FPath, ACallback);
end;

function THorseCoreRoute.Delete(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Delete(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute.Get(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Get(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute.Get(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Get(FPath, ACallback, ACallback);
end;

function THorseCoreRoute.Get(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Get(FPath, ACallbacks);
end;

function THorseCoreRoute.Get(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Get(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute.Head(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Head(FPath, ACallbacks);
end;

function THorseCoreRoute.Head(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Head(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute.Head(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Head(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute.Head(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Head(FPath, ACallback);
end;

function THorseCoreRoute.Patch(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Patch(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute.Patch(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Patch(FPath, ACallbacks);
end;

function THorseCoreRoute.Patch(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Patch(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute.Patch(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Patch(FPath, ACallback);
end;

function THorseCoreRoute.Post(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Post(FPath, ACallback);
end;

function THorseCoreRoute.Post(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Post(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute.Post(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Post(FPath, ACallbacks);
end;

function THorseCoreRoute.Post(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Post(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute.Put(ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Put(FPath, ACallback);
end;

function THorseCoreRoute.Put(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Put(FPath, ACallback);
end;

function THorseCoreRoute.Put(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Put(FPath, ACallback);
end;

function THorseCoreRoute.Put(ACallbacks: array of THorseCallback): IHorseCoreRoute;
begin
  Result := Self;
  FHorseCore.Put(FPath, ACallbacks);
end;

end.
