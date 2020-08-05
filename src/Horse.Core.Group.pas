unit Horse.Core.Group;

interface

uses
  System.SysUtils, Horse.Core.Group.Contract, Horse.Core.Route.Contract, Horse.Core.RouterTree;

type

  THorseCoreGroup<T: class, constructor> = class(TInterfacedObject, IHorseCoreGroup<T>)
  private
    FHorseCore: TObject;
  public
    constructor Create(ARoutes: THorseRouterTree);

    function Prefix(APrefix: string): IHorseCoreGroup<T>;
    function Route(APath: string): IHorseCoreRoute<T>;

    function Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Get(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Get(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Put(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Put(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Patch(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Patch(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Head(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Head(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Post(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Post(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function &End: T;
  end;

implementation

uses
  Horse.Core;

{ THorseCoreGroup }

function THorseCoreGroup<T>.&End: T;
begin
  Result := FHorseCore as T;
end;

constructor THorseCoreGroup<T>.Create(ARoutes: THorseRouterTree);
begin
  FHorseCore := THorseCore.Create;
  THorseCore(FHorseCore).Routes := ARoutes.CreateRouter(ARoutes.GetPrefix().Trim(['/']));
  THorseCore(FHorseCore).Routes.Prefix(EmptyStr);
end;

function THorseCoreGroup<T>.Prefix(APrefix: string): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Routes.Prefix(APrefix);
end;

function THorseCoreGroup<T>.Route(APath: string): IHorseCoreRoute<T>;
begin
  Result := THorseCore(FHorseCore).Route(APath) as IHorseCoreRoute<T>;
end;

function THorseCoreGroup<T>.Get(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup<T>.Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(APath, ACallback);
end;

function THorseCoreGroup<T>.Get(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(APath, ACallbacks);
end;

function THorseCoreGroup<T>.Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(APath, ACallbacks, ACallback);
end;

function THorseCoreGroup<T>.Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(APath, ACallback);
end;

function THorseCoreGroup<T>.Put(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup<T>.Put(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(APath, ACallbacks);
end;

function THorseCoreGroup<T>.Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(APath, ACallbacks, ACallback);
end;

function THorseCoreGroup<T>.Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(APath, ACallback);
end;

function THorseCoreGroup<T>.Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(APath, ACallbacks, ACallback);
end;

function THorseCoreGroup<T>.Patch(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(APath, ACallbacks);
end;

function THorseCoreGroup<T>.Patch(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup<T>.Head(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(APath, ACallbacks);
end;

function THorseCoreGroup<T>.Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(APath, ACallbacks, ACallback);
end;

function THorseCoreGroup<T>.Head(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup<T>.Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(APath, ACallback);
end;

function THorseCoreGroup<T>.Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(APath, ACallback);
end;

function THorseCoreGroup<T>.Post(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup<T>.Post(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(APath, ACallbacks);
end;

function THorseCoreGroup<T>.Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(APath, ACallbacks, ACallback);
end;

function THorseCoreGroup<T>.Delete(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(APath, AMiddleware, ACallback);
end;

function THorseCoreGroup<T>.Delete(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(APath, ACallbacks);
end;

function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(APath, ACallback);
end;

function THorseCoreGroup<T>.Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(APath, ACallbacks, ACallback);
end;

end.
