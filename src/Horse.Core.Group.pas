unit Horse.Core.Group;
{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}
interface

uses
  {$IF DEFINED(FPC)}
  SysUtils,
  {$ELSE}
    System.SysUtils,
  {$ENDIF}
  Horse.Core.Group.Contract, Horse.Core.Route.Contract, Horse.Core.RouterTree;

type
  THorseCoreGroup<T: class> = class(TInterfacedObject, IHorseCoreGroup<T>)
  private
    FHorseCore: TObject;
    FRoutesOld: THorseRouterTree;
    FRoutesNew: THorseRouterTree;
    procedure RouterOld;
    procedure RouterNew;
  public
    constructor Create;

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

constructor THorseCoreGroup<T>.Create;
begin
  FHorseCore := THorseCore.GetInstance;
  FRoutesOld := THorseCore(FHorseCore).Routes;
  FRoutesNew := FRoutesOld.CreateRouter(FRoutesOld.GetPrefix().Trim(['/']));
  FRoutesNew.Prefix(EmptyStr);
end;

function THorseCoreGroup<T>.Prefix(APrefix: string): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Routes.Prefix(APrefix);
  RouterOld;
end;

function THorseCoreGroup<T>.Route(APath: string): IHorseCoreRoute<T>;
begin
  RouterNew;
  Result := THorseCore(FHorseCore).Route(APath) as IHorseCoreRoute<T>;
  RouterOld;
end;

function THorseCoreGroup<T>.Get(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Get(APath, AMiddleware, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Get(APath, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Get(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Get(APath, ACallbacks);
  RouterOld;
end;

function THorseCoreGroup<T>.Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Get(APath, ACallbacks, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Put(APath, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Put(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Put(APath, AMiddleware, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Put(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Put(APath, ACallbacks);
  RouterOld;
end;

function THorseCoreGroup<T>.Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Put(APath, ACallbacks, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Patch(APath, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Patch(APath, ACallbacks, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Patch(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Patch(APath, ACallbacks);
  RouterOld;
end;

function THorseCoreGroup<T>.Patch(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Patch(APath, AMiddleware, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Head(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Head(APath, ACallbacks);
  RouterOld;
end;

function THorseCoreGroup<T>.Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Head(APath, ACallbacks, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Head(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Head(APath, AMiddleware, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Head(APath, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Post(APath, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Post(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Post(APath, AMiddleware, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Post(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Post(APath, ACallbacks);
  RouterOld;
end;

function THorseCoreGroup<T>.Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Post(APath, ACallbacks, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Delete(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Delete(APath, AMiddleware, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Delete(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Delete(APath, ACallbacks);
  RouterOld;
end;

function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Delete(APath, ACallback);
  RouterOld;
end;

function THorseCoreGroup<T>.Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;

  RouterNew;
  THorseCore(FHorseCore).Delete(APath, ACallbacks, ACallback);
  RouterOld;
end;

procedure THorseCoreGroup<T>.RouterNew;
begin
  THorseCore(FHorseCore).Routes := FRoutesNew;
end;

procedure THorseCoreGroup<T>.RouterOld;
begin
  THorseCore(FHorseCore).Routes := FRoutesOld;
end;

end.
