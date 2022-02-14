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
    FPrefix: string;
    function NormalizePath(APath: string): string;
  public
    constructor Create;

    function Prefix(APrefix: string): IHorseCoreGroup<T>;
    function Route(APath: string): IHorseCoreRoute<T>;

    function AddCallback(ACallback: THorseCallback): IHorseCoreGroup<T>;

    function Use(ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
    function Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
    function Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
    function Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
    function Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
    {$IFEND}

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

function THorseCoreGroup<T>.AddCallback(ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  result := Self;
  THorseCore(FHorseCore).AddCallback(ACallback);
end;

constructor THorseCoreGroup<T>.Create;
begin
  FHorseCore := THorseCore.GetInstance;
end;

function THorseCoreGroup<T>.Prefix(APrefix: string): IHorseCoreGroup<T>;
begin
  Result := Self;
  FPrefix := '/' + APrefix.Trim(['/']);
end;

function THorseCoreGroup<T>.Route(APath: string): IHorseCoreRoute<T>;
begin
  Result := THorseCore(FHorseCore).Route(NormalizePath(APath)) as IHorseCoreRoute<T>;
end;

function THorseCoreGroup<T>.Use(AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), [AMiddleware, ACallback]);
end;

function THorseCoreGroup<T>.Use(ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), ACallback);
end;

function THorseCoreGroup<T>.Use(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), ACallbacks);
  THorseCore(FHorseCore).Use(NormalizePath('/'), [ACallback]);
end;

function THorseCoreGroup<T>.Use(ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), ACallbacks);
end;

function THorseCoreGroup<T>.Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(NormalizePath(APath), ACallback);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function THorseCoreGroup<T>.Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(NormalizePath(APath), ACallback);
end;
{$IFEND}

function THorseCoreGroup<T>.NormalizePath(APath: string): string;
begin
  Result := FPrefix + '/' + APath.Trim(['/']);
end;

function THorseCoreGroup<T>.Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(NormalizePath(APath), ACallback);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(NormalizePath(APath), ACallback);
end;
{$IFEND}

end.
