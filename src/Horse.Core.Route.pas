unit Horse.Core.Route;

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
  Horse.Core.Route.Contract, Horse.Core.RouterTree;

type
  THorseCoreRoute<T: class> = class(TInterfacedObject, IHorseCoreRoute<T>)
  private
    FPath: string;
    FHorseCore: TObject;
  public
    constructor Create(APath: string);

    function This: IHorseCoreRoute<T>;

    function AddCallback(ACallback: THorseCallback): IHorseCoreRoute<T>;

    function All(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function All(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    function Get(ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Put(ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Head(ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Post(ACallback: THorseCallback): IHorseCoreRoute<T>;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Delete(ACallback: THorseCallback): IHorseCoreRoute<T>;
    {$IFEND}

    function &End: T;
  end;

implementation

uses Horse.Core;

{ THorseCoreRoute }

constructor THorseCoreRoute<T>.Create(APath: string);
begin
  FPath := APath;
  FHorseCore := THorseCore.GetInstance;
end;

function THorseCoreRoute<T>.This: IHorseCoreRoute<T>;
begin
  Result := Self;
end;

function THorseCoreRoute<T>.All(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(FPath, ACallback);
end;

function THorseCoreRoute<T>.All(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(FPath, [AMiddleware, ACallback]);
end;

function THorseCoreRoute<T>.All(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(FPath, ACallbacks);
end;

function THorseCoreRoute<T>.&End: T;
begin
  Result := FHorseCore as T;
end;

function THorseCoreRoute<T>.AddCallback(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  result := Self;
  THorseCore(FHorseCore).AddCallback(ACallback);
end;

function THorseCoreRoute<T>.All(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(FPath, ACallbacks);
  THorseCore(FHorseCore).Use(FPath, [ACallback]);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function THorseCoreRoute<T>.Delete(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(FPath, ACallback);
end;
{$IFEND}

function THorseCoreRoute<T>.Get(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(FPath, ACallback);
end;

function THorseCoreRoute<T>.Head(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(FPath, ACallback);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function THorseCoreRoute<T>.Patch(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(FPath, ACallback);
end;
{$IFEND}

function THorseCoreRoute<T>.Post(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(FPath, ACallback);
end;

function THorseCoreRoute<T>.Put(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(FPath, ACallback);
end;

end.
