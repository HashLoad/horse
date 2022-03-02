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
  Horse.Core.Route.Contract, Horse.Core.RouterTree, Horse.Callback;

type
  THorseCoreRoute<T: class> = class(TInterfacedObject, IHorseCoreRoute<T>)
  private
    FPath: string;
    FHorseCore: TObject;
  public
    constructor Create(const APath: string);

    function This: IHorseCoreRoute<T>;
    function AddCallback(const ACallback: THorseCallback): IHorseCoreRoute<T>;

    function All(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    function Get(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Put(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Head(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Post(const ACallback: THorseCallback): IHorseCoreRoute<T>;

    {$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
    function Patch(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Delete(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    {$IFEND}

    function &End: T;
  end;

implementation

uses Horse.Core;

constructor THorseCoreRoute<T>.Create(const APath: string);
begin
  FPath := APath;
  FHorseCore := THorseCore.GetInstance;
end;

function THorseCoreRoute<T>.This: IHorseCoreRoute<T>;
begin
  Result := Self;
end;

function THorseCoreRoute<T>.All(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(FPath, ACallback);
end;

function THorseCoreRoute<T>.All(const AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(FPath, [AMiddleware, ACallback]);
end;

function THorseCoreRoute<T>.All(const ACallbacks: array of THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(FPath, ACallbacks);
end;

function THorseCoreRoute<T>.&End: T;
begin
  Result := FHorseCore as T;
end;

function THorseCoreRoute<T>.AddCallback(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  result := Self;
  THorseCore(FHorseCore).AddCallback(ACallback);
end;

function THorseCoreRoute<T>.All(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(FPath, ACallbacks);
  THorseCore(FHorseCore).Use(FPath, [ACallback]);
end;

{$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
function THorseCoreRoute<T>.Delete(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(FPath, ACallback);
end;
{$IFEND}

function THorseCoreRoute<T>.Get(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(FPath, ACallback);
end;

function THorseCoreRoute<T>.Head(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(FPath, ACallback);
end;

{$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
function THorseCoreRoute<T>.Patch(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(FPath, ACallback);
end;
{$IFEND}

function THorseCoreRoute<T>.Post(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(FPath, ACallback);
end;

function THorseCoreRoute<T>.Put(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(FPath, ACallback);
end;

end.
