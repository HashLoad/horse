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

    function All(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function All(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    function Get(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    function Put(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Put(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Put(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Put(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Patch(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Patch(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Patch(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    {$IFEND}

    function Head(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Head(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Head(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Head(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    function Post(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Post(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Post(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Post(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Delete(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Delete(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Delete(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Delete(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    {$IFEND}

    function &End: T;
  end;

implementation

uses
  Horse.Core;

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

function THorseCoreRoute<T>.All(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(FPath, ACallbacks);
  THorseCore(FHorseCore).Use(FPath, [ACallback]);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function THorseCoreRoute<T>.Delete(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(FPath, ACallbacks);
end;

function THorseCoreRoute<T>.Delete(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute<T>.Delete(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(FPath, ACallback);
end;

function THorseCoreRoute<T>.Delete(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(FPath, ACallbacks, ACallback);
end;
{$IFEND}

function THorseCoreRoute<T>.Get(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute<T>.Get(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(FPath, ACallback);
end;

function THorseCoreRoute<T>.Get(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(FPath, ACallbacks);
end;

function THorseCoreRoute<T>.Get(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute<T>.Head(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(FPath, ACallbacks);
end;

function THorseCoreRoute<T>.Head(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute<T>.Head(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute<T>.Head(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(FPath, ACallback);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function THorseCoreRoute<T>.Patch(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute<T>.Patch(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(FPath, ACallbacks);
end;

function THorseCoreRoute<T>.Patch(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(FPath, AMiddleware, ACallback);
end;

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

function THorseCoreRoute<T>.Post(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute<T>.Post(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(FPath, ACallbacks);
end;

function THorseCoreRoute<T>.Post(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute<T>.Put(ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(FPath, ACallback);
end;

function THorseCoreRoute<T>.Put(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(FPath, AMiddleware, ACallback);
end;

function THorseCoreRoute<T>.Put(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(FPath, ACallbacks, ACallback);
end;

function THorseCoreRoute<T>.Put(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(FPath, ACallbacks);
end;

end.
