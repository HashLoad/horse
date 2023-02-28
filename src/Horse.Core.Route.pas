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
  Horse.Core.Route.Contract,
  Horse.Callback;

type
  THorseCoreRoute<T: class> = class(TInterfacedObject, IHorseCoreRoute<T>)
  private
    FPath: string;
    FHorseCore: TObject;
  public
    constructor Create(const APath: string);
    function This: IHorseCoreRoute<T>;
    function AddCallback(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    function AddCallbacks(const ACallbacks: TArray<THorseCallback>): IHorseCoreRoute<T>;
    function All(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Get(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Get(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
    function Put(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Put(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Put(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Put(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
    function Head(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Head(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Head(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Head(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
    function Post(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Post(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Post(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Post(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
{$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
    function Patch(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Delete(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Patch(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Patch(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Patch(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
    function Delete(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Delete(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Delete(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
{$IFEND}
    function &End: T;
  end;

implementation

uses
  Horse.Core;

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
  Result := Self;
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

function THorseCoreRoute<T>.Patch(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(FPath, ACallback);
end;

function THorseCoreRoute<T>.Delete(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Delete(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Delete(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Delete(FPath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Delete(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Delete(FPath, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Patch(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Patch(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Patch(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Patch(FPath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Patch(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Patch(FPath, ACallback);
  Result := Self;
end;
{$IFEND}
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

function THorseCoreRoute<T>.AddCallbacks(const ACallbacks: TArray<THorseCallback>): IHorseCoreRoute<T>;
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    AddCallback(LCallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Get(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Get(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Get(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Get(FPath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Get(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Get(FPath, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Head(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Head(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Head(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Head(FPath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Head(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Head(FPath, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Post(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Post(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Post(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Post(FPath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Post(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Post(FPath, ACallback);
  Result := Self;
end;
{$IFEND}

{$IFNDEF FPC}
function THorseCoreRoute<T>.Put(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Put(FPath, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Put(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Put(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Put(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore(FHorseCore).Put(FPath, ACallback);
  Result := Self;
end;

end.
