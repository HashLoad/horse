unit Horse.Core.Route;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Generics.Collections,
{$ENDIF}
  Horse.Core.Route.Contract,
  Horse.Callback,
  Horse.Core.Base;

type
  THorseCoreRoute<T: THorseCoreBase> = class(TInterfacedObject, IHorseCoreRoute<T>)
  private
    FPath: string;
    FHorseCore: T;
  public
    constructor Create(const APath: string);
    function This: IHorseCoreRoute<T>;
    function AddCallback(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    {$IF DEFINED(FPC)}
    function AddCallbacks(const ACallbacks: TList<THorseCallback>): IHorseCoreRoute<T>;
    {$ELSE}
    function AddCallbacks(const ACallbacks: TArray<THorseCallback>): IHorseCoreRoute<T>;
    {$ENDIF}
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

    function Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}

    function Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}

    function Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}

    function Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}

{$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
    function Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}

    function Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
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
  FHorseCore := T(GetHorseCoreInstance());
end;

function THorseCoreRoute<T>.This: IHorseCoreRoute<T>;
begin
  Result := Self;
end;

function THorseCoreRoute<T>.All(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(FPath, ACallback);
end;

function THorseCoreRoute<T>.All(const AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(FPath, [AMiddleware, ACallback]);
end;

function THorseCoreRoute<T>.All(const ACallbacks: array of THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(FPath, ACallbacks);
end;

function THorseCoreRoute<T>.&End: T;
begin
  Result := FHorseCore as T;
end;

function THorseCoreRoute<T>.AddCallback(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseAddCallback(ACallback);
end;

function THorseCoreRoute<T>.All(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(FPath, ACallbacks);
  THorseCoreBase(FHorseCore).BaseUse(FPath, [ACallback]);
end;

{$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
function THorseCoreRoute<T>.Delete(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseDelete(FPath, ACallback);
end;

function THorseCoreRoute<T>.Patch(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePatch(FPath, ACallback);
end;

function THorseCoreRoute<T>.Delete(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BaseDelete(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Delete(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BaseDelete(FPath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Delete(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BaseDelete(FPath, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Patch(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BasePatch(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Patch(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BasePatch(FPath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Patch(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BasePatch(FPath, ACallback);
  Result := Self;
end;
{$IFEND}
{$IFEND}

function THorseCoreRoute<T>.Get(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseGet(FPath, ACallback);
end;

function THorseCoreRoute<T>.Head(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseHead(FPath, ACallback);
end;

function THorseCoreRoute<T>.Post(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePost(FPath, ACallback);
end;

function THorseCoreRoute<T>.Put(const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePut(FPath, ACallback);
end;

{$IF DEFINED(FPC)}
function THorseCoreRoute<T>.AddCallbacks(const ACallbacks: TList<THorseCallback>): IHorseCoreRoute<T>;
var
  LCallback: THorseCallback;
begin
  if Assigned(ACallbacks) then
  begin
    for LCallback in ACallbacks do
      AddCallback(LCallback);
  end;
  Result := Self;
end;
{$ELSE}
function THorseCoreRoute<T>.AddCallbacks(const ACallbacks: TArray<THorseCallback>): IHorseCoreRoute<T>;
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    AddCallback(LCallback);
  Result := Self;
end;
{$ENDIF}

function THorseCoreRoute<T>.Get(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BaseGet(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Get(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BaseGet(FPath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Get(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BaseGet(FPath, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Head(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BaseHead(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Head(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BaseHead(FPath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Head(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BaseHead(FPath, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Post(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BasePost(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Post(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BasePost(FPath, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Post(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BasePost(FPath, ACallback);
  Result := Self;
end;
{$IFEND}

{$IFNDEF FPC}
function THorseCoreRoute<T>.Put(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BasePut(FPath, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Put(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BasePut(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Put(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCoreBase(FHorseCore).BasePut(FPath, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  THorseCore.Get(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Get(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore.Get(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Get(FPath, AMiddlewares, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  THorseCore.Put(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Put(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore.Put(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Put(FPath, AMiddlewares, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  THorseCore.Head(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Head(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore.Head(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Head(FPath, AMiddlewares, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  THorseCore.Post(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Post(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

// Corrigido typo do post request
function THorseCoreRoute<T>.Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore.Post(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Post(FPath, AMiddlewares, ACallback);
  Result := Self;
end;
{$IFEND}

{$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
function THorseCoreRoute<T>.Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  THorseCore.Patch(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Patch(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore.Patch(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreRoute<T>.Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Patch(FPath, AMiddlewares, ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreRoute<T>.Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>;
begin
  THorseCore.Delete(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreRoute<T>.Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Delete(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

// Corrigido typo do delete request
function THorseCoreRoute<T>.Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>;
begin
  THorseCore.Delete(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
// Corrigido typo do delete response
function THorseCoreRoute<T>.Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>;
begin
  THorseCore.Delete(FPath, AMiddlewares, ACallback);
  Result := Self;
end;
{$IFEND}
{$IFEND}

end.
