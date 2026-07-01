unit Horse.Core.Group;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
  Generics.Collections,
  {$ENDIF}
  Horse.Core.Group.Contract,
  Horse.Core.Route.Contract,
  Horse.Callback,
  Horse.Core.Base;

type
  THorseCoreGroup<T: THorseCoreBase> = class(TInterfacedObject, IHorseCoreGroup<T>)
  private
    FHorseCore: T;
    FPrefix: string;
    function NormalizePath(const APath: string): string;
  public
    constructor Create;
    function Prefix(const APrefix: string): IHorseCoreGroup<T>;
    function Route(const APath: string): IHorseCoreRoute<T>;
    function AddCallback(const ACallback: THorseCallback): IHorseCoreGroup<T>;
    {$IF DEFINED(FPC)}
    function AddCallbacks(const ACallbacks: TList<THorseCallback>): IHorseCoreGroup<T>;
    {$ELSE}
    function AddCallbacks(const ACallbacks: TArray<THorseCallback>): IHorseCoreGroup<T>;
    {$ENDIF}
    function Use(const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(const AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(const ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function All(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function All(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function All(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function All(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
    function Get(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Get(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Get(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Get(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
    function Put(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Put(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Put(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Put(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
    function Head(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Head(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Head(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Head(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
    function Post(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Post(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Post(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Post(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
{$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Patch(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Patch(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
    function Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Delete(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Delete(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
{$IFEND}
    function &End: T;
  end;

implementation

uses
{$IF DEFINED(FPC)}
  SysUtils;
{$ELSE}
  System.SysUtils;
{$ENDIF}

function THorseCoreGroup<T>.&End: T;
begin
  Result := FHorseCore as T;
end;

function THorseCoreGroup<T>.Get(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseGet(NormalizePath(APath), ACallback);
  Result := Self;
end;

function THorseCoreGroup<T>.Get(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseGet(NormalizePath(APath), ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreGroup<T>.Get(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseGet(NormalizePath(APath), ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreGroup<T>.Post(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BasePost(NormalizePath(APath), ACallback);
  Result := Self;
end;

function THorseCoreGroup<T>.Post(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BasePost(NormalizePath(APath), ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreGroup<T>.Post(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BasePost(NormalizePath(APath), ACallback);
  Result := Self;
end;
{$ENDIF}

{$IFNDEF FPC}
function THorseCoreGroup<T>.Put(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BasePut(NormalizePath(APath), ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreGroup<T>.Put(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BasePut(NormalizePath(APath), ACallback);
  Result := Self;
end;

function THorseCoreGroup<T>.Put(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BasePut(NormalizePath(APath), ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreGroup<T>.Head(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseHead(NormalizePath(APath), ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreGroup<T>.Head(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseHead(NormalizePath(APath), ACallback);
  Result := Self;
end;

function THorseCoreGroup<T>.Head(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseHead(NormalizePath(APath), ACallback);
  Result := Self;
end;

function THorseCoreGroup<T>.AddCallback(const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseAddCallback(ACallback);
end;

{$IF DEFINED(FPC)}
function THorseCoreGroup<T>.AddCallbacks(const ACallbacks: TList<THorseCallback>): IHorseCoreGroup<T>;
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
function THorseCoreGroup<T>.AddCallbacks(const ACallbacks: TArray<THorseCallback>): IHorseCoreGroup<T>;
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    AddCallback(LCallback);
  Result := Self;
end;
{$ENDIF}

function THorseCoreGroup<T>.All(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseAll(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.All(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseAll(NormalizePath(APath), ACallback);
  Result := Self;
end;

function THorseCoreGroup<T>.All(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseAll(NormalizePath(APath), ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreGroup<T>.All(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseAll(NormalizePath(APath), ACallback);
  Result := Self;
end;
{$IFEND}

constructor THorseCoreGroup<T>.Create;
begin
  FHorseCore := T(GetHorseCoreInstance());
end;

function THorseCoreGroup<T>.Prefix(const APrefix: string): IHorseCoreGroup<T>;
begin
  Result := Self;
  FPrefix := '/' + APrefix.Trim(['/']);
end;

function THorseCoreGroup<T>.Route(const APath: string): IHorseCoreRoute<T>;
begin
  Result := THorseCoreBase(FHorseCore).BaseRoute(NormalizePath(APath)) as IHorseCoreRoute<T>;
end;

function THorseCoreGroup<T>.Use(const AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(NormalizePath('/'), [AMiddleware, ACallback]);
end;

function THorseCoreGroup<T>.Use(const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(NormalizePath('/'), ACallback);
end;

function THorseCoreGroup<T>.Use(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(NormalizePath('/'), ACallbacks);
  THorseCoreBase(FHorseCore).BaseUse(NormalizePath('/'), [ACallback]);
end;

function THorseCoreGroup<T>.Use(const ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(NormalizePath('/'), ACallbacks);
end;

function THorseCoreGroup<T>.Get(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseGet(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Put(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePut(NormalizePath(APath), ACallback);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function THorseCoreGroup<T>.Patch(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePatch(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Delete(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseDelete(NormalizePath(APath), ACallback);
end;

{$IFNDEF FPC}
function THorseCoreGroup<T>.Patch(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BasePatch(NormalizePath(APath), ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreGroup<T>.Patch(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BasePatch(NormalizePath(APath), ACallback);
  Result := Self;
end;

function THorseCoreGroup<T>.Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BasePatch(NormalizePath(APath), ACallback);
  Result := Self;
end;

{$IFNDEF FPC}
function THorseCoreGroup<T>.Delete(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseDelete(NormalizePath(APath), ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreGroup<T>.Delete(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseDelete(NormalizePath(APath), ACallback);
  Result := Self;
end;

function THorseCoreGroup<T>.Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  THorseCoreBase(FHorseCore).BaseDelete(NormalizePath(APath), ACallback);
  Result := Self;
end;
{$IFEND}

function THorseCoreGroup<T>.NormalizePath(const APath: string): string;
begin
  Result := FPrefix + '/' + APath.Trim(['/']);
end;

function THorseCoreGroup<T>.Head(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseHead(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Post(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePost(NormalizePath(APath), ACallback);
end;

end.
