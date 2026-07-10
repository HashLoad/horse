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
    {$IF DEFINED(FPC)}
    function Route(const APath: string): IHorseCoreRoute<T>;
    {$ELSE}
    function Route(const APath: string): IHorseCoreRoute<IHorseCoreGroup<T>>;
    {$ENDIF}
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

  {$IFNDEF FPC}
  THorseCoreGroupRoute<T: THorseCoreBase> = class(TInterfacedObject, IHorseCoreRoute<IHorseCoreGroup<T>>)
  private
    FPath: string;
    FGroup: IHorseCoreGroup<T>;
    FHorseCore: T;
  public
    constructor Create(const AGroup: IHorseCoreGroup<T>; const AHorseCore: T; const APath: string);
    function AddCallback(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
    {$IF DEFINED(FPC)}
    function AddCallbacks(const ACallbacks: TList<THorseCallback>): IHorseCoreRoute<IHorseCoreGroup<T>>;
    {$ELSE}
    function AddCallbacks(const ACallbacks: TArray<THorseCallback>): IHorseCoreRoute<IHorseCoreGroup<T>>;
    {$ENDIF}
    function All(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function All(const AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function All(const ACallbacks: array of THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function All(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Get(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Get(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Get(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$IFNDEF FPC}
    function Get(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$ENDIF}
    function Put(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Put(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Put(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$IFNDEF FPC}
    function Put(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$ENDIF}
    function Head(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Head(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Head(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$IFNDEF FPC}
    function Head(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$ENDIF}
    function Post(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Post(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Post(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$IFNDEF FPC}
    function Post(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$ENDIF}
    {$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
    function Patch(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Delete(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Patch(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Patch(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$IFNDEF FPC}
    function Patch(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$ENDIF}
    function Delete(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Delete(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$IFNDEF FPC}
    function Delete(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    {$ENDIF}
    {$ENDIF}

    function Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;

    function Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;

    function Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;

    function Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;

    function Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;

    function Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;
    function Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>; overload;

    function &End: IHorseCoreGroup<T>;
  end;
  {$ENDIF}

implementation

uses
  Horse.Core,
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

{$IF DEFINED(FPC)}
function THorseCoreGroup<T>.Route(const APath: string): IHorseCoreRoute<T>;
begin
  Result := THorseCoreBase(FHorseCore).BaseRoute(NormalizePath(APath)) as IHorseCoreRoute<T>;
end;
{$ELSE}
function THorseCoreGroup<T>.Route(const APath: string): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := THorseCoreGroupRoute<T>.Create(Self, FHorseCore, NormalizePath(APath));
end;
{$ENDIF}

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

{$IFNDEF FPC}
{ THorseCoreGroupRoute<T> }

constructor THorseCoreGroupRoute<T>.Create(const AGroup: IHorseCoreGroup<T>; const AHorseCore: T; const APath: string);
begin
  FGroup := AGroup;
  FHorseCore := AHorseCore;
  FPath := APath;
end;

function THorseCoreGroupRoute<T>.AddCallback(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseAddCallback(ACallback);
end;

{$IF DEFINED(FPC)}
function THorseCoreGroupRoute<T>.AddCallbacks(const ACallbacks: TList<THorseCallback>): IHorseCoreRoute<IHorseCoreGroup<T>>;
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
function THorseCoreGroupRoute<T>.AddCallbacks(const ACallbacks: TArray<THorseCallback>): IHorseCoreRoute<IHorseCoreGroup<T>>;
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    AddCallback(LCallback);
  Result := Self;
end;
{$ENDIF}

function THorseCoreGroupRoute<T>.All(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.All(const AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(FPath, [AMiddleware, ACallback]);
end;

function THorseCoreGroupRoute<T>.All(const ACallbacks: array of THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(FPath, ACallbacks);
end;

function THorseCoreGroupRoute<T>.All(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseUse(FPath, ACallbacks);
  THorseCoreBase(FHorseCore).BaseUse(FPath, [ACallback]);
end;

function THorseCoreGroupRoute<T>.Get(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseGet(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Get(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseGet(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Get(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseGet(FPath, ACallback);
end;

{$IFNDEF FPC}
function THorseCoreGroupRoute<T>.Get(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseGet(FPath, ACallback);
end;
{$ENDIF}

function THorseCoreGroupRoute<T>.Put(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePut(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Put(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePut(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Put(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePut(FPath, ACallback);
end;

{$IFNDEF FPC}
function THorseCoreGroupRoute<T>.Put(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePut(FPath, ACallback);
end;
{$ENDIF}

function THorseCoreGroupRoute<T>.Head(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseHead(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Head(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseHead(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Head(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseHead(FPath, ACallback);
end;

{$IFNDEF FPC}
function THorseCoreGroupRoute<T>.Head(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseHead(FPath, ACallback);
end;
{$ENDIF}

// Implementacao de Post
function THorseCoreGroupRoute<T>.Post(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePost(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Post(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePost(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Post(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePost(FPath, ACallback);
end;

{$IFNDEF FPC}
function THorseCoreGroupRoute<T>.Post(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePost(FPath, ACallback);
end;
{$ENDIF}

{$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
function THorseCoreGroupRoute<T>.Patch(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePatch(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Delete(const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseDelete(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Patch(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePatch(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Patch(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePatch(FPath, ACallback);
end;

{$IFNDEF FPC}
function THorseCoreGroupRoute<T>.Patch(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BasePatch(FPath, ACallback);
end;
{$ENDIF}

function THorseCoreGroupRoute<T>.Delete(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseDelete(FPath, ACallback);
end;

function THorseCoreGroupRoute<T>.Delete(const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseDelete(FPath, ACallback);
end;

{$IFNDEF FPC}
function THorseCoreGroupRoute<T>.Delete(const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  Result := Self;
  THorseCoreBase(FHorseCore).BaseDelete(FPath, ACallback);
end;
{$ENDIF}
{$ENDIF}

function THorseCoreGroupRoute<T>.Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Get(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Get(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Get(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Get(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Get(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Put(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Put(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Put(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Put(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Put(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Head(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Head(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Head(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Head(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Head(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Post(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Post(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Post(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

// Corrigido typo do post response
function THorseCoreGroupRoute<T>.Post(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Post(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Patch(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

// Corrigido typo do patch request response
function THorseCoreGroupRoute<T>.Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Patch(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Patch(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Patch(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Patch(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Delete(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Delete(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackRequest): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Delete(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.Delete(const AMiddlewares: array of THorseCallback; const ACallback: THorseCallbackResponse): IHorseCoreRoute<IHorseCoreGroup<T>>;
begin
  THorseCore.Delete(FPath, AMiddlewares, ACallback);
  Result := Self;
end;

function THorseCoreGroupRoute<T>.&End: IHorseCoreGroup<T>;
begin
  Result := FGroup;
end;
{$ENDIF}

end.
