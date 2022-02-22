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

    function Use(ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Get(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Get(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Get(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Post(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Post(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Post(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Put(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Put(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Put(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    {$IF (DEFINED(FPC) or (CompilerVersion > 27.0))}
    function Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
      {$IFNDEF FPC}
      function Delete(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
      function Delete(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
      function Delete(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
      {$IFEND}

    function Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
      {$IFNDEF FPC}
      function Patch(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
      function Patch(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
      function Patch(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
      {$IFEND}
    {$IFEND}

    function Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Head(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Head(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Head(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function AddCallback(ACallback: THorseCallback): IHorseCoreGroup<T>;
    function AddCallbacks(ACallback: TArray<THorseCallback>): IHorseCoreGroup<T>;

    function &End: T;
  end;

implementation

uses Horse.Core;

{ THorseCoreGroup }

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

function THorseCoreGroup<T>.Use(ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), ACallback);
end;

function THorseCoreGroup<T>.Use(AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), [AMiddleware, ACallback]);
end;

function THorseCoreGroup<T>.Use(ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), ACallbacks);
end;

function THorseCoreGroup<T>.Use(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), ACallbacks);
  THorseCore(FHorseCore).Use(NormalizePath('/'), [ACallback]);
end;

function THorseCoreGroup<T>.Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(NormalizePath(APath), ACallbacks, ACallback);
end;

{$IFNDEF FPC}

function THorseCoreGroup<T>.Get(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Get(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Get(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(NormalizePath(APath), ACallback);
end;

{$IFEND}

function THorseCoreGroup<T>.Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(NormalizePath(APath), ACallbacks, ACallback);
end;

{$IFNDEF FPC}

function THorseCoreGroup<T>.Post(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Post(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Post(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(NormalizePath(APath), ACallback);
end;

{$IFEND}

function THorseCoreGroup<T>.Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(NormalizePath(APath), ACallbacks, ACallback);
end;

{$IFNDEF FPC}

function THorseCoreGroup<T>.Put(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Put(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Put(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(NormalizePath(APath), ACallback);
end;

{$IFEND}

{$IF (DEFINED(FPC) or (CompilerVersion > 27.0))}
function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(NormalizePath(APath), ACallbacks, ACallback);
end;

{$IFNDEF FPC}

function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(NormalizePath(APath), ACallback);
end;

{$IFEND}
{$IFEND}

{$IF (DEFINED(FPC) or (CompilerVersion > 27.0))}
function THorseCoreGroup<T>.Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(NormalizePath(APath), ACallbacks, ACallback);
end;

{$IFNDEF FPC}

function THorseCoreGroup<T>.Patch(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Patch(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Patch(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(NormalizePath(APath), ACallback);
end;

{$IFEND}
{$IFEND}

function THorseCoreGroup<T>.Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(NormalizePath(APath), ACallbacks, ACallback);
end;

{$IFNDEF FPC}

function THorseCoreGroup<T>.Head(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Head(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Head(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(NormalizePath(APath), ACallback);
end;

{$IFEND}

function THorseCoreGroup<T>.AddCallback(ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).AddCallback(ACallback);
end;

function THorseCoreGroup<T>.AddCallbacks(ACallback: TArray<THorseCallback>): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).AddCallbacks(ACallback);
end;

function THorseCoreGroup<T>.&End: T;
begin
  Result := FHorseCore as T;
end;

function THorseCoreGroup<T>.NormalizePath(APath: string): string;
begin
  Result := FPrefix + '/' + APath.Trim(['/']);
end;

end.
