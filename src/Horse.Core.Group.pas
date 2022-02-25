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

    {$IFNDEF FPC}
    function GetCallback(ACallbackRequest: THorseCallbackRequestResponse): THorseCallback; overload;
    function GetCallback(ACallbackRequest: THorseCallbackRequest): THorseCallback; overload;
    function GetCallback(ACallbackResponse: THorseCallbackResponse): THorseCallback; overload;
    {$ENDIF}
  public
    constructor Create;

    function Prefix(APrefix: string): IHorseCoreGroup<T>;
    function Route(APath: string): IHorseCoreRoute<T>;

    function AddCallback(ACallback: THorseCallback): IHorseCoreGroup<T>;

    function Use(ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Get(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Get(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Get(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Put(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Put(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Put(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Head(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Head(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Head(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Post(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Post(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Post(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

      {$IFNDEF FPC}
      function Patch(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
      function Patch(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
      function Patch(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;

      function Delete(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
      function Delete(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
      function Delete(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
      {$IFEND}
    {$IFEND}

    function &End: T;
  end;

implementation

uses Horse.Core, Horse;

function THorseCoreGroup<T>.&End: T;
begin
  Result := FHorseCore as T;
end;

{$IFNDEF FPC}
function THorseCoreGroup<T>.GetCallback(ACallbackRequest: THorseCallbackRequestResponse): THorseCallback;
begin
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      ACallbackRequest(Req, Res);
    end;
end;

function THorseCoreGroup<T>.GetCallback(ACallbackRequest: THorseCallbackRequest): THorseCallback;
begin
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Status(THTTPStatus.NoContent);
      ACallbackRequest(Req);
    end;
end;

function THorseCoreGroup<T>.GetCallback(ACallbackResponse: THorseCallbackResponse): THorseCallback;
begin
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      ACallbackResponse(Res);
    end;
end;

function THorseCoreGroup<T>.Get(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Get(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Get(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Get(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Get(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Get(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Post(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Post(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Post(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Post(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Post(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Post(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Put(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Put(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Put(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Put(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Put(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Put(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Head(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Head(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Head(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Head(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Head(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Head(APath, GetCallback(ACallback));
end;
{$IFEND}

function THorseCoreGroup<T>.AddCallback(ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
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

function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(NormalizePath(APath), ACallback);
end;

  {$IFNDEF FPC}
  function THorseCoreGroup<T>.Patch(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
  begin
    Result := Patch(APath, GetCallback(ACallback));
  end;

  function THorseCoreGroup<T>.Patch(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
  begin
    Result := Patch(APath, GetCallback(ACallback));
  end;

  function THorseCoreGroup<T>.Patch(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
  begin
    Result := Patch(APath, GetCallback(ACallback));
  end;

  function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
  begin
    Result := Delete(APath, GetCallback(ACallback));
  end;

  function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
  begin
    Result := Delete(APath, GetCallback(ACallback));
  end;

  function THorseCoreGroup<T>.Delete(APath: string; ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
  begin
    Result := Delete(APath, GetCallback(ACallback));
  end;
  {$IFEND}
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

end.
