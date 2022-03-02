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
  Horse.Core.Group.Contract, Horse.Core.Route.Contract, Horse.Core.RouterTree, Horse.Callback;

type
  THorseCoreGroup<T: class> = class(TInterfacedObject, IHorseCoreGroup<T>)
  private
    FHorseCore: TObject;
    FPrefix: string;
    function NormalizePath(const APath: string): string;

    {$IFNDEF FPC}
    function GetCallback(const ACallbackRequest: THorseCallbackRequestResponse): THorseCallback; overload;
    function GetCallback(const ACallbackRequest: THorseCallbackRequest): THorseCallback; overload;
    function GetCallback(const ACallbackResponse: THorseCallbackResponse): THorseCallback; overload;
    {$ENDIF}
  public
    constructor Create;

    function Prefix(const APrefix: string): IHorseCoreGroup<T>;
    function Route(const APath: string): IHorseCoreRoute<T>;

    function AddCallback(const ACallback: THorseCallback): IHorseCoreGroup<T>;

    function Use(const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(const AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(const ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Get(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Get(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Get(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Get(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function Put(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Put(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Put(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Put(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function Head(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Head(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Head(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Head(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function Post(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFNDEF FPC}
    function Post(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Post(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
    function Post(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
    {$IFEND}

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

      {$IFNDEF FPC}
      function Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
      function Patch(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
      function Patch(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;

      function Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
      function Delete(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
      function Delete(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
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
function THorseCoreGroup<T>.GetCallback(const ACallbackRequest: THorseCallbackRequestResponse): THorseCallback;
begin
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      ACallbackRequest(Req, Res);
    end;
end;

function THorseCoreGroup<T>.GetCallback(const ACallbackRequest: THorseCallbackRequest): THorseCallback;
begin
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Status(THTTPStatus.NoContent);
      ACallbackRequest(Req);
    end;
end;

function THorseCoreGroup<T>.GetCallback(const ACallbackResponse: THorseCallbackResponse): THorseCallback;
begin
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      ACallbackResponse(Res);
    end;
end;

function THorseCoreGroup<T>.Get(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Get(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Get(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Get(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Get(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Get(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Post(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Post(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Post(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Post(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Post(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Post(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Put(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Put(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Put(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Put(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Put(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Put(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Head(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
begin
  Result := Head(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Head(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
begin
  Result := Head(APath, GetCallback(ACallback));
end;

function THorseCoreGroup<T>.Head(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
begin
  Result := Head(APath, GetCallback(ACallback));
end;
{$IFEND}

function THorseCoreGroup<T>.AddCallback(const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).AddCallback(ACallback);
end;

constructor THorseCoreGroup<T>.Create;
begin
  FHorseCore := THorseCore.GetInstance;
end;

function THorseCoreGroup<T>.Prefix(const APrefix: string): IHorseCoreGroup<T>;
begin
  Result := Self;
  FPrefix := '/' + APrefix.Trim(['/']);
end;

function THorseCoreGroup<T>.Route(const APath: string): IHorseCoreRoute<T>;
begin
  Result := THorseCore(FHorseCore).Route(NormalizePath(APath)) as IHorseCoreRoute<T>;
end;

function THorseCoreGroup<T>.Use(const AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), [AMiddleware, ACallback]);
end;

function THorseCoreGroup<T>.Use(const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), ACallback);
end;

function THorseCoreGroup<T>.Use(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), ACallbacks);
  THorseCore(FHorseCore).Use(NormalizePath('/'), [ACallback]);
end;

function THorseCoreGroup<T>.Use(const ACallbacks: array of THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Use(NormalizePath('/'), ACallbacks);
end;

function THorseCoreGroup<T>.Get(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Get(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Put(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Put(NormalizePath(APath), ACallback);
end;

{$IF (defined(fpc) or (CompilerVersion > 27.0))}
function THorseCoreGroup<T>.Patch(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Patch(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Delete(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Delete(NormalizePath(APath), ACallback);
end;

  {$IFNDEF FPC}
  function THorseCoreGroup<T>.Patch(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
  begin
    Result := Patch(APath, GetCallback(ACallback));
  end;

  function THorseCoreGroup<T>.Patch(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
  begin
    Result := Patch(APath, GetCallback(ACallback));
  end;

  function THorseCoreGroup<T>.Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
  begin
    Result := Patch(APath, GetCallback(ACallback));
  end;

  function THorseCoreGroup<T>.Delete(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>;
  begin
    Result := Delete(APath, GetCallback(ACallback));
  end;

  function THorseCoreGroup<T>.Delete(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>;
  begin
    Result := Delete(APath, GetCallback(ACallback));
  end;

  function THorseCoreGroup<T>.Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>;
  begin
    Result := Delete(APath, GetCallback(ACallback));
  end;
  {$IFEND}
{$IFEND}

function THorseCoreGroup<T>.NormalizePath(const APath: string): string;
begin
  Result := FPrefix + '/' + APath.Trim(['/']);
end;

function THorseCoreGroup<T>.Head(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Head(NormalizePath(APath), ACallback);
end;

function THorseCoreGroup<T>.Post(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>;
begin
  Result := Self;
  THorseCore(FHorseCore).Post(NormalizePath(APath), ACallback);
end;

end.
