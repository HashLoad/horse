unit Horse.Provider.Pool;

{$IF DEFINED(FPC)}{$MODE DELPHI}{$H+}{$ENDIF}

{
  Horse Provider  —  Shared Context Pool
  =======================================
  Defines THorseContext and THorseContextPool for use by Horse provider units
  that are built into the horse repo itself (HttpSys, IOCP, Epoll) rather than
  living in a separate provider repo.

  The CrossSocket provider keeps its own pool in
  Horse.Provider.CrossSocket.Pool because it lives in a separate repo and
  predates this shared unit.  The API here intentionally matches the
  singleton pattern used by Horse.Provider.HttpSys:

      ctx := THorseContextPool.Instance.Acquire;
      ...
      THorseContextPool.Instance.Release(ctx);

  Prerequisites (patches to horse/src/ must be applied):
    PATCH-REQ-1  THorseRequest.Create;   — parameterless constructor
    PATCH-REQ-2  THorseRequest.Clear;    — pool-safe reset (nils FBody, no Free)
    PATCH-RES-2  THorseResponse.Clear;   — pool-safe reset
}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  SyncObjs,
  Generics.Collections,
{$ELSE}
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
{$ENDIF}
  Horse.Request,
  Horse.Response;

const
  HORSE_POOL_MAX_SIZE    = 512;
  HORSE_POOL_WARMUP_SIZE = 32;

type
  THorseContext = class
  private
    FRequest:  THorseRequest;
    FResponse: THorseResponse;
    FInUse:    Boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Reset;
    property Request:  THorseRequest  read FRequest;
    property Response: THorseResponse read FResponse;
    property InUse:    Boolean        read FInUse write FInUse;
  end;

  THorseContextPool = class
  private
    class var FInstance: THorseContextPool;
    FStack:     TStack<THorseContext>;
    FLock:      TCriticalSection;
    FIdleCount: Integer;
    procedure WarmUp;
  public
    constructor Create;
    destructor  Destroy; override;

    class constructor ClassCreate;
    class destructor  ClassDestroy;

    class function Instance: THorseContextPool; inline;

    function  Acquire: THorseContext;
    procedure Release(AContext: THorseContext);
    function  IdleCount: Integer; inline;
  end;

implementation

{ THorseContext }

constructor THorseContext.Create;
begin
  inherited Create;
  FRequest  := THorseRequest.Create;
  FResponse := THorseResponse.Create(nil);
  FInUse    := False;
end;

destructor THorseContext.Destroy;
begin
  FRequest.Clear;
  FRequest.Free;
  FResponse.Free;
  inherited Destroy;
end;

procedure THorseContext.Reset;
begin
  FRequest.Clear;
  FResponse.Clear;
  FInUse := False;
end;

{ THorseContextPool }

class constructor THorseContextPool.ClassCreate;
begin
  FInstance := THorseContextPool.Create;
end;

class destructor THorseContextPool.ClassDestroy;
begin
  FreeAndNil(FInstance);
end;

class function THorseContextPool.Instance: THorseContextPool;
begin
  Result := FInstance;
end;

constructor THorseContextPool.Create;
begin
  inherited Create;
  FStack     := TStack<THorseContext>.Create;
  FLock      := TCriticalSection.Create;
  FIdleCount := 0;
  WarmUp;
end;

destructor THorseContextPool.Destroy;
var
  LCtx: THorseContext;
begin
  FLock.Acquire;
  try
    while FStack.Count > 0 do
    begin
      LCtx := FStack.Pop;
      LCtx.Free;
    end;
    FIdleCount := 0;
  finally
    FLock.Release;
  end;
  FStack.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure THorseContextPool.WarmUp;
var
  I:    Integer;
  LCtx: THorseContext;
begin
  for I := 0 to HORSE_POOL_WARMUP_SIZE - 1 do
  begin
    LCtx := THorseContext.Create;
    FLock.Acquire;
    try
      FStack.Push(LCtx);
      Inc(FIdleCount);
    finally
      FLock.Release;
    end;
  end;
end;

function THorseContextPool.Acquire: THorseContext;
begin
  FLock.Acquire;
  try
    if FStack.Count > 0 then
    begin
      Result := FStack.Pop;
      Dec(FIdleCount);
    end
    else
      Result := THorseContext.Create;
  finally
    FLock.Release;
  end;
  {$IFDEF DEBUG}
  Assert(not Result.InUse, 'THorseContextPool.Acquire: context already in use');
  {$ENDIF}
  Result.InUse := True;
end;

procedure THorseContextPool.Release(AContext: THorseContext);
begin
  if AContext = nil then Exit;
  {$IFDEF DEBUG}
  Assert(AContext.InUse, 'THorseContextPool.Release: context not acquired');
  {$ENDIF}
  try
    AContext.Reset;
  except
    AContext.Free;
    Exit;
  end;
  FLock.Acquire;
  try
    if FIdleCount < HORSE_POOL_MAX_SIZE then
    begin
      FStack.Push(AContext);
      Inc(FIdleCount);
    end
    else
      AContext.Free;
  finally
    FLock.Release;
  end;
end;

function THorseContextPool.IdleCount: Integer;
begin
{$IF DEFINED(FPC)}
  Result := InterlockedCompareExchange(FIdleCount, 0, 0);
{$ELSE}
  Result := TInterlocked.CompareExchange(FIdleCount, 0, 0);
{$ENDIF}
end;

end.
