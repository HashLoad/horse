unit Horse.Core.Context;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
    Generics.Collections,
    SyncObjs,
  {$ELSE}
    System.Generics.Collections,
    System.SyncObjs,
  {$ENDIF}
  Horse.Commons;

type
  THorseContext = class
  private
    FRequest: TObject;
    FResponse: TObject;
    FArena: TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    property Request: TObject read FRequest;
    property Response: TObject read FResponse;
    property Arena: TObject read FArena;
  end;

  THorseContextPool = class
  private
    FList: TQueue<THorseContext>;
    FLock: TCriticalSection;
    FMaxCount: Integer;
    FAllocatedCount: Integer;
    class var FInstance: THorseContextPool;
  public
    constructor Create(const AMaxCount: Integer = 1024);
    destructor Destroy; override;
    function Acquire: THorseContext;
    procedure Release(const AContext: THorseContext);
    procedure WarmUp(const ACount: Integer);
    class property Instance: THorseContextPool read FInstance;
  end;

implementation

uses
  Horse.Request,
  Horse.Response;

{ THorseContext }

constructor THorseContext.Create;
begin
  inherited Create;
  FRequest := THorseRequest.Create;
  FResponse := THorseResponse.Create(nil);
  FArena := THorseArenaAllocator.Create(65536);
  THorseRequest(FRequest).Arena := THorseArenaAllocator(FArena);
end;

destructor THorseContext.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  FArena.Free;
  inherited;
end;

procedure THorseContext.Reset;
begin
  THorseRequest(FRequest).Clear;
  THorseResponse(FResponse).Clear;
  THorseArenaAllocator(FArena).Reset;
end;

{ THorseContextPool }

constructor THorseContextPool.Create(const AMaxCount: Integer);
begin
  inherited Create;
  FList := TQueue<THorseContext>.Create;
  FLock := TCriticalSection.Create;
  FMaxCount := AMaxCount;
  FAllocatedCount := 0;
end;

destructor THorseContextPool.Destroy;
var
  LContext: THorseContext;
begin
  FLock.Enter;
  try
    while FList.Count > 0 do
    begin
      LContext := FList.Dequeue;
      LContext.Free;
    end;
    FList.Free;
  finally
    FLock.Leave;
  end;
  FLock.Free;
  inherited;
end;

function THorseContextPool.Acquire: THorseContext;
begin
  Result := THorseContext.Create;
end;

procedure THorseContextPool.Release(const AContext: THorseContext);
begin
  if AContext <> nil then
    AContext.Free;
end;

procedure THorseContextPool.WarmUp(const ACount: Integer);
var
  I: Integer;
  LContext: THorseContext;
begin
  FLock.Enter;
  try
    for I := 1 to ACount do
    begin
      if FAllocatedCount < FMaxCount then
      begin
        LContext := THorseContext.Create;
        FList.Enqueue(LContext);
        Inc(FAllocatedCount);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

initialization
  THorseContextPool.FInstance := THorseContextPool.Create(1024);

finalization
  THorseContextPool.FInstance.Free;

end.
