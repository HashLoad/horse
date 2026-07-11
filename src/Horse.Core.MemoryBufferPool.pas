unit Horse.Core.MemoryBufferPool;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
  SysUtils,
  Classes,
  SyncObjs,
  Generics.Collections;
  {$ELSE}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections;
  {$ENDIF}

type
  THorsePooledStream = class;

  THorseMemoryBufferPool = class
  private
    FBufferSize: Integer;
    FMaxPoolSize: Integer;
    FMaxBufferSizeToPool: Integer;
    FPool: TStack<TBytes>;
    FLock: TCriticalSection;
    class var FDefaultPool: THorseMemoryBufferPool;
    class function GetDefaultPool: THorseMemoryBufferPool; static;
  public
    constructor Create(const ABufferSize: Integer = 65536; const AMaxPoolSize: Integer = 1024; const AMaxBufferSizeToPool: Integer = 2097152);
    destructor Destroy; override;
    
    function AcquireBuffer: TBytes;
    procedure ReleaseBuffer(var ABuffer: TBytes);
    
    function AcquireStream: THorsePooledStream;
    
    class property DefaultPool: THorseMemoryBufferPool read GetDefaultPool;
    class procedure UnInitialize; static;
  end;

  THorsePooledStream = class(TStream)
  private
    FBuffer: TBytes;
    FSize: Int64;
    FPosition: Int64;
    FCapacity: Int64;
    FPool: THorseMemoryBufferPool;
  protected
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(APool: THorseMemoryBufferPool; const ABuffer: TBytes);
    destructor Destroy; override;
    
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    
    function ToBytes: TBytes;
    function AsString: string;
    property InternalBuffer: TBytes read FBuffer;
  end;

implementation

{ THorseMemoryBufferPool }

constructor THorseMemoryBufferPool.Create(const ABufferSize, AMaxPoolSize, AMaxBufferSizeToPool: Integer);
begin
  inherited Create;
  FBufferSize := ABufferSize;
  FMaxPoolSize := AMaxPoolSize;
  FMaxBufferSizeToPool := AMaxBufferSizeToPool;
  FPool := TStack<TBytes>.Create;
  FLock := TCriticalSection.Create;
end;

destructor THorseMemoryBufferPool.Destroy;
begin
  FLock.Enter;
  try
    FPool.Free;
  finally
    FLock.Leave;
  end;
  FLock.Free;
  inherited Destroy;
end;

class function THorseMemoryBufferPool.GetDefaultPool: THorseMemoryBufferPool;
begin
  if not Assigned(FDefaultPool) then
  begin
    // Instanciação Lazy thread-safe do pool global default
    // Usamos um lock simples ou criamos direto.
    // Como a inicialização ocorre normalmente na thread principal na carga do Horse,
    // um create direto é seguro.
    FDefaultPool := THorseMemoryBufferPool.Create;
  end;
  Result := FDefaultPool;
end;

class procedure THorseMemoryBufferPool.UnInitialize;
begin
  if Assigned(FDefaultPool) then
    FreeAndNil(FDefaultPool);
end;

function THorseMemoryBufferPool.AcquireBuffer: TBytes;
begin
  FLock.Enter;
  try
    if FPool.Count > 0 then
      Result := FPool.Pop
    else
    begin
      SetLength(Result, FBufferSize);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure THorseMemoryBufferPool.ReleaseBuffer(var ABuffer: TBytes);
begin
  if Length(ABuffer) = 0 then
    Exit;

  // Se o buffer cresceu excessivamente (acima do teto máximo de RAM reutilizável),
  // nós o descartamos e não o devolvemos ao pool, evitando inflar o consumo fixo de RAM.
  if Length(ABuffer) > FMaxBufferSizeToPool then
  begin
    ABuffer := nil;
    Exit;
  end;

  FLock.Enter;
  try
    if FPool.Count < FMaxPoolSize then
    begin
      FPool.Push(ABuffer);
      ABuffer := nil;
    end;
  finally
    FLock.Leave;
  end;
end;

function THorseMemoryBufferPool.AcquireStream: THorsePooledStream;
begin
  Result := THorsePooledStream.Create(Self, AcquireBuffer);
end;

{ THorsePooledStream }

constructor THorsePooledStream.Create(APool: THorseMemoryBufferPool; const ABuffer: TBytes);
begin
  inherited Create;
  FPool := APool;
  FBuffer := ABuffer;
  FCapacity := Length(FBuffer);
  FSize := 0;
  FPosition := 0;
end;

destructor THorsePooledStream.Destroy;
begin
  if Assigned(FPool) and (Length(FBuffer) > 0) then
  begin
    FPool.ReleaseBuffer(FBuffer);
  end;
  FBuffer := nil;
  inherited Destroy;
end;

function THorsePooledStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (Count <= 0) or (FPosition >= FSize) then
    Exit(0);
    
  Result := Count;
  if FPosition + Result > FSize then
    Result := FSize - FPosition;
    
  Move(FBuffer[FPosition], Buffer, Result);
  FPosition := FPosition + Result;
end;

function THorsePooledStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count <= 0 then
    Exit(0);
    
  if FPosition + Count > FSize then
    SetSize(FPosition + Count);
    
  Move(Buffer, FBuffer[FPosition], Count);
  FPosition := FPosition + Count;
  Result := Count;
end;

function THorsePooledStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := Seek(Int64(Offset), TSeekOrigin(Origin));
end;

function THorsePooledStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: FPosition := FPosition + Offset;
    soEnd: FPosition := FSize + Offset;
  end;
  if FPosition < 0 then FPosition := 0;
  if FPosition > FSize then FPosition := FSize;
  Result := FPosition;
end;

procedure THorsePooledStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure THorsePooledStream.SetSize(const NewSize: Int64);
begin
  if NewSize > FCapacity then
  begin
    FCapacity := NewSize * 2; // Crescimento geométrico
    SetLength(FBuffer, FCapacity);
  end;
  FSize := NewSize;
  if FPosition > FSize then
    FPosition := FSize;
end;

function THorsePooledStream.ToBytes: TBytes;
begin
  SetLength(Result, FSize);
  if FSize > 0 then
    Move(FBuffer[0], Result[0], FSize);
end;

function THorsePooledStream.AsString: string;
begin
  if FSize > 0 then
    Result := TEncoding.UTF8.GetString(FBuffer, 0, FSize)
  else
    Result := '';
end;

initialization

finalization
  THorseMemoryBufferPool.UnInitialize;

end.
