unit Horse.Core.BufferPool;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs;

type
  THorseBufferPool = class
  private
    class var FPool: TStack<TBytes>;
    class var FLock: TCriticalSection;
    class var FBufferSize: Integer;
    class var FMaxPoolSize: Integer;
  public
    class procedure Init(ABufferSize: Integer = 65536; AMaxPoolSize: Integer = 512); static;
    class procedure Uninit; static;
    class function Acquire: TBytes; static;
    class procedure Release(var ABuffer: TBytes); static;
  end;

implementation

{ THorseBufferPool }

class procedure THorseBufferPool.Init(ABufferSize: Integer = 65536; AMaxPoolSize: Integer = 512);
begin
  FLock := TCriticalSection.Create;
  FPool := TStack<TBytes>.Create;
  FBufferSize := ABufferSize;
  FMaxPoolSize := AMaxPoolSize;
end;

class procedure THorseBufferPool.Uninit;
begin
  FLock.Enter;
  try
    if Assigned(FPool) then
    begin
      while FPool.Count > 0 do
        FPool.Pop;
      FreeAndNil(FPool);
    end;
  finally
    FLock.Leave;
    FreeAndNil(FLock);
  end;
end;

class function THorseBufferPool.Acquire: TBytes;
begin
  FLock.Enter;
  try
    if FPool.Count > 0 then
    begin
      Result := FPool.Pop;
      Exit;
    end;
  finally
    FLock.Leave;
  end;
  SetLength(Result, FBufferSize);
end;

class procedure THorseBufferPool.Release(var ABuffer: TBytes);
begin
  if Length(ABuffer) <> FBufferSize then
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
      Exit;
    end;
  finally
    FLock.Leave;
  end;
  ABuffer := nil;
end;

initialization
  THorseBufferPool.Init;

finalization
  THorseBufferPool.Uninit;

end.
