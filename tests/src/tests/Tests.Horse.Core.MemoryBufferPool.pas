unit Tests.Horse.Core.MemoryBufferPool;

interface

uses
  DUnitX.TestFramework,
  Horse.Core.MemoryBufferPool,
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.SyncObjs,
  System.Generics.Collections;

type
  [TestFixture]
  TTestHorseCoreMemoryBufferPool = class
  public
    [Test]
    procedure TestBasicAcquireRelease;
    [Test]
    procedure TestCapacityGrowth;
    [Test]
    procedure TestBufferCappingSafety;
    [Test]
    procedure TestConcurrencyMultithread;
  end;

implementation

{ TTestHorseCoreMemoryBufferPool }

procedure TTestHorseCoreMemoryBufferPool.TestBasicAcquireRelease;
var
  LPool: THorseMemoryBufferPool;
  LStream1, LStream2: THorsePooledStream;
  LBufferAddr1, LBufferAddr2: Pointer;
begin
  LPool := THorseMemoryBufferPool.Create(1024, 10);
  try
    LStream1 := LPool.AcquireStream;
    try
      LStream1.WriteBuffer(PChar('Hello')^, 5);
      LBufferAddr1 := @LStream1.InternalBuffer[0];
    finally
      LStream1.Free;
    end;

    LStream2 := LPool.AcquireStream;
    try
      LBufferAddr2 := @LStream2.InternalBuffer[0];
      Assert.AreEqual(LBufferAddr1, LBufferAddr2, 'O pool deve reutilizar o mesmo buffer físico de memória.');
    finally
      LStream2.Free;
    end;
  finally
    LPool.Free;
  end;
end;

procedure TTestHorseCoreMemoryBufferPool.TestCapacityGrowth;
var
  LPool: THorseMemoryBufferPool;
  LStream: THorsePooledStream;
  LData: TBytes;
  LResult: TBytes;
  I: Integer;
begin
  LPool := THorseMemoryBufferPool.Create(1024, 10);
  try
    LStream := LPool.AcquireStream;
    try
      SetLength(LData, 5000);
      for I := 0 to 4999 do
        LData[I] := Byte(I mod 256);

      LStream.WriteBuffer(LData[0], Length(LData));
      Assert.AreEqual(Int64(5000), LStream.Size, 'O tamanho do stream deve ser 5000.');

      LStream.Position := 0;
      LResult := LStream.ToBytes;
      Assert.AreEqual(Length(LData), Length(LResult), 'O tamanho dos bytes retornados deve ser igual.');
      for I := 0 to 4999 do
        Assert.AreEqual(LData[I], LResult[I], 'O conteúdo do buffer deve coincidir.');
    finally
      LStream.Free;
    end;
  finally
    LPool.Free;
  end;
end;

procedure TTestHorseCoreMemoryBufferPool.TestBufferCappingSafety;
var
  LPool: THorseMemoryBufferPool;
  LStream: THorsePooledStream;
  LData: TBytes;
begin
  LPool := THorseMemoryBufferPool.Create(512, 10, 2048);
  try
    LStream := LPool.AcquireStream;
    try
      SetLength(LData, 5000);
      LStream.WriteBuffer(LData[0], Length(LData));
    finally
      LStream.Free;
    end;

    LStream := LPool.AcquireStream;
    try
      Assert.IsTrue(Length(LStream.InternalBuffer) = 512, 'O buffer reciclado não deve ser o superdimensionado.');
    finally
      LStream.Free;
    end;
  finally
    LPool.Free;
  end;
end;

procedure TTestHorseCoreMemoryBufferPool.TestConcurrencyMultithread;
var
  LPool: THorseMemoryBufferPool;
  LThreads: TArray<TThread>;
  I: Integer;
  LErrorCount: Integer;
const
  THREAD_COUNT = 32;
  LOOP_COUNT = 100;
begin
  LErrorCount := 0;
  LPool := THorseMemoryBufferPool.Create(1024, 20);
  try
    SetLength(LThreads, THREAD_COUNT);
    for I := 0 to THREAD_COUNT - 1 do
    begin
      LThreads[I] := TThread.CreateAnonymousThread(
        procedure
        var
          J: Integer;
          LStream: THorsePooledStream;
          LWriteVal, LReadVal: Integer;
        begin
          for J := 1 to LOOP_COUNT do
          begin
            LStream := LPool.AcquireStream;
            try
              LWriteVal := J * 7;
              LStream.WriteBuffer(LWriteVal, SizeOf(LWriteVal));
              LStream.Position := 0;
              LStream.ReadBuffer(LReadVal, SizeOf(LReadVal));
              if LWriteVal <> LReadVal then
                TInterlocked.Increment(LErrorCount);
            finally
              LStream.Free;
            end;
          end;
        end
      );
      LThreads[I].FreeOnTerminate := False;
      LThreads[I].Start;
    end;

    for I := 0 to THREAD_COUNT - 1 do
      LThreads[I].WaitFor;

    for I := 0 to THREAD_COUNT - 1 do
      LThreads[I].Free;

    Assert.AreEqual(0, LErrorCount, 'Não deve haver corrupção ou incompatibilidade de dados entre as threads concorrentes.');
  finally
    LPool.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseCoreMemoryBufferPool);

end.
