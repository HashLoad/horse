unit Tests.Horse.Provider.IOCP;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestHorseProviderIOCP = class
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure PendingAcceptIsNotSelectedByKeepAliveTimeout;

    [Test]
    procedure TimeoutSelectionKeepsContextAliveOutsideRegistryLock;

    [Test]
    procedure PendingIOKeepsContextAliveUntilCompletion;

    [Test]
    procedure ConnectionCanBeClosedByAThreadThatDidNotRegisterIt;
  end;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  Horse.Provider.IOCP,
  Winapi.Winsock2;

type
  TTrackedIocpConnectionContext = class(TIocpConnectionContext)
  public
    class var DestroyCount: Integer;
    destructor Destroy; override;
  end;

destructor TTrackedIocpConnectionContext.Destroy;
begin
  Inc(DestroyCount);
  inherited;
end;

procedure TTestHorseProviderIOCP.ConnectionCanBeClosedByAThreadThatDidNotRegisterIt;
var
  LContext: TTrackedIocpConnectionContext;
  LRegistry: TIocpConnectionRegistry;
  LThread: TThread;
begin
  LRegistry := TIocpConnectionRegistry.Create;
  try
    LContext := TTrackedIocpConnectionContext.Create(INVALID_SOCKET);
    LRegistry.Add(LContext);
    { Simulates the outstanding completion that dispatches the close on any
      worker attached to the shared completion port. }
    LRegistry.BeginIO(LContext);

    LThread := TThread.CreateAnonymousThread(
      procedure
      begin
        LRegistry.Close(LContext);
      end);
    try
      LThread.FreeOnTerminate := False;
      LThread.Start;
      LThread.WaitFor;
    finally
      LThread.Free;
    end;

    Assert.AreEqual(0, TTrackedIocpConnectionContext.DestroyCount,
      'Closing from another worker must remove the shared registry entry safely');
    LRegistry.EndIO(LContext);
    Assert.AreEqual(1, TTrackedIocpConnectionContext.DestroyCount);
  finally
    LRegistry.Free;
  end;
end;

procedure TTestHorseProviderIOCP.Setup;
begin
  TTrackedIocpConnectionContext.DestroyCount := 0;
end;

procedure TTestHorseProviderIOCP.PendingAcceptIsNotSelectedByKeepAliveTimeout;
var
  LContext: TTrackedIocpConnectionContext;
  LExpired: TList<TIocpConnectionContext>;
  LRegistry: TIocpConnectionRegistry;
begin
  LRegistry := TIocpConnectionRegistry.Create;
  LExpired := TList<TIocpConnectionContext>.Create;
  try
    LContext := TTrackedIocpConnectionContext.Create(INVALID_SOCKET);
    LContext.Accepted := False;
    LContext.LastActive := 0;
    LRegistry.Add(LContext);

    LRegistry.CollectExpired(LExpired, 60001);

    Assert.AreEqual<Integer>(0, LExpired.Count,
      'A pending AcceptEx must not be treated as an idle client connection');
  finally
    LExpired.Free;
    LRegistry.Free;
  end;

  Assert.AreEqual(1, TTrackedIocpConnectionContext.DestroyCount);
end;

procedure TTestHorseProviderIOCP.TimeoutSelectionKeepsContextAliveOutsideRegistryLock;
var
  LContext: TTrackedIocpConnectionContext;
  LExpired: TList<TIocpConnectionContext>;
  LRegistry: TIocpConnectionRegistry;
begin
  LRegistry := TIocpConnectionRegistry.Create;
  LExpired := TList<TIocpConnectionContext>.Create;
  try
    LContext := TTrackedIocpConnectionContext.Create(INVALID_SOCKET);
    LContext.Accepted := True;
    LContext.LastActive := 0;
    LRegistry.Add(LContext);

    LRegistry.CollectExpired(LExpired, 60001);
    Assert.AreEqual<Integer>(1, LExpired.Count);

    LRegistry.Close(LContext);
    Assert.AreEqual(0, TTrackedIocpConnectionContext.DestroyCount,
      'The timeout snapshot must hold a reference after leaving the lock');

    LExpired[0].Release;
    LExpired.Clear;
    Assert.AreEqual(1, TTrackedIocpConnectionContext.DestroyCount);
  finally
    LExpired.Free;
    LRegistry.Free;
  end;
end;

procedure TTestHorseProviderIOCP.PendingIOKeepsContextAliveUntilCompletion;
var
  LContext: TTrackedIocpConnectionContext;
  LRegistry: TIocpConnectionRegistry;
begin
  LRegistry := TIocpConnectionRegistry.Create;
  try
    LContext := TTrackedIocpConnectionContext.Create(INVALID_SOCKET);
    LRegistry.Add(LContext);
    LRegistry.BeginIO(LContext);

    LRegistry.Close(LContext);
    Assert.AreEqual(0, TTrackedIocpConnectionContext.DestroyCount,
      'A posted I/O operation must keep its OVERLAPPED context alive');
    Assert.IsTrue(LRegistry.HasPendingIO);

    LRegistry.EndIO(LContext);
    Assert.IsFalse(LRegistry.HasPendingIO);
    Assert.AreEqual(1, TTrackedIocpConnectionContext.DestroyCount,
      'The final completion reference must release the context');
  finally
    LRegistry.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseProviderIOCP);

end.
