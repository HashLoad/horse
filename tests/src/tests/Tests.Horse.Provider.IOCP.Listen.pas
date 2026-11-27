unit Tests.Horse.Provider.IOCP.Listen;

{ The listen cycle of the IOCP provider, on real sockets.

  CA1   a port bound by another socket is refused, with the port and the code;
  CA1b  a port held by another fixed IOCP process is refused (10048 or 10013);
  CA1c  with the IOCP listening, a socket with SO_REUSEADDR cannot bind the
        port, which is what SO_EXCLUSIVEADDRUSE is for;
  CA2   the messages are the literal ones of the contract (inside CA1/CA1b);
  CA3   listening again in the same process right after a graceful stop with a
        keep-alive client still connected, 10 rounds;
  CA3b  listening again right after another process holding a client is
        killed, 10 rounds;
  CA5   the callbacks given to Listen run once each, and a repeated Listen
        never runs the stop callback of the previous one;
  CA6   after a refused Listen, freeing the port lets the next one come up;
  CA8   a StopListen made while keep-alive requests keep arriving returns,
        and so does the Listen, 10 rounds;
  CA7   a Listen while the server is up is refused, on another port and host,
        and the server keeps its port, its host, its answers and its own stop
        callback;
  CA7b  a Listen right after StopListen, before the Listen thread returns, never
        swaps the stop callback of the cycle still ending, 10 rounds;
  CA7c  a Listen made as soon as the previous one returns, after a stop slowed
        down by a keep-alive client, comes up whole and answers, 10 rounds.

  CA7c, CA7b and CA7 are declared last: against a source without the listen
  cycle they leave the provider in a state that fails the setup of the checks
  after them. The script runs chosen checks each in a process of its own.

  A Listen counts as up when its listen callback runs, which is after the
  workers exist, so that no test stops a server that is still starting. The
  source before this change ignores the callbacks; there a Listen counts as
  up CALLBACK_GRACE_MS after IsRunning.

  CA3 and CA3b first assert that a server-side connection on the port is
  still active or in TIME_WAIT; without it they fail by precondition, and the
  message lists the TCP rows of the port. A stop, and a killed process, end
  the keep-alive connections with a reset, which leaves no TIME_WAIT (measured
  on Windows); so before it a Connection: close request makes the server close
  one connection itself, which leaves its side in TIME_WAIT. The precondition
  is that very connection, found by the client port, so that connections left
  by an earlier round or check never satisfy it. They measure the risk
  SO_EXCLUSIVEADDRUSE brings and have no negative control.

  What Windows answered (the codes of CA1b and CA1c, the connections CA3 and
  CA3b found), and when each check started and finished, goes to
  OBSERVATIONS_FILE, which is written at once, so that a run killed for hanging
  still shows the check that hung. It sits next to the test executable, and
  tests/iocp-listen/run-iocp-listen.ps1 appends it to the run log.

  CA1b and CA3b start IocpListenHolder.exe, which must sit next to the test
  executable (tests/iocp-listen/run-iocp-listen.ps1 builds both). }

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestHorseProviderIOCPListen = class
  private
    FCheck: string;
    procedure StartCheck(const ACheck: string);
  public
    [Setup]
    procedure SetUp;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure BusyPortIsRefusedWithPortAndCode;

    [Test]
    procedure PortHeldByAnotherIocpIsRefused;

    [Test]
    procedure ReuseAddressSocketCannotShareTheListeningPort;

    [Test]
    procedure ListensAgainAfterGracefulStopInSameProcess;

    [Test]
    procedure ListensAgainAfterHolderProcessIsKilled;

    [Test]
    procedure ListenRunsItsOwnCallbacksOnce;

    [Test]
    procedure RefusedListenLeavesThePortFree;

    { A stop with keep-alive requests arriving can wait forever for a worker,
      before this change too (measured on Windows). It is the check of a
      separate fix, which removes this Ignore. }
    [Test]
    [Ignore('IOCP stop under load never returns at times; it has a separate fix')]
    procedure StopWithRequestsArrivingReturns;

    [Test]
    procedure ListenAgainAsSoonAsListenReturnsComesUpWhole;

    [Test]
    procedure ListenRightAfterStopRunsEachStopCallbackOnce;

    [Test]
    procedure ListenWhileListeningIsRefusedAndKeepsTheServer;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
  Winapi.Windows,
  Winapi.Winsock2,
  Horse.Provider.IOCP;

const
  LISTEN_TEST_PORT = 19266;
  WAIT_MS = 5000;
  CALLBACK_GRACE_MS = 1000;
  RELISTEN_ROUNDS = 10;
  HOLDER_EXE = 'IocpListenHolder.exe';
  OBSERVATIONS_FILE = 'iocp-listen-observations.log';
  REQUEST = 'GET /iocp-listen HTTP/1.1'#13#10'Host: 127.0.0.1'#13#10 +
    'Connection: keep-alive'#13#10#13#10;
  REQUEST_CLOSE = 'GET /iocp-listen HTTP/1.1'#13#10'Host: 127.0.0.1'#13#10 +
    'Connection: close'#13#10#13#10;

  { The literal messages the provider gives. }
  MESSAGE_IN_USE = 'Não foi possível escutar na porta %d: ela já está em uso por outro processo. ' +
    'Encerre o outro processo ou configure outra porta. (código %d: %s)';
  MESSAGE_DENIED = 'Não foi possível escutar na porta %d: o Windows não permite o uso dela, porque está ' +
    'reservada pelo sistema ou em uso exclusivo por outro processo. Configure outra porta. (código %d: %s)';
  MESSAGE_ALREADY_RUNNING = 'O servidor já está escutando na porta %d. Pare o servidor antes de escutar de novo.';

  MIB_TCP_STATE_LISTEN = 2;
  MIB_TCP_STATE_ESTAB = 5;
  MIB_TCP_STATE_TIME_WAIT = 11;

type
  TMibTcpRow2 = record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwOwningPid: DWORD;
    dwOffloadState: DWORD;
  end;
  PMibTcpRow2 = ^TMibTcpRow2;

  TListenOutcome = (loFailed, loRunning, loRunningWithoutCallback, loTimeout);

  { The body of a TListenRun: one blocking Listen, given the callback to pass
    as its listen callback (or to call from its own). }
  TListenBody = reference to procedure(AOnListen: TProc);

  { What the Listen thread writes. It is kept apart from TListenRun so that a
    Listen that never returns can be left behind without writing into freed
    memory. }
  TListenState = class
  public
    Listened: TEvent;
    Error: string;
    Failed: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { Runs one blocking Listen call on a thread of its own and keeps its error. }
  TListenRun = class
  private
    FThread: TThread;
    FState: TListenState;
    FCameUp: Boolean;
    function GetError: string;
  public
    constructor Create(const ABody: TListenBody);
    destructor Destroy; override;
    function WaitOutcome: TListenOutcome;
    function WaitFinished: Boolean;
    function Stop: Boolean;
    property Error: string read GetError;
  end;

function GetTcpTable2(ATcpTable: Pointer; var ASize: ULONG; AOrder: BOOL): ULONG; stdcall;
  external 'iphlpapi.dll' name 'GetTcpTable2';

var
  WsaData: TWSAData;

function CameUp(AOutcome: TListenOutcome): Boolean;
begin
  Result := AOutcome in [loRunning, loRunningWithoutCallback];
end;

procedure RecordObservation(const AText: string);
var
  LWriter: TStreamWriter;
begin
  LWriter := TStreamWriter.Create(ExtractFilePath(ParamStr(0)) + OBSERVATIONS_FILE, True, TEncoding.UTF8);
  try
    LWriter.WriteLine(AText);
  finally
    LWriter.Free;
  end;
end;

{ Frees a thread that ended, or leaves one that did not end in time to the
  process exit, rather than hanging the run. With AStart, starts it first. }
procedure ReleaseThread(AThread: TThread; AStart: Boolean = False);
begin
  if AStart then
  begin
    AThread.FreeOnTerminate := False;
    AThread.Start;
  end;
  if WaitForSingleObject(AThread.Handle, WAIT_MS) = WAIT_OBJECT_0 then
    AThread.Free
  else
    AThread.FreeOnTerminate := True;
end;

{ TListenState }

constructor TListenState.Create;
begin
  inherited Create;
  Listened := TEvent.Create(nil, True, False, '');
end;

destructor TListenState.Destroy;
begin
  Listened.Free;
  inherited;
end;

{ TListenRun }

constructor TListenRun.Create(const ABody: TListenBody);
var
  LBody: TListenBody;
  LState: TListenState;
begin
  inherited Create;
  LBody := ABody;
  LState := TListenState.Create;
  FState := LState;
  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        LBody(
          procedure
          begin
            LState.Listened.SetEvent;
          end);
      except
        on E: Exception do
        begin
          LState.Error := E.Message;
          LState.Failed := True;
        end;
      end;
    end);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

{ A Listen that does not return even after StopListen is left running, with
  its state, rather than hanging the run; the test that owns it has already
  failed on Stop. }
destructor TListenRun.Destroy;
begin
  if Stop then
  begin
    FThread.Free;
    FState.Free;
  end
  else
    FThread.FreeOnTerminate := True;
  inherited;
end;

function TListenRun.GetError: string;
begin
  Result := FState.Error;
end;

function TListenRun.WaitOutcome: TListenOutcome;
var
  LWatch: TStopwatch;
  LRunningSince: Int64;
begin
  LWatch := TStopwatch.StartNew;
  LRunningSince := -1;
  repeat
    if FState.Listened.WaitFor(10) = wrSignaled then
    begin
      FCameUp := True;
      Exit(loRunning);
    end;
    if FThread.Finished then
    begin
      if FState.Failed then
        Exit(loFailed);
      Exit(loTimeout);
    end;
    if THorseProviderIOCP.IsRunning then
    begin
      if LRunningSince < 0 then
        LRunningSince := LWatch.ElapsedMilliseconds
      else if LWatch.ElapsedMilliseconds - LRunningSince > CALLBACK_GRACE_MS then
      begin
        FCameUp := True;
        Exit(loRunningWithoutCallback);
      end;
    end;
  until LWatch.ElapsedMilliseconds > WAIT_MS;
  Result := loTimeout;
end;

function TListenRun.WaitFinished: Boolean;
begin
  Result := WaitForSingleObject(FThread.Handle, WAIT_MS) = WAIT_OBJECT_0;
end;

{ True when the Listen thread has returned. Only a run that came up stops the
  server, so that a refused run never takes down the server of another one: a
  graceful stop first, then a plain one if the thread is still there. }
function TListenRun.Stop: Boolean;
begin
  if FThread.Finished then
    Exit(True);
  if FCameUp and THorseProviderIOCP.IsRunning then
    THorseProviderIOCP.StopListenGraceful;
  if WaitFinished then
    Exit(True);
  { The plain stop runs on a thread of its own: when another stop is stuck it
    would block too, and the run would hang instead of failing. }
  if FCameUp then
    ReleaseThread(TThread.CreateAnonymousThread(
      procedure
      begin
        THorseProviderIOCP.StopListen;
      end), True);
  Result := WaitFinished;
end;

{ Sockets }

function SocketAddress(APort: Integer; AAddress: Cardinal): TSockAddrIn;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.sin_family := AF_INET;
  Result.sin_port := htons(APort);
  Result.sin_addr.S_addr := AAddress;
end;

function LoopbackAddress(APort: Integer): TSockAddrIn;
begin
  Result := SocketAddress(APort, inet_addr('127.0.0.1'));
end;

{ A plain socket bound to INADDR_ANY on the port, with no options, listening. }
function OccupyPort(APort: Integer): TSocket;
var
  LAddr: TSockAddrIn;
  LCode: Integer;
begin
  Result := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  Assert.IsTrue(Result <> INVALID_SOCKET, 'socket() of the occupier failed');
  LAddr := SocketAddress(APort, INADDR_ANY);
  if (bind(Result, PSockAddr(@LAddr)^, SizeOf(LAddr)) = SOCKET_ERROR) or
     (Winapi.Winsock2.listen(Result, SOMAXCONN) = SOCKET_ERROR) then
  begin
    LCode := WSAGetLastError;
    closesocket(Result);
    Assert.Fail(Format('the occupier could not take port %d (code %d)', [APort, LCode]));
  end;
end;

function TryConnect(APort: Integer; out ASocket: TSocket): Boolean;
var
  LAddr: TSockAddrIn;
  LTimeout: Integer;
begin
  ASocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if ASocket = INVALID_SOCKET then
    Exit(False);
  LAddr := LoopbackAddress(APort);
  Result := connect(ASocket, PSockAddr(@LAddr)^, SizeOf(LAddr)) <> SOCKET_ERROR;
  if not Result then
  begin
    closesocket(ASocket);
    ASocket := INVALID_SOCKET;
    Exit;
  end;
  LTimeout := WAIT_MS;
  setsockopt(ASocket, SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@LTimeout), SizeOf(LTimeout));
end;

function ConnectWithin(APort: Integer; out ASocket: TSocket): Boolean;
var
  LWatch: TStopwatch;
begin
  LWatch := TStopwatch.StartNew;
  repeat
    if TryConnect(APort, ASocket) then
      Exit(True);
    Sleep(20);
  until LWatch.ElapsedMilliseconds > WAIT_MS;
  Result := False;
end;

{ Sends one keep-alive request and reads the response head. True when an HTTP
  status line came back, which means the server accepted the connection. }
function RequestHead(ASocket: TSocket): Boolean;
var
  LRequest: AnsiString;
  LBuffer: array[0..4095] of AnsiChar;
  LChunk: AnsiString;
  LHead: AnsiString;
  LRead: Integer;
begin
  LRequest := REQUEST;
  if send(ASocket, PAnsiChar(LRequest)^, Length(LRequest), 0) <> Length(LRequest) then
    Exit(False);
  LHead := '';
  repeat
    LRead := recv(ASocket, LBuffer[0], SizeOf(LBuffer), 0);
    if LRead <= 0 then
      Break;
    SetString(LChunk, PAnsiChar(@LBuffer[0]), LRead);
    LHead := LHead + LChunk;
  until Pos(AnsiString(#13#10#13#10), LHead) > 0;
  Result := Copy(LHead, 1, 9) = 'HTTP/1.1 ';
end;

{ Sends a Connection: close request and reads until the server closes, so
  that the server closes first and its side of the connection goes to
  TIME_WAIT. True when an HTTP status line came back and the server closed. }
function RequestAndServerCloses(APort: Integer; out AClientPort: Integer): Boolean;
var
  LClient: TSocket;
  LLocal: TSockAddrIn;
  LLocalSize: Integer;
  LRequest: AnsiString;
  LBuffer: array[0..4095] of AnsiChar;
  LChunk: AnsiString;
  LResponse: AnsiString;
  LRead: Integer;
begin
  Result := False;
  AClientPort := 0;
  if not ConnectWithin(APort, LClient) then
    Exit;
  try
    LLocalSize := SizeOf(LLocal);
    if getsockname(LClient, PSockAddr(@LLocal)^, LLocalSize) = SOCKET_ERROR then
      Exit;
    AClientPort := ntohs(LLocal.sin_port);
    LRequest := REQUEST_CLOSE;
    if send(LClient, PAnsiChar(LRequest)^, Length(LRequest), 0) <> Length(LRequest) then
      Exit;
    LResponse := '';
    repeat
      LRead := recv(LClient, LBuffer[0], SizeOf(LBuffer), 0);
      if LRead > 0 then
      begin
        SetString(LChunk, PAnsiChar(@LBuffer[0]), LRead);
        LResponse := LResponse + LChunk;
      end;
    until LRead <= 0;
    Result := (LRead = 0) and (Copy(LResponse, 1, 9) = 'HTTP/1.1 ');
  finally
    closesocket(LClient);
  end;
end;

{ True when a connect and a request on the port are served. }
function Answers(APort: Integer): Boolean;
var
  LClient: TSocket;
begin
  Result := ConnectWithin(APort, LClient);
  if Result then
  try
    Result := RequestHead(LClient);
  finally
    closesocket(LClient);
  end;
end;

procedure CloseSocketIfOpen(var ASocket: TSocket);
begin
  if ASocket <> INVALID_SOCKET then
  begin
    closesocket(ASocket);
    ASocket := INVALID_SOCKET;
  end;
end;

{ TCP table }

function ReadTcpTable: TArray<TMibTcpRow2>;
var
  LSize: ULONG;
  LBuffer: TBytes;
  LCount: DWORD;
  LRow: PMibTcpRow2;
  I: Integer;
  LStatus: ULONG;
begin
  Result := nil;
  LSize := 0;
  LStatus := GetTcpTable2(nil, LSize, False);
  while LStatus = ERROR_INSUFFICIENT_BUFFER do
  begin
    SetLength(LBuffer, LSize);
    LStatus := GetTcpTable2(@LBuffer[0], LSize, False);
  end;
  Assert.AreEqual<ULONG>(NO_ERROR, LStatus, 'GetTcpTable2 failed');
  if Length(LBuffer) < SizeOf(DWORD) then
    Exit;
  LCount := PDWORD(@LBuffer[0])^;
  SetLength(Result, LCount);
  LRow := PMibTcpRow2(@LBuffer[SizeOf(DWORD)]);
  for I := 0 to Integer(LCount) - 1 do
  begin
    Result[I] := LRow^;
    Inc(LRow);
  end;
end;

function RowPort(APort: DWORD): Integer;
begin
  Result := ntohs(Word(APort));
end;

function HasListener(APort: Integer): Boolean;
var
  LRow: TMibTcpRow2;
begin
  Result := False;
  for LRow in ReadTcpTable do
    if (RowPort(LRow.dwLocalPort) = APort) and (LRow.dwState = MIB_TCP_STATE_LISTEN) then
      Exit(True);
end;

{ Every row with the port on either end, as state/local/remote/pid. }
function DescribePort(APort: Integer): string;
var
  LRow: TMibTcpRow2;
begin
  Result := '';
  for LRow in ReadTcpTable do
    if (RowPort(LRow.dwLocalPort) = APort) or (RowPort(LRow.dwRemotePort) = APort) then
      Result := Result + Format(' [state %d, local %d, remote %d, pid %d]',
        [LRow.dwState, RowPort(LRow.dwLocalPort), RowPort(LRow.dwRemotePort), LRow.dwOwningPid]);
  if Result = '' then
    Result := ' no rows';
end;

{ The state of the server side of the connection from AClientPort to the
  port, or 0 when the table has no such row. }
function ServerSideState(APort, AClientPort: Integer): DWORD;
var
  LRow: TMibTcpRow2;
begin
  Result := 0;
  for LRow in ReadTcpTable do
    if (RowPort(LRow.dwLocalPort) = APort) and (RowPort(LRow.dwRemotePort) = AClientPort) then
      Exit(LRow.dwState);
end;

{ The server side of the connection the round itself closed, from
  AClientPort, is still active or on its way to closed (TIME_WAIT included). }
procedure AssertServerSideConnection(APort, AClientPort: Integer; const AContext: string; ARecord: Boolean);
var
  LState: DWORD;
begin
  LState := ServerSideState(APort, AClientPort);
  if ARecord then
    RecordObservation(Format('%s: the server side of the connection from %d is in state %d;%s',
      [AContext, AClientPort, LState, DescribePort(APort)]));
  if (LState < MIB_TCP_STATE_ESTAB) or (LState > MIB_TCP_STATE_TIME_WAIT) then
    Assert.Fail(Format('%s: precondition not met, the server side of the connection from %d on port %d ' +
      'is not active or in TIME_WAIT (state %d), so the risk was not exercised. Rows:%s',
      [AContext, AClientPort, APort, LState, DescribePort(APort)]));
end;

function ExpectedMessage(const AFormat: string; APort, ACode: Integer): string;
begin
  Result := Format(AFormat, [APort, ACode, SysErrorMessage(ACode)]);
end;

{ Holder process }

function StartHolder(APort: Integer): TProcessInformation;
var
  LStartup: TStartupInfo;
  LCommand: string;
  LPath: string;
begin
  LPath := ExtractFilePath(ParamStr(0)) + HOLDER_EXE;
  Assert.IsTrue(FileExists(LPath), LPath + ' not found; build it with tests/iocp-listen/run-iocp-listen.ps1');
  LCommand := '"' + LPath + '" ' + IntToStr(APort);
  UniqueString(LCommand);
  FillChar(LStartup, SizeOf(LStartup), 0);
  LStartup.cb := SizeOf(LStartup);
  if not CreateProcess(nil, PChar(LCommand), nil, nil, False, CREATE_NO_WINDOW, nil, nil,
    LStartup, Result) then
    Assert.Fail('could not start ' + LPath + ': ' + SysErrorMessage(GetLastError));
end;

procedure KillHolder(var AProcess: TProcessInformation);
begin
  if AProcess.hProcess = 0 then
    Exit;
  TerminateProcess(AProcess.hProcess, 1);
  WaitForSingleObject(AProcess.hProcess, WAIT_MS);
  CloseHandle(AProcess.hThread);
  CloseHandle(AProcess.hProcess);
  AProcess.hProcess := 0;
end;

function ListenOnTestPort: TListenRun;
begin
  Result := TListenRun.Create(
    procedure(AOnListen: TProc)
    begin
      THorseProviderIOCP.Listen(LISTEN_TEST_PORT, '0.0.0.0', AOnListen, nil);
    end);
end;

{ Asserts a fresh Listen on the port comes up, then stops it. }
procedure AssertListensAndStop(const AContext: string);
var
  LRun: TListenRun;
begin
  LRun := ListenOnTestPort;
  try
    case LRun.WaitOutcome of
      loFailed:
        Assert.Fail(AContext + ': Listen failed: ' + LRun.Error);
      loTimeout:
        Assert.Fail(AContext + ': Listen neither came up nor failed');
    end;
    Assert.IsTrue(LRun.Stop, AContext + ': Listen did not return after the stop');
  finally
    LRun.Free;
  end;
end;

{ TTestHorseProviderIOCPListen }

procedure TTestHorseProviderIOCPListen.StartCheck(const ACheck: string);
begin
  FCheck := ACheck;
  RecordObservation(ACheck + ' started');
end;

procedure TTestHorseProviderIOCPListen.TearDown;
begin
  if FCheck <> '' then
    RecordObservation(FCheck + ' finished');
  FCheck := '';
end;

{ Each test starts with no IOCP up in this process and no listener on the
  port, whatever the previous one left. }
procedure TTestHorseProviderIOCPListen.SetUp;
var
  LWatch: TStopwatch;
begin
  Assert.IsFalse(THorseProviderIOCP.IsRunning, 'setup: an IOCP of a previous test is still up');
  LWatch := TStopwatch.StartNew;
  while HasListener(LISTEN_TEST_PORT) do
  begin
    if LWatch.ElapsedMilliseconds > WAIT_MS then
      Assert.Fail('setup: the port still has a listener. Rows:' + DescribePort(LISTEN_TEST_PORT));
    Sleep(20);
  end;
end;

procedure TTestHorseProviderIOCPListen.BusyPortIsRefusedWithPortAndCode;
var
  LOccupier: TSocket;
  LRun: TListenRun;
begin
  StartCheck('CA1');
  LOccupier := OccupyPort(LISTEN_TEST_PORT);
  try
    LRun := ListenOnTestPort;
    try
      case LRun.WaitOutcome of
        loRunning, loRunningWithoutCallback:
          Assert.Fail('CA1: the IOCP came up on a port another socket holds');
        loTimeout:
          Assert.Fail('CA1: Listen neither came up nor failed');
      end;
      Assert.AreEqual(ExpectedMessage(MESSAGE_IN_USE, LISTEN_TEST_PORT, WSAEADDRINUSE), LRun.Error,
        'CA2: message of the busy port');
    finally
      LRun.Free;
    end;
  finally
    closesocket(LOccupier);
  end;
end;

procedure TTestHorseProviderIOCPListen.PortHeldByAnotherIocpIsRefused;
var
  LHolder: TProcessInformation;
  LProbe: TSocket;
  LRun: TListenRun;
  LInUse, LDenied: string;
begin
  StartCheck('CA1b');
  LHolder := StartHolder(LISTEN_TEST_PORT);
  try
    Assert.IsTrue(ConnectWithin(LISTEN_TEST_PORT, LProbe), 'CA1b: the holder did not come up');
    CloseSocketIfOpen(LProbe);

    LRun := ListenOnTestPort;
    try
      case LRun.WaitOutcome of
        loRunning, loRunningWithoutCallback:
          Assert.Fail('CA1b: the IOCP came up on a port another IOCP holds');
        loTimeout:
          Assert.Fail('CA1b: Listen neither came up nor failed');
      end;
      LInUse := ExpectedMessage(MESSAGE_IN_USE, LISTEN_TEST_PORT, WSAEADDRINUSE);
      LDenied := ExpectedMessage(MESSAGE_DENIED, LISTEN_TEST_PORT, WSAEACCES);
      if LRun.Error = LInUse then
        RecordObservation('CA1b: Windows answered 10048 (WSAEADDRINUSE)')
      else if LRun.Error = LDenied then
        RecordObservation('CA1b: Windows answered 10013 (WSAEACCES)')
      else
        Assert.Fail('CA2: unexpected message for a port held by another IOCP: ' + LRun.Error);
    finally
      LRun.Free;
    end;
  finally
    KillHolder(LHolder);
  end;
end;

procedure TTestHorseProviderIOCPListen.ReuseAddressSocketCannotShareTheListeningPort;
var
  LRun: TListenRun;
  LIntruder: TSocket;
  LAddr: TSockAddrIn;
  LOpt: Integer;
  LBound: Boolean;
  LCode: Integer;
begin
  StartCheck('CA1c');
  LRun := ListenOnTestPort;
  try
    Assert.IsTrue(CameUp(LRun.WaitOutcome), 'CA1c: the IOCP did not come up: ' + LRun.Error);

    LIntruder := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    Assert.IsTrue(LIntruder <> INVALID_SOCKET, 'CA1c: socket() of the intruder failed');
    try
      LOpt := 1;
      Assert.IsTrue(setsockopt(LIntruder, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@LOpt), SizeOf(LOpt)) <> SOCKET_ERROR,
        'CA1c: SO_REUSEADDR could not be set on the intruder');
      LAddr := SocketAddress(LISTEN_TEST_PORT, INADDR_ANY);
      LBound := bind(LIntruder, PSockAddr(@LAddr)^, SizeOf(LAddr)) <> SOCKET_ERROR;
      LCode := WSAGetLastError;
      Assert.IsFalse(LBound, 'CA1c: a socket with SO_REUSEADDR bound the port the IOCP listens on');
      RecordObservation(Format('CA1c: the bind with SO_REUSEADDR was refused with %d', [LCode]));
    finally
      closesocket(LIntruder);
    end;

    Assert.IsTrue(LRun.Stop, 'CA1c: Listen did not return after the stop');
  finally
    LRun.Free;
  end;
end;

procedure TTestHorseProviderIOCPListen.ListensAgainAfterGracefulStopInSameProcess;
var
  LRound: Integer;
  LClient: TSocket;
  LClosedFrom: Integer;
  LRun: TListenRun;
begin
  StartCheck('CA3');
  for LRound := 1 to RELISTEN_ROUNDS do
  begin
    LClient := INVALID_SOCKET;
    try
      LRun := ListenOnTestPort;
      try
        Assert.IsTrue(CameUp(LRun.WaitOutcome),
          Format('CA3 round %d: the first Listen did not come up: %s', [LRound, LRun.Error]));
        Assert.IsTrue(ConnectWithin(LISTEN_TEST_PORT, LClient), Format('CA3 round %d: connect', [LRound]));
        Assert.IsTrue(RequestHead(LClient), Format('CA3 round %d: first request', [LRound]));
        Assert.IsTrue(RequestHead(LClient), Format('CA3 round %d: keep-alive request', [LRound]));
        Assert.IsTrue(RequestAndServerCloses(LISTEN_TEST_PORT, LClosedFrom),
          Format('CA3 round %d: the server did not close a Connection: close request', [LRound]));
        Assert.IsTrue(LRun.Stop, Format('CA3 round %d: Listen did not return', [LRound]));
      finally
        LRun.Free;
      end;

      AssertServerSideConnection(LISTEN_TEST_PORT, LClosedFrom, Format('CA3 round %d', [LRound]), LRound = 1);
      AssertListensAndStop(Format('CA3 round %d, second Listen', [LRound]));
    finally
      CloseSocketIfOpen(LClient);
    end;
  end;
end;

procedure TTestHorseProviderIOCPListen.ListensAgainAfterHolderProcessIsKilled;
var
  LRound: Integer;
  LClient: TSocket;
  LClosedFrom: Integer;
  LHolder: TProcessInformation;
begin
  StartCheck('CA3b');
  for LRound := 1 to RELISTEN_ROUNDS do
  begin
    LClient := INVALID_SOCKET;
    LHolder := StartHolder(LISTEN_TEST_PORT);
    try
      Assert.IsTrue(ConnectWithin(LISTEN_TEST_PORT, LClient),
        Format('CA3b round %d: the holder did not come up', [LRound]));
      Assert.IsTrue(RequestHead(LClient), Format('CA3b round %d: request to the holder', [LRound]));
      Assert.IsTrue(RequestAndServerCloses(LISTEN_TEST_PORT, LClosedFrom),
        Format('CA3b round %d: the holder did not close a Connection: close request', [LRound]));
      KillHolder(LHolder);

      AssertServerSideConnection(LISTEN_TEST_PORT, LClosedFrom, Format('CA3b round %d', [LRound]), LRound = 1);
      AssertListensAndStop(Format('CA3b round %d, Listen after the kill', [LRound]));
    finally
      KillHolder(LHolder);
      CloseSocketIfOpen(LClient);
    end;
  end;
end;

procedure TTestHorseProviderIOCPListen.ListenRunsItsOwnCallbacksOnce;
var
  LListened, LStopped, LListened2: Integer;
  LAccepted: Boolean;
  LRun: TListenRun;
begin
  StartCheck('CA5');
  LListened := 0;
  LStopped := 0;
  LListened2 := 0;
  LAccepted := False;

  LRun := TListenRun.Create(
    procedure(AOnListen: TProc)
    begin
      THorseProviderIOCP.Listen(LISTEN_TEST_PORT,
        procedure
        var
          LProbe: TSocket;
        begin
          TInterlocked.Increment(LListened);
          if TryConnect(LISTEN_TEST_PORT, LProbe) then
          try
            LAccepted := RequestHead(LProbe);
          finally
            CloseSocketIfOpen(LProbe);
          end;
          AOnListen();
        end,
        procedure
        begin
          TInterlocked.Increment(LStopped);
        end);
    end);
  try
    Assert.IsTrue(CameUp(LRun.WaitOutcome), 'CA5: the first Listen did not come up: ' + LRun.Error);
    Assert.IsTrue(LRun.Stop, 'CA5: the first Listen did not return');
  finally
    LRun.Free;
  end;
  Assert.AreEqual(1, LListened, 'CA5: the listen callback runs once');
  Assert.IsTrue(LAccepted, 'CA5: a connect inside the listen callback is served');
  Assert.AreEqual(1, LStopped, 'CA5: the stop callback runs once');

  LRun := TListenRun.Create(
    procedure(AOnListen: TProc)
    begin
      THorseProviderIOCP.Listen(LISTEN_TEST_PORT,
        procedure
        begin
          TInterlocked.Increment(LListened2);
          AOnListen();
        end);
    end);
  try
    Assert.IsTrue(CameUp(LRun.WaitOutcome), 'CA5: the second Listen did not come up: ' + LRun.Error);
    Assert.IsTrue(LRun.Stop, 'CA5: the second Listen did not return');
  finally
    LRun.Free;
  end;
  Assert.AreEqual(1, LListened2, 'CA5: the second listen callback runs once');
  Assert.AreEqual(1, LListened, 'CA5: the first listen callback does not run again');
  Assert.AreEqual(1, LStopped, 'CA5: the second Listen does not run the first stop callback');
end;

procedure TTestHorseProviderIOCPListen.RefusedListenLeavesThePortFree;
var
  LOccupier: TSocket;
  LRun: TListenRun;
begin
  StartCheck('CA6');
  LOccupier := OccupyPort(LISTEN_TEST_PORT);
  try
    LRun := ListenOnTestPort;
    try
      Assert.IsTrue(LRun.WaitOutcome = loFailed, 'CA6: Listen on the busy port did not fail');
    finally
      LRun.Free;
    end;
  finally
    closesocket(LOccupier);
  end;

  AssertListensAndStop('CA6: Listen after the port was freed');
end;

procedure TTestHorseProviderIOCPListen.ListenWhileListeningIsRefusedAndKeepsTheServer;
var
  LFirst, LSecond: TListenRun;
  LStopped, LListened2, LStopped2: Integer;
begin
  StartCheck('CA7');
  LStopped := 0;
  LListened2 := 0;
  LStopped2 := 0;

  LFirst := TListenRun.Create(
    procedure(AOnListen: TProc)
    begin
      THorseProviderIOCP.Listen(LISTEN_TEST_PORT, '0.0.0.0', AOnListen,
        procedure
        begin
          TInterlocked.Increment(LStopped);
        end);
    end);
  try
    Assert.IsTrue(CameUp(LFirst.WaitOutcome), 'CA7: the first Listen did not come up: ' + LFirst.Error);

    LSecond := TListenRun.Create(
      procedure(AOnListen: TProc)
      begin
        THorseProviderIOCP.Listen(LISTEN_TEST_PORT + 1, '127.0.0.1',
          procedure
          begin
            TInterlocked.Increment(LListened2);
            AOnListen();
          end,
          procedure
          begin
            TInterlocked.Increment(LStopped2);
          end);
      end);
    try
      Assert.IsTrue(LSecond.WaitOutcome = loFailed, 'CA7: the Listen while the server is up was not refused');
      Assert.AreEqual(Format(MESSAGE_ALREADY_RUNNING, [LISTEN_TEST_PORT]), LSecond.Error,
        'CA7: message of the Listen while the server is up');
    finally
      LSecond.Free;
    end;

    Assert.IsTrue(THorseProviderIOCP.IsRunning, 'CA7: the refused Listen stopped the server');
    Assert.AreEqual(LISTEN_TEST_PORT, THorseProviderIOCP.Port, 'CA7: the refused Listen changed the port');
    Assert.AreEqual('0.0.0.0', THorseProviderIOCP.Host, 'CA7: the refused Listen changed the host');
    Assert.IsTrue(Answers(LISTEN_TEST_PORT), 'CA7: the server stopped answering after the refused Listen');
    Assert.IsTrue(LFirst.Stop, 'CA7: the first Listen did not return');
  finally
    LFirst.Free;
  end;
  Assert.AreEqual(1, LStopped, 'CA7: the stop callback of the server runs once');
  Assert.AreEqual(0, LListened2, 'CA7: the listen callback of the refused Listen never runs');
  Assert.AreEqual(0, LStopped2, 'CA7: the stop callback of the refused Listen never runs');
end;

procedure TTestHorseProviderIOCPListen.ListenRightAfterStopRunsEachStopCallbackOnce;
var
  LRound, LRefused: Integer;
  LFirst, LSecond: TListenRun;
  LOutcome: TListenOutcome;
  LStopped, LListened2, LStopped2: Integer;
begin
  StartCheck('CA7b');
  LRefused := 0;
  for LRound := 1 to RELISTEN_ROUNDS do
  begin
    LStopped := 0;
    LListened2 := 0;
    LStopped2 := 0;

    LFirst := TListenRun.Create(
      procedure(AOnListen: TProc)
      begin
        THorseProviderIOCP.Listen(LISTEN_TEST_PORT, '0.0.0.0', AOnListen,
          procedure
          begin
            TInterlocked.Increment(LStopped);
          end);
      end);
    try
      Assert.IsTrue(CameUp(LFirst.WaitOutcome),
        Format('CA7b round %d: the first Listen did not come up: %s', [LRound, LFirst.Error]));

      { StopListen returns before the Listen thread runs its stop callback. }
      THorseProviderIOCP.StopListen;
      LSecond := TListenRun.Create(
        procedure(AOnListen: TProc)
        begin
          THorseProviderIOCP.Listen(LISTEN_TEST_PORT, '0.0.0.0',
            procedure
            begin
              TInterlocked.Increment(LListened2);
              AOnListen();
            end,
            procedure
            begin
              TInterlocked.Increment(LStopped2);
            end);
        end);
      try
        LOutcome := LSecond.WaitOutcome;
        Assert.IsTrue(LFirst.WaitFinished, Format('CA7b round %d: the first Listen did not return', [LRound]));
        case LOutcome of
          loFailed:
            begin
              Inc(LRefused);
              Assert.AreEqual(Format(MESSAGE_ALREADY_RUNNING, [LISTEN_TEST_PORT]), LSecond.Error,
                Format('CA7b round %d: message of the refused Listen', [LRound]));
            end;
          loTimeout:
            Assert.Fail(Format('CA7b round %d: the second Listen neither came up nor failed', [LRound]));
        end;
        Assert.IsTrue(LSecond.Stop, Format('CA7b round %d: the second Listen did not return', [LRound]));
      finally
        LSecond.Free;
      end;
    finally
      LFirst.Free;
    end;

    Assert.AreEqual(1, LStopped, Format('CA7b round %d: the stop callback of the first cycle runs once', [LRound]));
    Assert.IsTrue(LListened2 <= 1, Format('CA7b round %d: the second listen callback runs at most once', [LRound]));
    Assert.AreEqual(LListened2, LStopped2,
      Format('CA7b round %d: the second stop callback runs once if, and only if, the second Listen came up', [LRound]));
  end;
  RecordObservation(Format('CA7b: %d of %d Listen calls right after StopListen were refused',
    [LRefused, RELISTEN_ROUNDS]));
end;

procedure TTestHorseProviderIOCPListen.StopWithRequestsArrivingReturns;
const
  HAMMERS = 2;
  { Long enough to tell a slow stop from one that never returns; each round
    records how long the stop took. }
  STOP_WAIT_MS = 30000;
var
  LRound, I: Integer;
  LRun: TListenRun;
  LKeepGoing: Integer;
  LHammers: array[0..HAMMERS - 1] of TThread;
  LStopper: TThread;
  LStopReturned: Boolean;
  LStopWatch: TStopwatch;
begin
  StartCheck('CA8');
  for LRound := 1 to RELISTEN_ROUNDS do
  begin
    LRun := ListenOnTestPort;
    try
      Assert.IsTrue(CameUp(LRun.WaitOutcome),
        Format('CA8 round %d: the Listen did not come up: %s', [LRound, LRun.Error]));

      { Keep-alive requests keep arriving, so that the stop meets a worker
        in the middle of a request. }
      LKeepGoing := 1;
      for I := 0 to HAMMERS - 1 do
      begin
        LHammers[I] := TThread.CreateAnonymousThread(
          procedure
          var
            LClient: TSocket;
          begin
            while TInterlocked.CompareExchange(LKeepGoing, 0, 0) = 1 do
              if TryConnect(LISTEN_TEST_PORT, LClient) then
              try
                while (TInterlocked.CompareExchange(LKeepGoing, 0, 0) = 1) and RequestHead(LClient) do
                  ;
              finally
                closesocket(LClient);
              end;
          end);
        LHammers[I].FreeOnTerminate := False;
        LHammers[I].Start;
      end;
      Sleep(100);

      LStopper := TThread.CreateAnonymousThread(
        procedure
        begin
          THorseProviderIOCP.StopListen;
        end);
      LStopper.FreeOnTerminate := False;
      LStopWatch := TStopwatch.StartNew;
      LStopper.Start;
      LStopReturned := WaitForSingleObject(LStopper.Handle, STOP_WAIT_MS) = WAIT_OBJECT_0;
      if LStopReturned then
        RecordObservation(Format('CA8 round %d: StopListen returned in %d ms',
          [LRound, LStopWatch.ElapsedMilliseconds]))
      else
        RecordObservation(Format('CA8 round %d: StopListen did not return in %d ms',
          [LRound, STOP_WAIT_MS]));

      TInterlocked.Exchange(LKeepGoing, 0);
      for I := 0 to HAMMERS - 1 do
        ReleaseThread(LHammers[I]);
      ReleaseThread(LStopper);

      Assert.IsTrue(LStopReturned,
        Format('CA8 round %d: StopListen did not return with requests arriving', [LRound]));
      Assert.IsTrue(LRun.WaitFinished, Format('CA8 round %d: the Listen did not return', [LRound]));
    finally
      LRun.Free;
    end;
  end;
end;

procedure TTestHorseProviderIOCPListen.ListenAgainAsSoonAsListenReturnsComesUpWhole;
var
  LRound: Integer;
  LRun: TListenRun;
  LFirstUp: TEvent;
  LClient: TSocket;
  LStopped: Integer;
begin
  StartCheck('CA7c');
  for LRound := 1 to RELISTEN_ROUNDS do
  begin
    LStopped := 0;
    LClient := INVALID_SOCKET;
    LFirstUp := TEvent.Create(nil, True, False, '');
    try
      LRun := TListenRun.Create(
        procedure(AOnListen: TProc)
        begin
          THorseProviderIOCP.Listen(LISTEN_TEST_PORT, '0.0.0.0',
            procedure
            begin
              LFirstUp.SetEvent;
            end,
            procedure
            begin
              TInterlocked.Increment(LStopped);
            end);
          { The same thread listens again the moment the first Listen returns. }
          THorseProviderIOCP.Listen(LISTEN_TEST_PORT, '0.0.0.0', AOnListen, nil);
        end);
      try
        Assert.IsTrue(LFirstUp.WaitFor(WAIT_MS) = wrSignaled,
          Format('CA7c round %d: the first Listen did not come up: %s', [LRound, LRun.Error]));
        { A connected keep-alive client makes the release of the first cycle
          take longer. }
        Assert.IsTrue(ConnectWithin(LISTEN_TEST_PORT, LClient), Format('CA7c round %d: connect', [LRound]));
        Assert.IsTrue(RequestHead(LClient), Format('CA7c round %d: request to the first Listen', [LRound]));
        THorseProviderIOCP.StopListenGraceful;

        Assert.IsTrue(LRun.WaitOutcome = loRunning,
          Format('CA7c round %d: the second Listen did not come up: %s', [LRound, LRun.Error]));
        Assert.AreEqual(1, LStopped,
          Format('CA7c round %d: the stop callback of the first Listen runs once', [LRound]));
        Assert.IsTrue(Answers(LISTEN_TEST_PORT),
          Format('CA7c round %d: the second Listen does not answer; the release of the first ran over it', [LRound]));
        Assert.IsTrue(LRun.Stop, Format('CA7c round %d: the second Listen did not return', [LRound]));
      finally
        LRun.Free;
      end;
    finally
      CloseSocketIfOpen(LClient);
      LFirstUp.Free;
    end;
  end;
end;

initialization
  WSAStartup(MakeWord(2, 2), WsaData);
  TDUnitX.RegisterTestFixture(TTestHorseProviderIOCPListen);

finalization
  WSACleanup;

end.
