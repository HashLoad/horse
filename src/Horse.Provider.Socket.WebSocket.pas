unit Horse.Provider.Socket.WebSocket;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(FPC)}
    SyncObjs,
    Sockets,
    {$IFDEF MSWINDOWS}
      { FIX-WS-NONBLOCK  WSAGetLastError / select / TFDSet }
      WinSock2,
    {$ELSE}
      { FIX-WS-NONBLOCK  fpgeterrno, ESysEAGAIN, fpSelect, fpFD_* , TTimeVal }
      BaseUnix,
    {$ENDIF}
  {$ELSE}
    System.SyncObjs,
    {$IFDEF MSWINDOWS}
      Winapi.WinSock2,
    {$ELSE}
      Posix.SysSocket, Posix.Unistd,
      { FIX-WS-NONBLOCK  errno / EAGAIN / select / fd_set / timeval }
      Posix.Errno, Posix.SysSelect, Posix.SysTime,
    {$ENDIF}
  {$ENDIF}
  Horse.Core.WebSocket;

type
  {$IFDEF FPC}
    {$IFDEF MSWINDOWS}
      TSocket = THandle;
    {$ELSE}
      TSocket = LongInt;
    {$ENDIF}
  {$ELSE}
    {$IFNDEF MSWINDOWS}
      TSocket = Integer;
    {$ENDIF}
  {$ENDIF}

  { Transporte de socket bruto multiplataforma (WinSock2 / POSIX) }
  THorseWebSocketSocketTransport = class(TInterfacedObject, IHorseWebSocketTransport)
  private
    FSocket: TSocket;
    FIsClosed: Boolean;
    FSocketClosed: Boolean;
    FActiveOperations: Integer;
    FStateLock: TCriticalSection;
    FOperationsFinished: TEvent;
    FClientIP: string;
    FServerPort: Integer;
    { FIX-WS-NONBLOCK }
    function BeginOperation(out ASocket: TSocket): Boolean;
    procedure EndOperation;
    function GetLastSocketError: Integer;
    function IsInterrupted(const AError: Integer): Boolean;
    function WouldBlock(const AError: Integer): Boolean;
    function WaitReadable(const ASocket: TSocket; const ATimeoutMS: Integer): Boolean;
    function Closing: Boolean;
    procedure RequestClose(const AWaitForOperations: Boolean);
  public
    constructor Create(ASocket: TSocket; const AClientIP: string = ''; const AServerPort: Integer = 0);
    destructor Destroy; override;
    function Read(var ABuffer: TBytes; const ACount: Integer): Integer;
    function Write(const ABuffer: TBytes; const ACount: Integer): Integer;
    procedure Close;
    function IsConnected: Boolean;
    function GetClientIP: string;
    function GetServerPort: Integer;
  end;

  { Upgrader para Provedores baseados em Sockets Brutos (IOCP, Epoll, etc.) }
  THorseWebSocketSocketUpgrader = class(THorseWebSocketUpgrader)
  private
    FSocket: TSocket;
    FClientIP: string;
    FServerPort: Integer;
    FClientKey: string;
  public
    constructor Create(ASocket: TSocket; const AClientKey: string; const AClientIP: string = ''; const AServerPort: Integer = 0);
    function Upgrade(const APath: string; const AHeartbeatInterval: Integer = 0): IHorseWebSocketConnection; override;
  end;

const
  { FIX-WS-NONBLOCK  one select() tick. A timeout is not a disconnect, so Read
    simply retries; this only bounds how often the loop re-checks FIsClosed. }
  WS_SOCKET_READ_TICK_MS = 250;

implementation

{ THorseWebSocketSocketTransport }

constructor THorseWebSocketSocketTransport.Create(ASocket: TSocket; const AClientIP: string; const AServerPort: Integer);
begin
  inherited Create;
  FSocket := ASocket;
  FIsClosed := False;
  FSocketClosed := False;
  FActiveOperations := 0;
  FStateLock := TCriticalSection.Create;
  FOperationsFinished := TEvent.Create(nil, True, True, '');
  FClientIP := AClientIP;
  FServerPort := AServerPort;
end;

destructor THorseWebSocketSocketTransport.Destroy;
begin
  Close;
  FOperationsFinished.Free;
  FStateLock.Free;
  inherited;
end;

{ FIX-WS-NONBLOCK  distinguish "no data yet" from "peer closed".

  IOCP and epoll both put accepted client sockets into non-blocking mode --
  epoll requires it (Horse.Provider.Epoll.pas sets O_NONBLOCK on every accepted
  fd). On a non-blocking socket, recv returns -1 with EAGAIN/EWOULDBLOCK to mean
  "nothing available right now", which is the normal state of an idle WebSocket
  peer, not a disconnect.

  Treating every non-positive result as closed made the upgrader's read loop
  break on its very first iteration: the connection was marked dead about a
  millisecond after the 101, before any frame could arrive. The handler then
  returned and the HTTP pipeline resumed, writing a stray "HTTP/1.1 200 OK"
  onto a socket that had already been handed to WebSocket.

  Only two results actually mean the connection is over:
    recv = 0   orderly shutdown by the peer
    recv < 0   with an errno that is NOT EAGAIN/EWOULDBLOCK/EINTR

  Anything else means wait. WaitReadable blocks in select() so an idle peer
  costs no CPU, and the loop is bounded only by the socket's own lifetime --
  which is correct: a WebSocket peer may legitimately stay silent for minutes. }

function THorseWebSocketSocketTransport.BeginOperation(out ASocket: TSocket): Boolean;
begin
  FStateLock.Enter;
  try
    Result := not FIsClosed and not FSocketClosed;
    if not Result then
      Exit;
    ASocket := FSocket;
    if FActiveOperations = 0 then
      FOperationsFinished.ResetEvent;
    Inc(FActiveOperations);
  finally
    FStateLock.Leave;
  end;
end;

procedure THorseWebSocketSocketTransport.EndOperation;
var
  LSocket: TSocket;
  LCloseSocket: Boolean;
  LSignalFinished: Boolean;
begin
  LSocket := 0;
  LCloseSocket := False;
  FStateLock.Enter;
  try
    Dec(FActiveOperations);
    LSignalFinished := FActiveOperations = 0;
    if LSignalFinished and FIsClosed and not FSocketClosed then
    begin
      LSocket := FSocket;
      FSocketClosed := True;
      LCloseSocket := True;
    end;
  finally
    FStateLock.Leave;
  end;

  if LCloseSocket then
  begin
    {$IF DEFINED(FPC)}
      CloseSocket(LSocket);
    {$ELSE}
      {$IFDEF MSWINDOWS}
        closesocket(LSocket);
      {$ELSE}
        Posix.Unistd.__close(LSocket);
      {$ENDIF}
    {$ENDIF}
  end;

  if LSignalFinished then
    FOperationsFinished.SetEvent;
end;

function THorseWebSocketSocketTransport.GetLastSocketError: Integer;
{$IF DEFINED(FPC)}
  {$IFDEF MSWINDOWS}
  begin
    Result := WSAGetLastError;
  end;
  {$ELSE}
  begin
    Result := fpgeterrno;
  end;
  {$ENDIF}
{$ELSE}
  {$IFDEF MSWINDOWS}
  begin
    Result := WSAGetLastError;
  end;
  {$ELSE}
  begin
    Result := errno;
  end;
  {$ENDIF}
{$ENDIF}

function THorseWebSocketSocketTransport.IsInterrupted(const AError: Integer): Boolean;
begin
  {$IF DEFINED(FPC)}
    {$IFDEF MSWINDOWS}
      Result := AError = WSAEINTR;
    {$ELSE}
      Result := AError = ESysEINTR;
    {$ENDIF}
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Result := AError = WSAEINTR;
    {$ELSE}
      Result := AError = EINTR;
    {$ENDIF}
  {$ENDIF}
end;

function THorseWebSocketSocketTransport.WouldBlock(const AError: Integer): Boolean;
begin
  {$IF DEFINED(FPC)}
    {$IFDEF MSWINDOWS}
      Result := AError = WSAEWOULDBLOCK;
    {$ELSE}
      Result := (AError = ESysEAGAIN) or (AError = ESysEWOULDBLOCK);
    {$ENDIF}
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Result := AError = WSAEWOULDBLOCK;
    {$ELSE}
      Result := (AError = EAGAIN) or (AError = EWOULDBLOCK);
    {$ENDIF}
  {$ENDIF}
end;

function THorseWebSocketSocketTransport.WaitReadable(const ASocket: TSocket;
  const ATimeoutMS: Integer): Boolean;
{$IF DEFINED(FPC)}
  {$IFDEF MSWINDOWS}
  var
    LSet: TFDSet;
    LTimeout: TTimeVal;
  begin
    LSet.fd_count := 1;
    LSet.fd_array[0] := ASocket;
    LTimeout.tv_sec := ATimeoutMS div 1000;
    LTimeout.tv_usec := (ATimeoutMS mod 1000) * 1000;
    Result := select(0, @LSet, nil, nil, @LTimeout) > 0;
  end;
  {$ELSE}
  var
    LSet: TFDSet;
    LTimeout: TTimeVal;
  begin
    fpFD_ZERO(LSet);
    fpFD_SET(ASocket, LSet);
    LTimeout.tv_sec := ATimeoutMS div 1000;
    LTimeout.tv_usec := (ATimeoutMS mod 1000) * 1000;
    Result := fpSelect(ASocket + 1, @LSet, nil, nil, @LTimeout) > 0;
  end;
  {$ENDIF}
{$ELSE}
  {$IFDEF MSWINDOWS}
  var
    LSet: TFDSet;
    LTimeout: TTimeVal;
  begin
    { Winapi.WinSock2 exposes FD_SET as a TYPE, not the usual macro-style
      procedure, so the members are filled in by hand. }
    LSet.fd_count := 1;
    LSet.fd_array[0] := ASocket;
    LTimeout.tv_sec := ATimeoutMS div 1000;
    LTimeout.tv_usec := (ATimeoutMS mod 1000) * 1000;
    Result := select(0, @LSet, nil, nil, @LTimeout) > 0;
  end;
  {$ELSE}
  var
    LSet: fd_set;
    LTimeout: timeval;
  begin
    FD_ZERO(LSet);
    FD_SET(ASocket, LSet);
    LTimeout.tv_sec := ATimeoutMS div 1000;
    LTimeout.tv_usec := (ATimeoutMS mod 1000) * 1000;
    Result := select(ASocket + 1, @LSet, nil, nil, @LTimeout) > 0;
  end;
  {$ENDIF}
{$ENDIF}

function THorseWebSocketSocketTransport.Read(var ABuffer: TBytes; const ACount: Integer): Integer;
var
  LError: Integer;
  LSocket: TSocket;
begin
  Result := 0;
  if (ACount <= 0) or (Length(ABuffer) < ACount) or not BeginOperation(LSocket) then
    Exit;
  try
    try
      while not Closing do
      begin
        {$IF DEFINED(FPC)}
          Result := fprecv(LSocket, @ABuffer[0], ACount, 0);
        {$ELSE}
          {$IFDEF MSWINDOWS}
            Result := recv(LSocket, ABuffer[0], ACount, 0);
          {$ELSE}
            Result := recv(LSocket, ABuffer[0], ACount, 0);
          {$ENDIF}
        {$ENDIF}

        if Result > 0 then
          Exit;

        if Result = 0 then
        begin
          RequestClose(False);
          Exit;
        end;

        LError := GetLastSocketError;
        if IsInterrupted(LError) then
          Continue;
        if not WouldBlock(LError) then
        begin
          Result := 0;
          RequestClose(False);
          Exit;
        end;

        WaitReadable(LSocket, WS_SOCKET_READ_TICK_MS);
      end;
      Result := 0;
    except
      Result := 0;
      RequestClose(False);
    end;
  finally
    EndOperation;
  end;
end;

function THorseWebSocketSocketTransport.Write(const ABuffer: TBytes; const ACount: Integer): Integer;
var
  LSocket: TSocket;
begin
  Result := 0;
  if (ACount <= 0) or (Length(ABuffer) < ACount) or not BeginOperation(LSocket) then
    Exit;
  try
    try
      {$IF DEFINED(FPC)}
        Result := fpsend(LSocket, @ABuffer[0], ACount, 0);
      {$ELSE}
        {$IFDEF MSWINDOWS}
          Result := send(LSocket, ABuffer[0], ACount, 0);
        {$ELSE}
          Result := send(LSocket, ABuffer[0], ACount, 0);
        {$ENDIF}
      {$ENDIF}
      if Result < 0 then
      begin
        Result := 0;
        RequestClose(False);
      end;
    except
      Result := 0;
      RequestClose(False);
    end;
  finally
    EndOperation;
  end;
end;

procedure THorseWebSocketSocketTransport.Close;
begin
  RequestClose(True);
end;

function THorseWebSocketSocketTransport.IsConnected: Boolean;
begin
  Result := not Closing;
end;

function THorseWebSocketSocketTransport.Closing: Boolean;
begin
  FStateLock.Enter;
  try
    Result := FIsClosed;
  finally
    FStateLock.Leave;
  end;
end;

procedure THorseWebSocketSocketTransport.RequestClose(
  const AWaitForOperations: Boolean);
var
  LSocket: TSocket;
  LCloseSocket: Boolean;
  LHasOperations: Boolean;
begin
  LCloseSocket := False;
  FStateLock.Enter;
  try
    FIsClosed := True;
    if FSocketClosed then
      Exit;
    LSocket := FSocket;
    LHasOperations := FActiveOperations > 0;
    if not LHasOperations then
    begin
      FSocketClosed := True;
      LCloseSocket := True;
    end;
  finally
    FStateLock.Leave;
  end;

  if LHasOperations then
  begin
    {$IF DEFINED(FPC)}
      fpShutdown(LSocket, 2);
    {$ELSE}
      {$IFDEF MSWINDOWS}
        shutdown(LSocket, SD_BOTH);
      {$ELSE}
        shutdown(LSocket, SHUT_RDWR);
      {$ENDIF}
    {$ENDIF}
    if AWaitForOperations then
      FOperationsFinished.WaitFor(INFINITE);
  end
  else if LCloseSocket then
  begin
    {$IF DEFINED(FPC)}
      CloseSocket(LSocket);
    {$ELSE}
      {$IFDEF MSWINDOWS}
        closesocket(LSocket);
      {$ELSE}
        Posix.Unistd.__close(LSocket);
      {$ENDIF}
    {$ENDIF}
    FOperationsFinished.SetEvent;
  end;
end;

function THorseWebSocketSocketTransport.GetClientIP: string;
begin
  Result := FClientIP;
end;

function THorseWebSocketSocketTransport.GetServerPort: Integer;
begin
  Result := FServerPort;
end;

{ THorseWebSocketSocketUpgrader }

constructor THorseWebSocketSocketUpgrader.Create(ASocket: TSocket; const AClientKey: string; const AClientIP: string; const AServerPort: Integer);
begin
  inherited Create;
  FSocket := ASocket;
  FClientKey := AClientKey;
  FClientIP := AClientIP;
  FServerPort := AServerPort;
end;

function THorseWebSocketSocketUpgrader.Upgrade(const APath: string; const AHeartbeatInterval: Integer): IHorseWebSocketConnection;
var
  LAcceptKey: string;
  LHandshakeResponse: string;
  LResponseBytes: TBytes;
  LTransport: IHorseWebSocketTransport;
  LConnection: IHorseWebSocketConnection;
  LBuffer: TBytes;
  LBytesRead: Integer;
begin
  LAcceptKey := THorseWebSocketHandshake.CalculateAcceptKey(FClientKey);

  LHandshakeResponse :=
    'HTTP/1.1 101 Switching Protocols' + #13#10 +
    'Upgrade: websocket' + #13#10 +
    'Connection: Upgrade' + #13#10 +
    'Sec-WebSocket-Accept: ' + LAcceptKey + #13#10#13#10;
    
  LResponseBytes := TEncoding.ASCII.GetBytes(LHandshakeResponse);
  
  LTransport := THorseWebSocketSocketTransport.Create(FSocket, FClientIP, FServerPort);
  LTransport.Write(LResponseBytes, Length(LResponseBytes));

  LConnection := THorseWebSocketConnection.Create(LTransport, APath, AHeartbeatInterval);

  if Assigned(OnConnect) then
  begin
    try
      OnConnect(LConnection);
    except
      on E: Exception do
      begin
        LConnection.TriggerError(E);
        LConnection.Close(1011, 'Internal Error');
        LConnection.TriggerDisconnect;
        raise;
      end;
    end;
  end;

  // Executa o loop bloqueante na thread worker do provedor (IOCP ou Epoll)
  SetLength(LBuffer, 4096);
  try
    while LConnection.IsConnected do
    begin
      LBytesRead := LTransport.Read(LBuffer, 4096);
      if LBytesRead > 0 then
        LConnection.HandleIncomingBytes(LBuffer, LBytesRead)
      else
        Break;
    end;
  finally
    LConnection.TriggerDisconnect;
  end;

  Result := LConnection;
end;

end.
