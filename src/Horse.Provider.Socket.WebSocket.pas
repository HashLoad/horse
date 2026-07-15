unit Horse.Provider.Socket.WebSocket;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(FPC)}
    Sockets,
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Winapi.WinSock2,
    {$ELSE}
      Posix.SysSocket, Posix.Unistd,
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
    FClientIP: string;
    FServerPort: Integer;
  public
    constructor Create(ASocket: TSocket; const AClientIP: string = ''; const AServerPort: Integer = 0);
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

implementation

{ THorseWebSocketSocketTransport }

constructor THorseWebSocketSocketTransport.Create(ASocket: TSocket; const AClientIP: string; const AServerPort: Integer);
begin
  inherited Create;
  FSocket := ASocket;
  FIsClosed := False;
  FClientIP := AClientIP;
  FServerPort := AServerPort;
end;

function THorseWebSocketSocketTransport.Read(var ABuffer: TBytes; const ACount: Integer): Integer;
begin
  Result := 0;
  if FIsClosed then
    Exit;
  try
    {$IF DEFINED(FPC)}
      Result := fprecv(FSocket, @ABuffer[0], ACount, 0);
    {$ELSE}
      {$IFDEF MSWINDOWS}
        Result := recv(FSocket, ABuffer[0], ACount, 0);
      {$ELSE}
        Result := recv(FSocket, ABuffer[0], ACount, 0);
      {$ENDIF}
    {$ENDIF}
    if Result <= 0 then
    begin
      Result := 0;
      FIsClosed := True;
    end;
  except
    Result := 0;
    FIsClosed := True;
  end;
end;

function THorseWebSocketSocketTransport.Write(const ABuffer: TBytes; const ACount: Integer): Integer;
begin
  Result := 0;
  if FIsClosed then
    Exit;
  try
    {$IF DEFINED(FPC)}
      Result := fpsend(FSocket, @ABuffer[0], ACount, 0);
    {$ELSE}
      {$IFDEF MSWINDOWS}
        Result := send(FSocket, ABuffer[0], ACount, 0);
      {$ELSE}
        Result := send(FSocket, ABuffer[0], ACount, 0);
      {$ENDIF}
    {$ENDIF}
    if Result < 0 then
    begin
      Result := 0;
      FIsClosed := True;
    end;
  except
    Result := 0;
    FIsClosed := True;
  end;
end;

procedure THorseWebSocketSocketTransport.Close;
begin
  if not FIsClosed then
  begin
    FIsClosed := True;
    try
      {$IF DEFINED(FPC)}
        CloseSocket(FSocket);
      {$ELSE}
        {$IFDEF MSWINDOWS}
          closesocket(FSocket);
        {$ELSE}
          Posix.Unistd.__close(FSocket);
        {$ENDIF}
      {$ENDIF}
    except
    end;
  end;
end;

function THorseWebSocketSocketTransport.IsConnected: Boolean;
begin
  Result := not FIsClosed;
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
