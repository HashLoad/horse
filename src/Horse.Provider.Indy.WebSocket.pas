unit Horse.Provider.Indy.WebSocket;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, IdContext, IdHTTPWebBrokerBridge, Web.HTTPApp, IdGlobal,
  Horse.Core.WebSocket;

type
  { Helper para expor a thread protegida em TIdHTTPAppRequest no Delphi e FPC }
  TIdHTTPAppRequestHelper = class helper for TIdHTTPAppRequest
  public
    function GetThread: TIdContext;
  end;

  { Implementação de IHorseWebSocketTransport baseada no TIdContext do Indy }
  TIndyWebSocketTransport = class(TInterfacedObject, IHorseWebSocketTransport)
  private
    FContext: TIdContext;
  public
    constructor Create(AContext: TIdContext);
    function Read(var ABuffer: TBytes; const ACount: Integer): Integer;
    function Write(const ABuffer: TBytes; const ACount: Integer): Integer;
    procedure Close;
    function IsConnected: Boolean;
    function GetClientIP: string;
    function GetServerPort: Integer;
  end;

  { Implementação do Upgrader WebSocket para o Indy }
  TIndyWebSocketUpgrader = class(THorseWebSocketUpgrader)
  private
    FContext: TIdContext;
    FWebRequest: TWebRequest;
    FWebResponse: TWebResponse;
  public
    constructor Create(AContext: TIdContext; AWebRequest: TWebRequest; AWebResponse: TWebResponse);
    function Upgrade(const APath: string; const AHeartbeatInterval: Integer = 0): IHorseWebSocketConnection; override;
  end;

implementation

{ TIdHTTPAppRequestHelper }

function TIdHTTPAppRequestHelper.GetThread: TIdContext;
begin
  Result := TIdContext(Self.FThread);
end;

{ TIndyWebSocketTransport }

constructor TIndyWebSocketTransport.Create(AContext: TIdContext);
begin
  inherited Create;
  FContext := AContext;
end;

function TIndyWebSocketTransport.Read(var ABuffer: TBytes; const ACount: Integer): Integer;
var
  LIdBytes: TIdBytes;
begin
  Result := 0;
  if not IsConnected then
    Exit;
  try
    FContext.Connection.IOHandler.ReadBytes(LIdBytes, ACount, False);
    if Length(LIdBytes) > 0 then
    begin
      SetLength(ABuffer, Length(LIdBytes));
      Move(LIdBytes[0], ABuffer[0], Length(LIdBytes));
      Result := Length(LIdBytes);
    end;
  except
    Result := 0;
  end;
end;

function TIndyWebSocketTransport.Write(const ABuffer: TBytes; const ACount: Integer): Integer;
var
  LIdBytes: TIdBytes;
begin
  Result := 0;
  if not IsConnected then
    Exit;
  try
    SetLength(LIdBytes, ACount);
    if ACount > 0 then
      Move(ABuffer[0], LIdBytes[0], ACount);
    FContext.Connection.IOHandler.Write(LIdBytes);
    Result := ACount;
  except
    Result := 0;
  end;
end;

procedure TIndyWebSocketTransport.Close;
begin
  try
    if IsConnected then
      FContext.Connection.Disconnect;
  except
  end;
end;

function TIndyWebSocketTransport.IsConnected: Boolean;
begin
  try
    Result := (FContext <> nil) and (FContext.Connection <> nil) and FContext.Connection.Connected;
  except
    Result := False;
  end;
end;

function TIndyWebSocketTransport.GetClientIP: string;
begin
  Result := FContext.Binding.PeerIP;
end;

function TIndyWebSocketTransport.GetServerPort: Integer;
begin
  Result := FContext.Binding.Port;
end;

{ TIndyWebSocketUpgrader }

constructor TIndyWebSocketUpgrader.Create(AContext: TIdContext; AWebRequest: TWebRequest; AWebResponse: TWebResponse);
begin
  inherited Create;
  FContext := AContext;
  FWebRequest := AWebRequest;
  FWebResponse := AWebResponse;
end;

function TIndyWebSocketUpgrader.Upgrade(const APath: string; const AHeartbeatInterval: Integer): IHorseWebSocketConnection;
var
  LAcceptKey: string;
  LHandshakeResponse: string;
  LTransport: IHorseWebSocketTransport;
  LConnection: IHorseWebSocketConnection;
  LClientKey: string;
  LBuffer: TBytes;
  LBytesRead: Integer;
begin
  LClientKey := FWebRequest.GetFieldByName('Sec-WebSocket-Key');
  LAcceptKey := THorseWebSocketHandshake.CalculateAcceptKey(LClientKey);

  LHandshakeResponse :=
    'HTTP/1.1 101 Switching Protocols' + #13#10 +
    'Upgrade: websocket' + #13#10 +
    'Connection: Upgrade' + #13#10 +
    'Sec-WebSocket-Accept: ' + LAcceptKey + #13#10#13#10;
    
  FContext.Connection.IOHandler.Write(LHandshakeResponse);

  LTransport := TIndyWebSocketTransport.Create(FContext);
  LConnection := THorseWebSocketConnection.Create(LTransport, APath, AHeartbeatInterval);

  // Executa o loop bloqueante na thread atual do Indy, mantendo o socket aberto
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
