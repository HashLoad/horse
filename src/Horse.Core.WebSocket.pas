unit Horse.Core.WebSocket;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
    SysUtils, Classes, Generics.Collections, syncobjs, sha1, base64, fgl, strutils;
  {$ELSE}
    System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs,
    {$IF CompilerVersion >= 29.0}
      System.Hash, System.NetEncoding;
    {$ELSE}
      IdHashSHA, IdEncoderMIME;
    {$ENDIF}
  {$ENDIF}

type
  TWebSocketMessageType = (wsmtText, wsmtBinary, wsmtClose, wsmtPing, wsmtPong);

  IHorseWebSocketConnection = interface;

  TOnWebSocketConnect = {$IF DEFINED(FPC)}procedure{$ELSE}reference to procedure{$ENDIF}(const AConnection: IHorseWebSocketConnection);
  TOnWebSocketMessage = {$IF DEFINED(FPC)}procedure{$ELSE}reference to procedure{$ENDIF}(const AConnection: IHorseWebSocketConnection; const AMessage: string);
  TOnWebSocketBinary = {$IF DEFINED(FPC)}procedure{$ELSE}reference to procedure{$ENDIF}(const AConnection: IHorseWebSocketConnection; const AData: TBytes);
  TOnWebSocketDisconnect = {$IF DEFINED(FPC)}procedure{$ELSE}reference to procedure{$ENDIF}(const AConnection: IHorseWebSocketConnection);
  TOnWebSocketError = {$IF DEFINED(FPC)}procedure{$ELSE}reference to procedure{$ENDIF}(const AConnection: IHorseWebSocketConnection; const AException: Exception);

  { Interface de transporte abstrata para desacoplar o Core do canal físico do Socket }
  IHorseWebSocketTransport = interface
    ['{E1F2A3B4-C5D6-4E7F-8A9B-0C1D2E3F4A5B}']
    function Read(var ABuffer: TBytes; const ACount: Integer): Integer;
    function Write(const ABuffer: TBytes; const ACount: Integer): Integer;
    procedure Close;
    function IsConnected: Boolean;
    function GetClientIP: string;
    function GetServerPort: Integer;
  end;

  { Representação da conexão WebSocket para o desenvolvedor final }
  IHorseWebSocketConnection = interface
    ['{C7C3B4A1-4D2E-4A8E-9B6C-1F0D3E2A4B5D}']
    procedure SendText(const AText: string);
    procedure SendBinary(const AData: TBytes);
    procedure Ping(const AData: TBytes = nil);
    procedure Close(const AStatus: Word = 1000; const AReason: string = '');
    function IsConnected: Boolean;
    function GetClientIP: string;
    function GetServerPort: Integer;
    function GetPath: string;

    // Registradores de callbacks de eventos da conexão ativa
    procedure SetOnMessage(const ACallback: TOnWebSocketMessage);
    procedure SetOnBinary(const ACallback: TOnWebSocketBinary);
    procedure SetOnDisconnect(const ACallback: TOnWebSocketDisconnect);
    procedure SetOnError(const ACallback: TOnWebSocketError);
    procedure TriggerError(const AException: Exception);

    // Métodos internos de recebimento chamados pelos parsers dos provedores
    procedure HandleIncomingBytes(const ABytes: TBytes; const ALength: Integer);
    procedure TriggerDisconnect;

    // Propriedades expostas para facilidade de uso
    property ClientIP: string read GetClientIP;
    property ServerPort: Integer read GetServerPort;
    property Path: string read GetPath;
    property OnMessage: TOnWebSocketMessage write SetOnMessage;
    property OnBinary: TOnWebSocketBinary write SetOnBinary;
    property OnDisconnect: TOnWebSocketDisconnect write SetOnDisconnect;
    property OnError: TOnWebSocketError write SetOnError;
  end;

  EHorseWebSocketException = class(Exception);
  EHorseWebSocketProtocolError = class(EHorseWebSocketException);
  EHorseWebSocketNotSupported = class(EHorseWebSocketException);

  THorseWebSocketUpgrader = class
  public
    function Upgrade(const APath: string; const AHeartbeatInterval: Integer = 0): IHorseWebSocketConnection; virtual; abstract;
  end;

  { Cálculo de chaves e handshake da RFC 6455 }
  THorseWebSocketHandshake = class
  public
    class function CalculateAcceptKey(const AClientKey: string): string; static;
  end;

  { Estrutura Interna de Frame WebSocket RFC 6455 }
  TWebSocketFrame = record
    Fin: Boolean;
    Opcode: Byte;
    Mask: Boolean;
    PayloadLen: Int64;
    MaskKey: array[0..3] of Byte;
    Payload: TBytes;
  end;

  { Classe Gerenciadora de Conexões e Broadcast por Rota }
  THorseWebSocketManager = class
  private
    class var FSync: TCriticalSection;
    class var FConnections: TObjectDictionary<string, TList<IHorseWebSocketConnection>>;
    class constructor CreateClass;
    class destructor DestroyClass;
  public
    class procedure RegisterConnection(const APath: string; const AConnection: IHorseWebSocketConnection); static;
    class procedure UnregisterConnection(const APath: string; const AConnection: IHorseWebSocketConnection); static;
    class procedure Broadcast(const APath: string; const AMessage: string); overload; static;
    class procedure Broadcast(const APath: string; const AMessage: string; const AIgnore: IHorseWebSocketConnection); overload; static;
    class procedure Broadcast(const APath: string; const AData: TBytes); overload; static;
    class procedure Broadcast(const APath: string; const AData: TBytes; const AIgnore: IHorseWebSocketConnection); overload; static;
    class function GetConnections(const APath: string): TArray<IHorseWebSocketConnection>; static;
  end;

  { Máquina de Estados para Parsing e Decodificação de Frames WebSocket }
  THorseWebSocketParser = class
  private
    FBuffer: TBytes;
    FBufferLen: Integer;
    FAccumulatedPayload: TBytes;
    FAccumulatedOpcode: Byte;
    procedure CompactBuffer(AConsumed: Integer);
    function ParseFrame(out AFrame: TWebSocketFrame; out AConsumedBytes: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FeedBytes(const ABytes: TBytes; ALength: Integer; const AConnection: IHorseWebSocketConnection);
    class function BuildFrame(const AType: TWebSocketMessageType; const AData: TBytes): TBytes; static;
  end;

  { Implementação concreta e thread-safe da conexão WebSocket }
  THorseWebSocketConnection = class(TInterfacedObject, IHorseWebSocketConnection)
  private
    FTransport: IHorseWebSocketTransport;
    FPath: string;
    FParser: THorseWebSocketParser;
    FSyncWrite: TCriticalSection;
    FIsConnected: Boolean;
    FHeartbeatInterval: Integer;
    FHeartbeatThread: TThread;
    
    FOnMessage: TOnWebSocketMessage;
    FOnBinary: TOnWebSocketBinary;
    FOnDisconnect: TOnWebSocketDisconnect;
    FOnError: TOnWebSocketError;
    
    procedure SendRawFrame(const AType: TWebSocketMessageType; const AData: TBytes);
    procedure StartHeartbeat;
    procedure StopHeartbeat;
  public
    constructor Create(const ATransport: IHorseWebSocketTransport; const APath: string; const AHeartbeatInterval: Integer = 0);
    destructor Destroy; override;
    
    procedure SendText(const AText: string);
    procedure SendBinary(const AData: TBytes);
    procedure Ping(const AData: TBytes = nil);
    procedure Close(const AStatus: Word = 1000; const AReason: string = '');
    function IsConnected: Boolean;
    function GetClientIP: string;
    function GetServerPort: Integer;
    function GetPath: string;

    procedure SetOnMessage(const ACallback: TOnWebSocketMessage);
    procedure SetOnBinary(const ACallback: TOnWebSocketBinary);
    procedure SetOnDisconnect(const ACallback: TOnWebSocketDisconnect);
    procedure SetOnError(const ACallback: TOnWebSocketError);
    procedure TriggerError(const AException: Exception);

    procedure HandleIncomingBytes(const ABytes: TBytes; const ALength: Integer);
    procedure TriggerDisconnect;
  end;

implementation

{ THorseWebSocketHandshake }

class function THorseWebSocketHandshake.CalculateAcceptKey(const AClientKey: string): string;
const
  WS_GUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
var
  LConcat: string;
  LInputBytes: TBytes;
  LHashBytes: TBytes;
  {$IF DEFINED(FPC)}
    LSHA1Ctx: TSHA1Context;
    LDigest: TSHA1Digest;
  {$ELSE}
    {$IF CompilerVersion < 29.0}
      LSHA1: TIdHashSHA1;
    {$ENDIF}
  {$ENDIF}
begin
  LConcat := AClientKey + WS_GUID;
  LInputBytes := TEncoding.UTF8.GetBytes(LConcat);

  {$IF DEFINED(FPC)}
    SHA1Init(LSHA1Ctx);
    if Length(LInputBytes) > 0 then
      SHA1Update(LSHA1Ctx, LInputBytes[0], Length(LInputBytes));
    SHA1Final(LSHA1Ctx, LDigest);
    SetLength(LHashBytes, 20);
    Move(LDigest, LHashBytes[0], 20);
    Result := EncodeStringBase64(TEncoding.ASCII.GetString(LHashBytes));
    // FPC base64 encoding returns with CRLF suffix sometimes, strip it
    Result := ReplaceStr(ReplaceStr(Result, #13, ''), #10, '');
  {$ELSE}
    {$IF CompilerVersion >= 29.0}
      LHashBytes := THashSHA1.GetHashBytes(LConcat);
      Result := TNetEncoding.Base64.EncodeBytesToString(LHashBytes);
      Result := Result.Replace(#13, '').Replace(#10, '');
    {$ELSE}
      LSHA1 := TIdHashSHA1.Create;
      try
        // No Delphi XE7 com Indy
        LHashBytes := LSHA1.HashValue(LInputBytes);
        Result := TIdEncoderMIME.EncodeBytes(LHashBytes);
        Result := Result.Replace(#13, '').Replace(#10, '');
      finally
        LSHA1.Free;
      end;
    {$ENDIF}
  {$ENDIF}
end;

{ THorseWebSocketManager }

class constructor THorseWebSocketManager.CreateClass;
begin
  FSync := TCriticalSection.Create;
  FConnections := TObjectDictionary<string, TList<IHorseWebSocketConnection>>.Create([doOwnsValues]);
end;

class destructor THorseWebSocketManager.DestroyClass;
begin
  FConnections.Free;
  FSync.Free;
end;

class procedure THorseWebSocketManager.RegisterConnection(const APath: string; const AConnection: IHorseWebSocketConnection);
var
  LList: TList<IHorseWebSocketConnection>;
begin
  FSync.Enter;
  try
    if not FConnections.TryGetValue(APath, LList) then
    begin
      LList := TList<IHorseWebSocketConnection>.Create;
      FConnections.Add(APath, LList);
    end;
    LList.Add(AConnection);
  finally
    FSync.Leave;
  end;
end;

class procedure THorseWebSocketManager.UnregisterConnection(const APath: string; const AConnection: IHorseWebSocketConnection);
var
  LList: TList<IHorseWebSocketConnection>;
begin
  FSync.Enter;
  try
    if FConnections.TryGetValue(APath, LList) then
    begin
      LList.Remove(AConnection);
      if LList.Count = 0 then
        FConnections.Remove(APath);
    end;
  finally
    FSync.Leave;
  end;
end;

class procedure THorseWebSocketManager.Broadcast(const APath: string; const AMessage: string);
var
  LList: TList<IHorseWebSocketConnection>;
  LConn: IHorseWebSocketConnection;
begin
  FSync.Enter;
  try
    if FConnections.TryGetValue(APath, LList) then
    begin
      for LConn in LList do
      begin
        if LConn.IsConnected then
          LConn.SendText(AMessage);
      end;
    end;
  finally
    FSync.Leave;
  end;
end;

class procedure THorseWebSocketManager.Broadcast(const APath: string; const AMessage: string; const AIgnore: IHorseWebSocketConnection);
var
  LList: TList<IHorseWebSocketConnection>;
  LConn: IHorseWebSocketConnection;
begin
  FSync.Enter;
  try
    if FConnections.TryGetValue(APath, LList) then
    begin
      for LConn in LList do
      begin
        if (LConn <> AIgnore) and LConn.IsConnected then
          LConn.SendText(AMessage);
      end;
    end;
  finally
    FSync.Leave;
  end;
end;

class procedure THorseWebSocketManager.Broadcast(const APath: string; const AData: TBytes);
var
  LList: TList<IHorseWebSocketConnection>;
  LConn: IHorseWebSocketConnection;
begin
  FSync.Enter;
  try
    if FConnections.TryGetValue(APath, LList) then
    begin
      for LConn in LList do
      begin
        if LConn.IsConnected then
          LConn.SendBinary(AData);
      end;
    end;
  finally
    FSync.Leave;
  end;
end;

class procedure THorseWebSocketManager.Broadcast(const APath: string; const AData: TBytes; const AIgnore: IHorseWebSocketConnection);
var
  LList: TList<IHorseWebSocketConnection>;
  LConn: IHorseWebSocketConnection;
begin
  FSync.Enter;
  try
    if FConnections.TryGetValue(APath, LList) then
    begin
      for LConn in LList do
      begin
        if (LConn <> AIgnore) and LConn.IsConnected then
          LConn.SendBinary(AData);
      end;
    end;
  finally
    FSync.Leave;
  end;
end;

class function THorseWebSocketManager.GetConnections(const APath: string): TArray<IHorseWebSocketConnection>;
var
  LList: TList<IHorseWebSocketConnection>;
begin
  FSync.Enter;
  try
    if FConnections.TryGetValue(APath, LList) then
      Result := LList.ToArray
    else
      SetLength(Result, 0);
  finally
    FSync.Leave;
  end;
end;

{ THorseWebSocketParser }

constructor THorseWebSocketParser.Create;
begin
  inherited Create;
  SetLength(FBuffer, 0);
  FBufferLen := 0;
  SetLength(FAccumulatedPayload, 0);
  FAccumulatedOpcode := 0;
end;

destructor THorseWebSocketParser.Destroy;
begin
  SetLength(FBuffer, 0);
  SetLength(FAccumulatedPayload, 0);
  inherited Destroy;
end;

procedure THorseWebSocketParser.CompactBuffer(AConsumed: Integer);
begin
  if (AConsumed > 0) and (AConsumed <= FBufferLen) then
  begin
    if AConsumed < FBufferLen then
      Move(FBuffer[AConsumed], FBuffer[0], FBufferLen - AConsumed);
    FBufferLen := FBufferLen - AConsumed;
    SetLength(FBuffer, FBufferLen);
  end;
end;

function THorseWebSocketParser.ParseFrame(out AFrame: TWebSocketFrame; out AConsumedBytes: Integer): Boolean;
var
  LPayloadLenByte: Byte;
  LHeaderLen: Integer;
  LMaskKeyOffset: Integer;
  i: Integer;
begin
  Result := False;
  AConsumedBytes := 0;
  
  if FBufferLen < 2 then
    Exit;
    
  AFrame.Fin := (FBuffer[0] and $80) <> 0;
  AFrame.Opcode := FBuffer[0] and $0F;
  AFrame.Mask := (FBuffer[1] and $80) <> 0;
  LPayloadLenByte := FBuffer[1] and $7F;
  
  LHeaderLen := 2;
  if LPayloadLenByte = 126 then
  begin
    LHeaderLen := 4;
    if FBufferLen < LHeaderLen then
      Exit;
    AFrame.PayloadLen := (FBuffer[2] shl 8) or FBuffer[3];
  end
  else if LPayloadLenByte = 127 then
  begin
    LHeaderLen := 10;
    if FBufferLen < LHeaderLen then
      Exit;
    AFrame.PayloadLen := 0;
    for i := 0 to 7 do
      AFrame.PayloadLen := (AFrame.PayloadLen shl 8) or FBuffer[2 + i];
  end
  else
    AFrame.PayloadLen := LPayloadLenByte;
    
  LMaskKeyOffset := LHeaderLen;
  if AFrame.Mask then
    Inc(LHeaderLen, 4);
    
  if FBufferLen < LHeaderLen + AFrame.PayloadLen then
    Exit; // Frame incompleto ainda
    
  if AFrame.Mask then
    Move(FBuffer[LMaskKeyOffset], AFrame.MaskKey[0], 4);
    
  SetLength(AFrame.Payload, AFrame.PayloadLen);
  if AFrame.PayloadLen > 0 then
  begin
    Move(FBuffer[LHeaderLen], AFrame.Payload[0], AFrame.PayloadLen);
    
    // Desmascarar o payload usando XOR se mascarado (obrigatório do cliente)
    if AFrame.Mask then
    begin
      for i := 0 to AFrame.PayloadLen - 1 do
        AFrame.Payload[i] := AFrame.Payload[i] xor AFrame.MaskKey[i mod 4];
    end;
  end;
  
  AConsumedBytes := LHeaderLen + AFrame.PayloadLen;
  Result := True;
end;

procedure THorseWebSocketParser.FeedBytes(const ABytes: TBytes; ALength: Integer; const AConnection: IHorseWebSocketConnection);
var
  LFrame: TWebSocketFrame;
  LConsumed: Integer;
  LOldLen: Integer;
  LMsgText: string;
  LAccumLen: Integer;
  LStatus: Word;
begin
  if ALength <= 0 then
    Exit;
    
  LOldLen := FBufferLen;
  Inc(FBufferLen, ALength);
  SetLength(FBuffer, FBufferLen);
  Move(ABytes[0], FBuffer[LOldLen], ALength);
  
  try
    while ParseFrame(LFrame, LConsumed) do
    begin
      CompactBuffer(LConsumed);
      
      // Lógica de manipulação de Opcode
      case LFrame.Opcode of
        $0: // Continuation frame
        begin
          if Length(FAccumulatedPayload) = 0 then
            raise EHorseWebSocketProtocolError.Create('Continuation frame without preceding start frame.');
            
          LAccumLen := Length(FAccumulatedPayload);
          SetLength(FAccumulatedPayload, LAccumLen + LFrame.PayloadLen);
          if LFrame.PayloadLen > 0 then
            Move(LFrame.Payload[0], FAccumulatedPayload[LAccumLen], LFrame.PayloadLen);
            
          if LFrame.Fin then
          begin
            if FAccumulatedOpcode = $1 then
            begin
              LMsgText := TEncoding.UTF8.GetString(FAccumulatedPayload);
              if Assigned(THorseWebSocketConnection(AConnection).FOnMessage) then
                THorseWebSocketConnection(AConnection).FOnMessage(AConnection, LMsgText);
            end
            else if FAccumulatedOpcode = $2 then
            begin
              if Assigned(THorseWebSocketConnection(AConnection).FOnBinary) then
                THorseWebSocketConnection(AConnection).FOnBinary(AConnection, FAccumulatedPayload);
            end;
            SetLength(FAccumulatedPayload, 0);
            FAccumulatedOpcode := 0;
          end;
        end;
        
        $1: // Text frame
        begin
          if not LFrame.Fin then
          begin
            FAccumulatedOpcode := $1;
            FAccumulatedPayload := LFrame.Payload;
          end
          else
          begin
            LMsgText := TEncoding.UTF8.GetString(LFrame.Payload);
            if Assigned(THorseWebSocketConnection(AConnection).FOnMessage) then
              THorseWebSocketConnection(AConnection).FOnMessage(AConnection, LMsgText);
          end;
        end;
        
        $2: // Binary frame
        begin
          if not LFrame.Fin then
          begin
            FAccumulatedOpcode := $2;
            FAccumulatedPayload := LFrame.Payload;
          end
          else
          begin
            if Assigned(THorseWebSocketConnection(AConnection).FOnBinary) then
              THorseWebSocketConnection(AConnection).FOnBinary(AConnection, LFrame.Payload);
          end;
        end;
        
        $8: // Close connection control frame
        begin
          LStatus := 1000;
          if LFrame.PayloadLen >= 2 then
            LStatus := (LFrame.Payload[0] shl 8) or LFrame.Payload[1];
          AConnection.Close(LStatus, '');
        end;
        
        $9: // Ping frame
        begin
          // Envia Pong com o mesmo payload recebido
          THorseWebSocketConnection(AConnection).SendRawFrame(wsmtPong, LFrame.Payload);
        end;
        
        $A: // Pong frame
        begin
          // Pong recebido
        end;
      else
        raise EHorseWebSocketProtocolError.Create('Unsupported Opcode: ' + IntToStr(LFrame.Opcode));
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(THorseWebSocketConnection(AConnection).FOnError) then
        THorseWebSocketConnection(AConnection).FOnError(AConnection, E);
      AConnection.Close(1002, 'Protocol Error');
    end;
  end;
end;

class function THorseWebSocketParser.BuildFrame(const AType: TWebSocketMessageType; const AData: TBytes): TBytes;
var
  LOpcode: Byte;
  LPayloadLen: Int64;
  LHeaderLen: Integer;
  LFrame: TBytes;
  i: Integer;
begin
  case AType of
    wsmtText: LOpcode := $1;
    wsmtBinary: LOpcode := $2;
    wsmtClose: LOpcode := $8;
    wsmtPing: LOpcode := $9;
    wsmtPong: LOpcode := $A;
  else
    LOpcode := $1;
  end;
  
  LPayloadLen := Length(AData);
  if LPayloadLen <= 125 then
    LHeaderLen := 2
  else if LPayloadLen <= 65535 then
    LHeaderLen := 4
  else
    LHeaderLen := 10;
    
  SetLength(LFrame, LHeaderLen + LPayloadLen);
  LFrame[0] := $80 or LOpcode; // FIN = 1
  
  if LPayloadLen <= 125 then
    LFrame[1] := LPayloadLen
  else if LPayloadLen <= 65535 then
  begin
    LFrame[1] := 126;
    LFrame[2] := (LPayloadLen shr 8) and $FF;
    LFrame[3] := LPayloadLen and $FF;
  end
  else
  begin
    LFrame[1] := 127;
    for i := 0 to 7 do
      LFrame[2 + i] := (LPayloadLen shr (8 * (7 - i))) and $FF;
  end;
  
  if LPayloadLen > 0 then
    Move(AData[0], LFrame[LHeaderLen], LPayloadLen);
    
  Result := LFrame;
end;

{ THorseWebSocketHeartbeatThread }

type
  THorseWebSocketHeartbeatThread = class(TThread)
  private
    FConnection: THorseWebSocketConnection;
    FIntervalSeconds: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AConnection: THorseWebSocketConnection; AInterval: Integer);
  end;

constructor THorseWebSocketHeartbeatThread.Create(AConnection: THorseWebSocketConnection; AInterval: Integer);
begin
  inherited Create(True);
  FConnection := AConnection;
  FIntervalSeconds := AInterval;
  FreeOnTerminate := True;
end;

procedure THorseWebSocketHeartbeatThread.Execute;
var
  I: Integer;
begin
  while not Terminated and FConnection.IsConnected do
  begin
    // Dorme em intervalos de 1 segundo para responder rápido à terminação
    for I := 0 to FIntervalSeconds - 1 do
    begin
      if Terminated then Exit;
      Sleep(1000);
    end;
    if not Terminated and FConnection.IsConnected then
    begin
      try
        FConnection.Ping(nil);
      except
        FConnection.TriggerDisconnect;
        Exit;
      end;
    end;
  end;
end;

{ THorseWebSocketConnection }

constructor THorseWebSocketConnection.Create(const ATransport: IHorseWebSocketTransport; const APath: string; const AHeartbeatInterval: Integer);
begin
  inherited Create;
  FTransport := ATransport;
  FPath := APath;
  FParser := THorseWebSocketParser.Create;
  FSyncWrite := TCriticalSection.Create;
  FIsConnected := True;
  FHeartbeatInterval := AHeartbeatInterval;
  THorseWebSocketManager.RegisterConnection(FPath, Self);
  StartHeartbeat;
end;

destructor THorseWebSocketConnection.Destroy;
begin
  StopHeartbeat;
  THorseWebSocketManager.UnregisterConnection(FPath, Self);
  FParser.Free;
  FSyncWrite.Free;
  FTransport := nil;
  inherited Destroy;
end;

procedure THorseWebSocketConnection.SendRawFrame(const AType: TWebSocketMessageType; const AData: TBytes);
var
  LFrame: TBytes;
begin
  if not FIsConnected then
    Exit;
    
  FSyncWrite.Enter;
  try
    LFrame := THorseWebSocketParser.BuildFrame(AType, AData);
    if Length(LFrame) > 0 then
      FTransport.Write(LFrame, Length(LFrame));
  except
    on E: Exception do
    begin
      FSyncWrite.Leave;
      TriggerDisconnect;
      raise;
    end;
  end;
  FSyncWrite.Leave;
end;

procedure THorseWebSocketConnection.SendText(const AText: string);
begin
  SendRawFrame(wsmtText, TEncoding.UTF8.GetBytes(AText));
end;

procedure THorseWebSocketConnection.SendBinary(const AData: TBytes);
begin
  SendRawFrame(wsmtBinary, AData);
end;

procedure THorseWebSocketConnection.Ping(const AData: TBytes);
begin
  SendRawFrame(wsmtPing, AData);
end;

procedure THorseWebSocketConnection.Close(const AStatus: Word; const AReason: string);
var
  LPayload: TBytes;
  LReasonBytes: TBytes;
begin
  if not FIsConnected then
    Exit;
    
  SetLength(LPayload, 2);
  LPayload[0] := (AStatus shr 8) and $FF;
  LPayload[1] := AStatus and $FF;
  
  if AReason <> '' then
  begin
    LReasonBytes := TEncoding.UTF8.GetBytes(AReason);
    SetLength(LPayload, 2 + Length(LReasonBytes));
    Move(LReasonBytes[0], LPayload[2], Length(LReasonBytes));
  end;
  
  try
    SendRawFrame(wsmtClose, LPayload);
  except
  end;
  
  TriggerDisconnect;
end;

function THorseWebSocketConnection.IsConnected: Boolean;
begin
  Result := FIsConnected and FTransport.IsConnected;
end;

function THorseWebSocketConnection.GetClientIP: string;
begin
  Result := FTransport.GetClientIP;
end;

function THorseWebSocketConnection.GetServerPort: Integer;
begin
  Result := FTransport.GetServerPort;
end;

function THorseWebSocketConnection.GetPath: string;
begin
  Result := FPath;
end;

procedure THorseWebSocketConnection.SetOnMessage(const ACallback: TOnWebSocketMessage);
begin
  FOnMessage := ACallback;
end;

procedure THorseWebSocketConnection.SetOnBinary(const ACallback: TOnWebSocketBinary);
begin
  FOnBinary := ACallback;
end;

procedure THorseWebSocketConnection.SetOnDisconnect(const ACallback: TOnWebSocketDisconnect);
begin
  FOnDisconnect := ACallback;
end;

procedure THorseWebSocketConnection.SetOnError(const ACallback: TOnWebSocketError);
begin
  FOnError := ACallback;
end;

procedure THorseWebSocketConnection.TriggerError(const AException: Exception);
begin
  if Assigned(FOnError) then
    FOnError(Self, AException);
end;

procedure THorseWebSocketConnection.HandleIncomingBytes(const ABytes: TBytes; const ALength: Integer);
begin
  if FIsConnected then
    FParser.FeedBytes(ABytes, ALength, Self);
end;

procedure THorseWebSocketConnection.TriggerDisconnect;
begin
  if FIsConnected then
  begin
    FIsConnected := False;
    StopHeartbeat;
    FTransport.Close;
    if Assigned(FOnDisconnect) then
    begin
      try
        FOnDisconnect(Self);
      except
      end;
    end;
  end;
end;

procedure THorseWebSocketConnection.StartHeartbeat;
begin
  if (FHeartbeatInterval > 0) and (FHeartbeatThread = nil) then
  begin
    FHeartbeatThread := THorseWebSocketHeartbeatThread.Create(Self, FHeartbeatInterval);
    FHeartbeatThread.Start;
  end;
end;

procedure THorseWebSocketConnection.StopHeartbeat;
begin
  if FHeartbeatThread <> nil then
  begin
    FHeartbeatThread.Terminate;
    FHeartbeatThread := nil;
  end;
end;

end.
