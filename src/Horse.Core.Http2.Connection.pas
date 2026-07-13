unit Horse.Core.Http2.Connection;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}
{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Classes,
  Horse.Core.Http2.Hpack,
  Horse.Core.Http2.Framing,
  Horse.Core.Http2.Stream;

type
  THorseHttp2ConnectionState = (
    csAwaitingPreface,
    csSettingsExchange,
    csOpen,
    csClosing,
    csClosed
  );

  THorseOutputProc = procedure(AData: PByte; ALen: Integer) of object;

  THorseHttp2ConnectionOptions = record
    MaxConcurrentStreams: Cardinal;
    InitialWindowSize: Integer;
    MaxFrameSize: Cardinal;
    MaxHeaderListSize: Cardinal;
    HeaderTableSize: Cardinal;
    class function Default: THorseHttp2ConnectionOptions; static;
  end;

  THorseHttp2RequestHandler = procedure(
    AConnection: TObject;
    AStreamId: Cardinal;
    const AHeaders: TNameValuePairs;
    const ABody: TBytes
  ) of object;

  THorseHttp2CloseHandler = procedure(AConnection: TObject) of object;

  THorseHttp2Connection = class
  private
    FState: THorseHttp2ConnectionState;
    FOptions: THorseHttp2ConnectionOptions;
    FDecoder: THpackDecoder;
    FEncoder: THpackEncoder;
    FStreams: THorseHttp2StreamMap;
    FLastStreamId: Cardinal;
    FRecvBuffer: TBytes;
    FRecvLen: Integer;
    FConnRecvWindow: Integer;
    FConnSendWindow: Integer;
    FPeerSettingsSynced: Boolean;
    FLocalSettingsSent: Boolean;
    FOutputBuffer: TBytes;
    FOutputLen: Integer;
    FContinuationStreamId: Cardinal;
    FOnRequest: THorseHttp2RequestHandler;
    FOnClose: THorseHttp2CloseHandler;
    FOnOutput: THorseOutputProc;

    procedure HandleData(const AFrame: THorseHttp2Frame);
    procedure HandleHeaders(const AFrame: THorseHttp2Frame);
    procedure HandlePriority(const AFrame: THorseHttp2Frame);
    procedure HandleRstStream(const AFrame: THorseHttp2Frame);
    procedure HandleSettings(const AFrame: THorseHttp2Frame);
    procedure HandlePing(const AFrame: THorseHttp2Frame);
    procedure HandleGoaway(const AFrame: THorseHttp2Frame);
    procedure HandleWindowUpdate(const AFrame: THorseHttp2Frame);
    procedure HandleContinuation(const AFrame: THorseHttp2Frame);

    procedure SendProtocolError(const AMessage: string);
    procedure FlushOutput;
    procedure DispatchRequest(AStream: THorseHttp2Stream);
    procedure EnsureStream(AStreamId: Cardinal; out AStream: THorseHttp2Stream);
    function ValidatePreface: Boolean;
    procedure SendInitialSettings;
  public
    constructor Create(const AOptions: THorseHttp2ConnectionOptions);
    destructor Destroy; override;
    procedure Feed(AData: PByte; ALen: Integer); overload;
    procedure Feed(const ABuffer: TBytes; AOffset, ALen: Integer); overload;
    procedure SendResponse(AStreamId: Cardinal;
      const AHeaders: TNameValuePairs;
      const ABody: TBytes;
      AEndStream: Boolean = True);
    procedure SendGoaway(AErrorCode: Cardinal = HTTP2_ERR_NO_ERROR;
      const ADebugMessage: string = '');
    procedure SendWindowUpdate(AStreamId: Cardinal; AIncrement: Cardinal);

    property State: THorseHttp2ConnectionState read FState;
    property LastStreamId: Cardinal read FLastStreamId;
    property OnRequest: THorseHttp2RequestHandler read FOnRequest write FOnRequest;
    property OnClose: THorseHttp2CloseHandler read FOnClose write FOnClose;
    property OnOutput: THorseOutputProc read FOnOutput write FOnOutput;
    function PendingOutputLen: Integer;
    function ReadOutput(ABuffer: PByte; AMaxLen: Integer): Integer;
  end;

implementation

const
  HTTP2_CLIENT_PREFACE_BYTES: array[0..23] of Byte = (
    $50, $52, $49, $20, $2a, $20, $48, $54, $54, $50, $2f, $32,
    $2e, $30, $0d, $0a, $0d, $0a, $53, $4d, $0d, $0a, $0d, $0a
  );

{ THorseHttp2ConnectionOptions }

class function THorseHttp2ConnectionOptions.Default: THorseHttp2ConnectionOptions;
begin
  Result.MaxConcurrentStreams := 100;
  Result.InitialWindowSize    := 65535;
  Result.MaxFrameSize         := HTTP2_DEFAULT_MAX_FRAME_SIZE;
  Result.MaxHeaderListSize    := 65536;
  Result.HeaderTableSize      := 4096;
end;

{ THorseHttp2Connection }

constructor THorseHttp2Connection.Create(const AOptions: THorseHttp2ConnectionOptions);
begin
  inherited Create;
  FOptions  := AOptions;
  FState    := THorseHttp2ConnectionState.csAwaitingPreface;
  FDecoder  := THpackDecoder.Create(AOptions.HeaderTableSize);
  FEncoder  := THpackEncoder.Create(AOptions.HeaderTableSize);
  FStreams  := THorseHttp2StreamMap.Create(AOptions.InitialWindowSize);

  FRecvLen              := 0;
  FOutputLen            := 0;
  FConnRecvWindow       := 65535;
  FConnSendWindow       := 65535;
  FLastStreamId         := 0;
  FPeerSettingsSynced   := False;
  FLocalSettingsSent    := False;
  FContinuationStreamId := 0;

  SetLength(FRecvBuffer, 65536);
  SetLength(FOutputBuffer, 4096);
end;

destructor THorseHttp2Connection.Destroy;
begin
  FDecoder.Free;
  FEncoder.Free;
  FStreams.Free;
  inherited;
end;

procedure THorseHttp2Connection.FlushOutput;
begin
  if (FOutputLen > 0) and Assigned(FOnOutput) then
  begin
    FOnOutput(@FOutputBuffer[0], FOutputLen);
    FOutputLen := 0;
  end;
end;

function THorseHttp2Connection.PendingOutputLen: Integer;
begin
  Result := FOutputLen;
end;

function THorseHttp2Connection.ReadOutput(ABuffer: PByte; AMaxLen: Integer): Integer;
begin
  Result := FOutputLen;
  if Result > AMaxLen then Result := AMaxLen;
  if Result > 0 then
  begin
    Move(FOutputBuffer[0], ABuffer^, Result);
    if Result < FOutputLen then
      Move(FOutputBuffer[Result], FOutputBuffer[0], FOutputLen - Result);
    Dec(FOutputLen, Result);
  end;
end;

function THorseHttp2Connection.ValidatePreface: Boolean;
const
  PREFACE_LEN = 24;
begin
  Result := False;
  if FRecvLen < PREFACE_LEN then Exit;
  Result := CompareMem(@FRecvBuffer[0], @HTTP2_CLIENT_PREFACE_BYTES[0], PREFACE_LEN);
end;

procedure THorseHttp2Connection.SendInitialSettings;
var
  settings: THorseHttp2Settings;
  pos: Integer;
begin
  settings := DefaultServerSettings;
  pos := FOutputLen;
  THorseHttp2FrameCodec.WriteSettingsFrame(settings, False, FOutputBuffer, pos);
  FOutputLen := pos;
  FLocalSettingsSent := True;
end;

procedure THorseHttp2Connection.Feed(AData: PByte; ALen: Integer);
const
  PREFACE_LEN = 24;
var
  frame: THorseHttp2Frame;
  consumed: Integer;
  ptr: PByte;
  avail: Integer;
begin
  if not Assigned(Self) then
  begin
    WriteLn('Feed called on NIL THorseHttp2Connection instance!');
    Flush(Output);
    Exit;
  end;
  if FState = THorseHttp2ConnectionState.csClosed then Exit;

  if FRecvLen + ALen > Length(FRecvBuffer) then
    SetLength(FRecvBuffer, FRecvLen + ALen + 4096);
  Move(AData^, FRecvBuffer[FRecvLen], ALen);
  Inc(FRecvLen, ALen);

  if FState = THorseHttp2ConnectionState.csAwaitingPreface then
  begin
    if FRecvLen < PREFACE_LEN then Exit;
    if not ValidatePreface then
    begin
      SendProtocolError('Invalid client preface');
      Exit;
    end;
    Move(FRecvBuffer[PREFACE_LEN], FRecvBuffer[0], FRecvLen - PREFACE_LEN);
    Dec(FRecvLen, PREFACE_LEN);

    FState := THorseHttp2ConnectionState.csSettingsExchange;
    SendInitialSettings;
    FlushOutput;
  end;

  ptr   := @FRecvBuffer[0];
  avail := FRecvLen;

  while avail > 0 do
  begin
    if not THorseHttp2FrameCodec.TryReadFrame(ptr, avail, FOptions.MaxFrameSize,
      frame, consumed) then
      Break;

    case THorseHttp2FrameType(frame.FrameType) of
      THorseHttp2FrameType.ftData:         HandleData(frame);
      THorseHttp2FrameType.ftHeaders:      HandleHeaders(frame);
      THorseHttp2FrameType.ftPriority:     HandlePriority(frame);
      THorseHttp2FrameType.ftRstStream:    HandleRstStream(frame);
      THorseHttp2FrameType.ftSettings:     HandleSettings(frame);
      THorseHttp2FrameType.ftPing:         HandlePing(frame);
      THorseHttp2FrameType.ftGoaway:       HandleGoaway(frame);
      THorseHttp2FrameType.ftWindowUpdate: HandleWindowUpdate(frame);
      THorseHttp2FrameType.ftContinuation: HandleContinuation(frame);
    end;

    Inc(ptr, consumed);
    Dec(avail, consumed);

    if FState = THorseHttp2ConnectionState.csClosed then Break;
  end;

  FRecvLen := avail;
  if (avail > 0) and (ptr <> @FRecvBuffer[0]) then
    Move(ptr^, FRecvBuffer[0], avail);

  FlushOutput;
end;

procedure THorseHttp2Connection.Feed(const ABuffer: TBytes; AOffset, ALen: Integer);
begin
  if ALen > 0 then
    Feed(@ABuffer[AOffset], ALen);
end;

procedure THorseHttp2Connection.EnsureStream(AStreamId: Cardinal; out AStream: THorseHttp2Stream);
begin
  AStream := FStreams.Find(AStreamId);
  if AStream = nil then
    AStream := FStreams.OpenStream(AStreamId);
end;

procedure THorseHttp2Connection.DispatchRequest(AStream: THorseHttp2Stream);
var
  body: TBytes;
begin
  if Assigned(FOnRequest) then
  begin
    SetLength(body, AStream.DataLen);
    if AStream.DataLen > 0 then
      Move(AStream.DataBuffer[0], body[0], AStream.DataLen);
    FOnRequest(Self, AStream.StreamId, AStream.Headers, body);
  end;
end;

procedure THorseHttp2Connection.HandleData(const AFrame: THorseHttp2Frame);
var
  Stream: THorseHttp2Stream;
  DataPtr: PByte;
  DataLen: Integer;
  Increment: Integer;
  Pos: Integer;
begin
  if AFrame.StreamId = 0 then
  begin
    SendProtocolError('DATA on Stream 0');
    Exit;
  end;

  Stream := FStreams.Find(AFrame.StreamId);
  if Stream = nil then
  begin
    Pos := FOutputLen;
    THorseHttp2FrameCodec.WriteRstStream(AFrame.StreamId, HTTP2_ERR_STREAM_CLOSED, FOutputBuffer, Pos);
    FOutputLen := Pos;
    Exit;
  end;

  if not THorseHttp2FrameCodec.GetDataPayload(AFrame, DataPtr, DataLen) then
  begin
    SendProtocolError('Invalid DATA frame');
    Exit;
  end;

  Dec(FConnRecvWindow, DataLen);
  Stream.ConsumeRecvWindow(DataLen);

  Stream.AppendData(DataPtr, DataLen);

  if THorseHttp2FrameCodec.HasEndStream(AFrame) then
  begin
    Stream.RemoteEndStream;
    if Stream.HeadersComplete then
      DispatchRequest(Stream);
  end;

  if FConnRecvWindow < 32768 then
  begin
    Increment := 65535 - FConnRecvWindow;
    SendWindowUpdate(0, Increment);
    Inc(FConnRecvWindow, Increment);
  end;
end;

procedure THorseHttp2Connection.HandleHeaders(const AFrame: THorseHttp2Frame);
var
  stream: THorseHttp2Stream;
  fragPtr: PByte;
  fragLen: Integer;
begin
  if AFrame.StreamId = 0 then
  begin
    SendProtocolError('HEADERS on stream 0');
    Exit;
  end;

  EnsureStream(AFrame.StreamId, stream);
  if stream.State = THorseHttp2StreamState.ssIdle then
    stream.Open;

  if not THorseHttp2FrameCodec.GetHeaderBlockFragment(AFrame, fragPtr, fragLen) then
  begin
    SendProtocolError('Invalid HEADERS frame');
    Exit;
  end;

  stream.AppendHeaderFragment(fragPtr, fragLen);

  if AFrame.StreamId > FLastStreamId then
    FLastStreamId := AFrame.StreamId;

  if THorseHttp2FrameCodec.HasEndHeaders(AFrame) then
  begin
    if not stream.FinalizeHeaders(FDecoder) then
    begin
      SendProtocolError('HPACK decode error');
      Exit;
    end;
    FContinuationStreamId := 0;

    if THorseHttp2FrameCodec.HasEndStream(AFrame) then
    begin
      stream.RemoteEndStream;
      DispatchRequest(stream);
    end;
  end
  else
  begin
    FContinuationStreamId := AFrame.StreamId;
  end;
end;

procedure THorseHttp2Connection.HandlePriority(const AFrame: THorseHttp2Frame);
begin
end;

procedure THorseHttp2Connection.HandleRstStream(const AFrame: THorseHttp2Frame);
var
  stream: THorseHttp2Stream;
  errCode: Cardinal;
begin
  if AFrame.StreamId = 0 then
  begin
    SendProtocolError('RST_STREAM on stream 0');
    Exit;
  end;
  stream := FStreams.Find(AFrame.StreamId);
  if stream = nil then Exit;
  THorseHttp2FrameCodec.GetRstStreamError(AFrame, errCode);
  stream.Reset(errCode);
end;

procedure THorseHttp2Connection.HandleSettings(const AFrame: THorseHttp2Frame);
var
  settings: THorseHttp2Settings;
  i: Integer;
  oldWindowSize: Integer;
  delta: Integer;
  pos: Integer;
begin
  if THorseHttp2FrameCodec.HasAck(AFrame) then
  begin
    if FState = THorseHttp2ConnectionState.csSettingsExchange then
    begin
      FState := THorseHttp2ConnectionState.csOpen;
      FPeerSettingsSynced := True;
    end;
    Exit;
  end;

  if not THorseHttp2FrameCodec.GetSettings(AFrame, settings) then
  begin
    SendProtocolError('Invalid SETTINGS frame');
    Exit;
  end;

  oldWindowSize := FOptions.InitialWindowSize;

  for i := 0 to High(settings) do
  begin
    case settings[i].Id of
      HTTP2_SETTINGS_HEADER_TABLE_SIZE:
        begin
          FOptions.HeaderTableSize := settings[i].Value;
          FDecoder.SetMaxTableSize(FOptions.HeaderTableSize);
        end;
      HTTP2_SETTINGS_ENABLE_PUSH:
        ;
      HTTP2_SETTINGS_MAX_CONCURRENT_STREAMS:
        FOptions.MaxConcurrentStreams := settings[i].Value;
      HTTP2_SETTINGS_INITIAL_WINDOW_SIZE:
        begin
          FOptions.InitialWindowSize := Integer(settings[i].Value);
          FStreams.SetInitialWindowSize(FOptions.InitialWindowSize);
          delta := FOptions.InitialWindowSize - oldWindowSize;
          if delta <> 0 then
            FStreams.ApplyWindowSizeDelta(delta);
        end;
      HTTP2_SETTINGS_MAX_FRAME_SIZE:
        begin
          if (settings[i].Value < 16384) or (settings[i].Value > $FFFFFF) then
          begin
            SendProtocolError('Invalid SETTINGS_MAX_FRAME_SIZE');
            Exit;
          end;
          FOptions.MaxFrameSize := settings[i].Value;
        end;
      HTTP2_SETTINGS_MAX_HEADER_LIST_SIZE:
        FOptions.MaxHeaderListSize := settings[i].Value;
    end;
  end;

  pos := FOutputLen;
  THorseHttp2FrameCodec.WriteSettingsAck(FOutputBuffer, pos);
  FOutputLen := pos;
end;

procedure THorseHttp2Connection.HandlePing(const AFrame: THorseHttp2Frame);
var
  pos: Integer;
begin
  if AFrame.StreamId <> 0 then
  begin
    SendProtocolError('PING on non-zero stream');
    Exit;
  end;
  if THorseHttp2FrameCodec.HasAck(AFrame) then
    Exit;
  pos := FOutputLen;
  THorseHttp2FrameCodec.WritePingAck(AFrame.PayloadPtr, FOutputBuffer, pos);
  FOutputLen := pos;
end;

procedure THorseHttp2Connection.HandleGoaway(const AFrame: THorseHttp2Frame);
var
  lastSid, errCode: Cardinal;
  debugData: TBytes;
begin
  THorseHttp2FrameCodec.GetGoaway(AFrame, lastSid, errCode, debugData);
  FState := THorseHttp2ConnectionState.csClosed;
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure THorseHttp2Connection.HandleWindowUpdate(const AFrame: THorseHttp2Frame);
var
  Increment: Cardinal;
  Stream: THorseHttp2Stream;
  Pos: Integer;
begin
  if not THorseHttp2FrameCodec.GetWindowUpdateIncrement(AFrame, Increment) then
  begin
    SendProtocolError('Invalid WINDOW_UPDATE');
    Exit;
  end;
  if Increment = 0 then
  begin
    if AFrame.StreamId = 0 then
      SendProtocolError('WINDOW_UPDATE Increment 0 on connection')
    else
    begin
      Pos := FOutputLen;
      THorseHttp2FrameCodec.WriteRstStream(AFrame.StreamId, HTTP2_ERR_PROTOCOL_ERROR, FOutputBuffer, Pos);
      FOutputLen := Pos;
    end;
    Exit;
  end;
  if AFrame.StreamId = 0 then
    Inc(FConnSendWindow, Increment)
  else
  begin
    Stream := FStreams.Find(AFrame.StreamId);
    if Stream <> nil then
      Stream.IncreaseSendWindow(Increment);
  end;
end;

procedure THorseHttp2Connection.HandleContinuation(const AFrame: THorseHttp2Frame);
var
  stream: THorseHttp2Stream;
  fragPtr: PByte;
  fragLen: Integer;
begin
  if AFrame.StreamId = 0 then
  begin
    SendProtocolError('CONTINUATION on stream 0');
    Exit;
  end;
  if AFrame.StreamId <> FContinuationStreamId then
  begin
    SendProtocolError('CONTINUATION on unexpected stream');
    Exit;
  end;

  stream := FStreams.Find(AFrame.StreamId);
  if stream = nil then Exit;

  fragPtr := AFrame.PayloadPtr;
  fragLen := Integer(AFrame.PayloadLength);
  stream.AppendHeaderFragment(fragPtr, fragLen);

  if THorseHttp2FrameCodec.HasEndHeaders(AFrame) then
  begin
    if not stream.FinalizeHeaders(FDecoder) then
    begin
      SendProtocolError('HPACK decode error in CONTINUATION');
      Exit;
    end;
    FContinuationStreamId := 0;
    if stream.EndStreamReceived then
      DispatchRequest(stream);
  end;
end;

procedure THorseHttp2Connection.SendProtocolError(const AMessage: string);
begin
  SendGoaway(HTTP2_ERR_PROTOCOL_ERROR, AMessage);
end;

procedure THorseHttp2Connection.SendResponse(AStreamId: Cardinal;
  const AHeaders: TNameValuePairs;
  const ABody: TBytes;
  AEndStream: Boolean);
var
  headerBlock: TBytes;
  pos: Integer;
  bodyEndStream: Boolean;
  offset: Integer;
  chunk: Integer;
  maxChunk: Integer;
  stream: THorseHttp2Stream;
begin
  headerBlock := FEncoder.Encode(AHeaders);
  pos := FOutputLen;

  bodyEndStream := AEndStream and (Length(ABody) = 0);

  THorseHttp2FrameCodec.WriteHeadersFrame(AStreamId,
    @headerBlock[0], Length(headerBlock),
    bodyEndStream,
    True,
    FOutputBuffer, pos);

  if Length(ABody) > 0 then
  begin
    maxChunk := Integer(FOptions.MaxFrameSize);
    offset := 0;
    while offset < Length(ABody) do
    begin
      chunk := Length(ABody) - offset;
      if chunk > maxChunk then chunk := maxChunk;
      bodyEndStream := AEndStream and (offset + chunk >= Length(ABody));
      THorseHttp2FrameCodec.WriteDataFrame(AStreamId,
        @ABody[offset], chunk,
        bodyEndStream,
        FOutputBuffer, pos);
      Inc(offset, chunk);
    end;
  end;

  FOutputLen := pos;

  stream := FStreams.Find(AStreamId);
  if stream <> nil then
  begin
    if AEndStream then
    begin
      stream.LocalEndStream;
      if stream.State = THorseHttp2StreamState.ssClosed then
        FStreams.Remove(AStreamId);
    end;
  end;

  FlushOutput;
end;

procedure THorseHttp2Connection.SendGoaway(AErrorCode: Cardinal;
  const ADebugMessage: string);
var
  pos: Integer;
begin
  if FState = THorseHttp2ConnectionState.csClosed then Exit;
  pos := FOutputLen;
  THorseHttp2FrameCodec.WriteGoaway(FLastStreamId, AErrorCode, ADebugMessage, FOutputBuffer, pos);
  FOutputLen := pos;
  FState := THorseHttp2ConnectionState.csClosing;
  FlushOutput;
end;

procedure THorseHttp2Connection.SendWindowUpdate(AStreamId: Cardinal; AIncrement: Cardinal);
var
  pos: Integer;
begin
  pos := FOutputLen;
  THorseHttp2FrameCodec.WriteWindowUpdate(AStreamId, AIncrement, FOutputBuffer, pos);
  FOutputLen := pos;
end;

end.
