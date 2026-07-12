unit Horse.Core.Http2.Framing;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}
{$SCOPEDENUMS ON}

interface

uses
  System.Classes,
  System.SysUtils;

const
  HTTP2_FRAME_HEADER_SIZE = 9;
  HTTP2_DEFAULT_MAX_FRAME_SIZE = 16384;
  HTTP2_CLIENT_PREFACE = 'PRI * HTTP/2.0'#13#10#13#10'SM'#13#10#13#10;

  // SETTINGS identifiers
  HTTP2_SETTINGS_HEADER_TABLE_SIZE      = $1;
  HTTP2_SETTINGS_ENABLE_PUSH            = $2;
  HTTP2_SETTINGS_MAX_CONCURRENT_STREAMS = $3;
  HTTP2_SETTINGS_INITIAL_WINDOW_SIZE    = $4;
  HTTP2_SETTINGS_MAX_FRAME_SIZE         = $5;
  HTTP2_SETTINGS_MAX_HEADER_LIST_SIZE   = $6;

  // HTTP/2 error codes
  HTTP2_ERR_NO_ERROR            = $0;
  HTTP2_ERR_PROTOCOL_ERROR      = $1;
  HTTP2_ERR_INTERNAL_ERROR      = $2;
  HTTP2_ERR_FLOW_CONTROL_ERROR  = $3;
  HTTP2_ERR_SETTINGS_TIMEOUT    = $4;
  HTTP2_ERR_STREAM_CLOSED       = $5;
  HTTP2_ERR_FRAME_SIZE_ERROR    = $6;
  HTTP2_ERR_REFUSED_STREAM      = $7;
  HTTP2_ERR_CANCEL              = $8;
  HTTP2_ERR_COMPRESSION_ERROR   = $9;
  HTTP2_ERR_CONNECT_ERROR       = $A;
  HTTP2_ERR_ENHANCE_YOUR_CALM   = $B;
  HTTP2_ERR_INADEQUATE_SECURITY = $C;
  HTTP2_ERR_HTTP_1_1_REQUIRED   = $D;

type
  THorseHttp2FrameType = (
    ftData         = $00,
    ftHeaders      = $01,
    ftPriority     = $02,
    ftRstStream    = $03,
    ftSettings     = $04,
    ftPushPromise  = $05,
    ftPing         = $06,
    ftGoaway       = $07,
    ftWindowUpdate = $08,
    ftContinuation = $09
  );

  THorseHttp2Frame = record
    PayloadLength: Cardinal;
    FrameType: Byte;
    Flags: Byte;
    StreamId: Cardinal;
    PayloadPtr: PByte;
  end;

  THorseHttp2Setting = record
    Id: Word;
    Value: Cardinal;
  end;

  THorseHttp2Settings = array of THorseHttp2Setting;

  THorseHttp2FrameCodec = class
  private
    class function Read3(P: PByte): Cardinal; static; inline;
    class function Read4(P: PByte): Cardinal; static; inline;
    class procedure AppendBytes(var AOutput: TBytes; var APos: Integer;
      AData: PByte; ALen: Integer); static;
    class procedure GrowIfNeeded(var AOutput: TBytes; APos, ANeeded: Integer); static; inline;
  public
    class function TryReadFrame(ABuffer: PByte; AAvail: Integer;
      AMaxFrameSize: Cardinal;
      out AFrame: THorseHttp2Frame; out ABytesConsumed: Integer): Boolean; static;

    class function HasEndStream(const AFrame: THorseHttp2Frame): Boolean; static; inline;
    class function HasEndHeaders(const AFrame: THorseHttp2Frame): Boolean; static; inline;
    class function HasAck(const AFrame: THorseHttp2Frame): Boolean; static; inline;
    class function HasPadded(const AFrame: THorseHttp2Frame): Boolean; static; inline;

    class function GetHeaderBlockFragment(const AFrame: THorseHttp2Frame;
      out AData: PByte; out ALen: Integer): Boolean; static;

    class function GetDataPayload(const AFrame: THorseHttp2Frame;
      out AData: PByte; out ALen: Integer): Boolean; static;

    class function GetRstStreamError(const AFrame: THorseHttp2Frame;
      out AErrorCode: Cardinal): Boolean; static;

    class function GetGoaway(const AFrame: THorseHttp2Frame;
      out ALastStreamId, AErrorCode: Cardinal;
      out ADebugData: TBytes): Boolean; static;

    class function GetWindowUpdateIncrement(const AFrame: THorseHttp2Frame;
      out AIncrement: Cardinal): Boolean; static;

    class function GetSettings(const AFrame: THorseHttp2Frame;
      out ASettings: THorseHttp2Settings): Boolean; static;

    class procedure WriteSettingsFrame(const ASettings: THorseHttp2Settings;
      AAck: Boolean; var AOutput: TBytes; var APos: Integer); static;

    class procedure WriteSettingsAck(var AOutput: TBytes; var APos: Integer); static;

    class procedure WritePingAck(APayload: PByte; var AOutput: TBytes; var APos: Integer); static;

    class procedure WriteGoaway(ALastStreamId, AErrorCode: Cardinal;
      const ADebugMessage: string; var AOutput: TBytes; var APos: Integer); static;

    class procedure WriteWindowUpdate(AStreamId: Cardinal; AIncrement: Cardinal;
      var AOutput: TBytes; var APos: Integer); static;

    class procedure WriteRstStream(AStreamId: Cardinal; AErrorCode: Cardinal;
      var AOutput: TBytes; var APos: Integer); static;

    class procedure WriteHeadersFrame(AStreamId: Cardinal;
      AHeaderBlock: PByte; AHeaderBlockLen: Integer;
      AEndStream, AEndHeaders: Boolean;
      var AOutput: TBytes; var APos: Integer); static;

    class procedure WriteContinuationFrame(AStreamId: Cardinal;
      AHeaderBlock: PByte; AHeaderBlockLen: Integer;
      AEndHeaders: Boolean;
      var AOutput: TBytes; var APos: Integer); static;

    class procedure WriteDataFrame(AStreamId: Cardinal;
      AData: PByte; ADataLen: Integer;
      AEndStream: Boolean;
      var AOutput: TBytes; var APos: Integer); static;
  end;

function DefaultServerSettings: THorseHttp2Settings;

const
  FLAG_END_STREAM  = $01;
  FLAG_END_HEADERS = $04;
  FLAG_PADDED      = $08;
  FLAG_PRIORITY    = $20;
  FLAG_ACK         = $01;

implementation

function DefaultServerSettings: THorseHttp2Settings;
begin
  SetLength(Result, 5);
  Result[0].Id := HTTP2_SETTINGS_HEADER_TABLE_SIZE;      Result[0].Value := 4096;
  Result[1].Id := HTTP2_SETTINGS_ENABLE_PUSH;            Result[1].Value := 0;
  Result[2].Id := HTTP2_SETTINGS_MAX_CONCURRENT_STREAMS; Result[2].Value := 100;
  Result[3].Id := HTTP2_SETTINGS_INITIAL_WINDOW_SIZE;    Result[3].Value := 65535;
  Result[4].Id := HTTP2_SETTINGS_MAX_FRAME_SIZE;         Result[4].Value := HTTP2_DEFAULT_MAX_FRAME_SIZE;
end;

{ THorseHttp2FrameCodec }

class function THorseHttp2FrameCodec.Read3(P: PByte): Cardinal;
begin
  Result := (Cardinal(P[0]) shl 16) or (Cardinal(P[1]) shl 8) or P[2];
end;

class function THorseHttp2FrameCodec.Read4(P: PByte): Cardinal;
begin
  Result := (Cardinal(P[0]) shl 24) or (Cardinal(P[1]) shl 16) or
            (Cardinal(P[2]) shl 8)  or P[3];
end;

class procedure THorseHttp2FrameCodec.GrowIfNeeded(var AOutput: TBytes; APos, ANeeded: Integer);
begin
  if APos + ANeeded > Length(AOutput) then
    SetLength(AOutput, APos + ANeeded + 256);
end;

class procedure THorseHttp2FrameCodec.AppendBytes(var AOutput: TBytes; var APos: Integer;
  AData: PByte; ALen: Integer);
begin
  GrowIfNeeded(AOutput, APos, ALen);
  if ALen > 0 then
    Move(AData^, AOutput[APos], ALen);
  Inc(APos, ALen);
end;

class function THorseHttp2FrameCodec.TryReadFrame(ABuffer: PByte; AAvail: Integer;
  AMaxFrameSize: Cardinal;
  out AFrame: THorseHttp2Frame; out ABytesConsumed: Integer): Boolean;
begin
  Result := False;
  ABytesConsumed := 0;

  if AAvail < HTTP2_FRAME_HEADER_SIZE then Exit;

  AFrame.PayloadLength := Read3(ABuffer);
  AFrame.FrameType     := ABuffer[3];
  AFrame.Flags         := ABuffer[4];
  AFrame.StreamId      := Read4(ABuffer + 5) and $7FFFFFFF;

  if AFrame.PayloadLength > AMaxFrameSize then
    raise EInvalidOperation.CreateFmt(
      'HTTP/2 FRAME_SIZE_ERROR: frame payload %d exceeds max %d',
      [AFrame.PayloadLength, AMaxFrameSize]);

  if AAvail < HTTP2_FRAME_HEADER_SIZE + Integer(AFrame.PayloadLength) then Exit;

  AFrame.PayloadPtr := ABuffer + HTTP2_FRAME_HEADER_SIZE;
  ABytesConsumed    := HTTP2_FRAME_HEADER_SIZE + Integer(AFrame.PayloadLength);
  Result := True;
end;

class function THorseHttp2FrameCodec.HasEndStream(const AFrame: THorseHttp2Frame): Boolean;
begin
  Result := (AFrame.Flags and FLAG_END_STREAM) <> 0;
end;

class function THorseHttp2FrameCodec.HasEndHeaders(const AFrame: THorseHttp2Frame): Boolean;
begin
  Result := (AFrame.Flags and FLAG_END_HEADERS) <> 0;
end;

class function THorseHttp2FrameCodec.HasAck(const AFrame: THorseHttp2Frame): Boolean;
begin
  Result := (AFrame.Flags and FLAG_ACK) <> 0;
end;

class function THorseHttp2FrameCodec.HasPadded(const AFrame: THorseHttp2Frame): Boolean;
begin
  Result := (AFrame.Flags and FLAG_PADDED) <> 0;
end;

class function THorseHttp2FrameCodec.GetHeaderBlockFragment(const AFrame: THorseHttp2Frame;
  out AData: PByte; out ALen: Integer): Boolean;
var
  payload: PByte;
  remaining: Integer;
  padLen: Byte;
begin
  Result := False;
  AData := nil;
  ALen := 0;
  payload := AFrame.PayloadPtr;
  remaining := Integer(AFrame.PayloadLength);

  padLen := 0;
  if HasPadded(AFrame) then
  begin
    if remaining < 1 then Exit;
    padLen := payload[0];
    Inc(payload);
    Dec(remaining);
  end;

  if (AFrame.Flags and FLAG_PRIORITY) <> 0 then
  begin
    if remaining < 5 then Exit;
    Inc(payload, 5);
    Dec(remaining, 5);
  end;

  if remaining < Integer(padLen) then Exit;
  AData := payload;
  ALen := remaining - Integer(padLen);
  Result := True;
end;

class function THorseHttp2FrameCodec.GetDataPayload(const AFrame: THorseHttp2Frame;
  out AData: PByte; out ALen: Integer): Boolean;
var
  payload: PByte;
  remaining: Integer;
  padLen: Byte;
begin
  Result := False;
  AData := nil;
  ALen := 0;
  payload := AFrame.PayloadPtr;
  remaining := Integer(AFrame.PayloadLength);

  padLen := 0;
  if HasPadded(AFrame) then
  begin
    if remaining < 1 then Exit;
    padLen := payload[0];
    Inc(payload);
    Dec(remaining);
  end;

  if remaining < Integer(padLen) then Exit;
  AData := payload;
  ALen := remaining - Integer(padLen);
  Result := True;
end;

class function THorseHttp2FrameCodec.GetRstStreamError(const AFrame: THorseHttp2Frame;
  out AErrorCode: Cardinal): Boolean;
begin
  Result := AFrame.PayloadLength >= 4;
  if Result then
    AErrorCode := Read4(AFrame.PayloadPtr);
end;

class function THorseHttp2FrameCodec.GetGoaway(const AFrame: THorseHttp2Frame;
  out ALastStreamId, AErrorCode: Cardinal;
  out ADebugData: TBytes): Boolean;
var
  debugLen: Integer;
begin
  Result := False;
  if AFrame.PayloadLength < 8 then Exit;
  ALastStreamId := Read4(AFrame.PayloadPtr) and $7FFFFFFF;
  AErrorCode    := Read4(AFrame.PayloadPtr + 4);
  debugLen      := Integer(AFrame.PayloadLength) - 8;
  SetLength(ADebugData, debugLen);
  if debugLen > 0 then
    Move((AFrame.PayloadPtr + 8)^, ADebugData[0], debugLen);
  Result := True;
end;

class function THorseHttp2FrameCodec.GetWindowUpdateIncrement(const AFrame: THorseHttp2Frame;
  out AIncrement: Cardinal): Boolean;
begin
  Result := AFrame.PayloadLength >= 4;
  if Result then
    AIncrement := Read4(AFrame.PayloadPtr) and $7FFFFFFF;
end;

class function THorseHttp2FrameCodec.GetSettings(const AFrame: THorseHttp2Frame;
  out ASettings: THorseHttp2Settings): Boolean;
var
  count: Integer;
  i: Integer;
  ptr: PByte;
begin
  Result := False;
  if (AFrame.PayloadLength mod 6) <> 0 then Exit;
  count := Integer(AFrame.PayloadLength) div 6;
  SetLength(ASettings, count);
  ptr := AFrame.PayloadPtr;
  for i := 0 to count - 1 do
  begin
    ASettings[i].Id    := (Word(ptr[0]) shl 8) or ptr[1];
    ASettings[i].Value := Read4(ptr + 2);
    Inc(ptr, 6);
  end;
  Result := True;
end;

procedure WriteHeader(var AOutput: TBytes; var APos: Integer;
  APayloadLen: Cardinal; AFrameType: Byte; AFlags: Byte; AStreamId: Cardinal);
var
  p: PByte;
begin
  THorseHttp2FrameCodec.GrowIfNeeded(AOutput, APos, HTTP2_FRAME_HEADER_SIZE);
  p := @AOutput[APos];
  p[0] := Byte(APayloadLen shr 16);
  p[1] := Byte(APayloadLen shr 8);
  p[2] := Byte(APayloadLen);
  p[3] := AFrameType;
  p[4] := AFlags;
  p[5] := Byte((AStreamId shr 24) and $7F);
  p[6] := Byte(AStreamId shr 16);
  p[7] := Byte(AStreamId shr 8);
  p[8] := Byte(AStreamId);
  Inc(APos, HTTP2_FRAME_HEADER_SIZE);
end;

class procedure THorseHttp2FrameCodec.WriteSettingsFrame(const ASettings: THorseHttp2Settings;
  AAck: Boolean; var AOutput: TBytes; var APos: Integer);
var
  payloadLen: Cardinal;
  flags: Byte;
  i: Integer;
  p: PByte;
begin
  if AAck then
  begin
    payloadLen := 0;
    flags := FLAG_ACK;
  end
  else
  begin
    payloadLen := Cardinal(Length(ASettings)) * 6;
    flags := 0;
  end;

  WriteHeader(AOutput, APos, payloadLen, Byte(THorseHttp2FrameType.ftSettings), flags, 0);
  if not AAck then
  begin
    GrowIfNeeded(AOutput, APos, Integer(payloadLen));
    if payloadLen > 0 then
    begin
      p := @AOutput[APos];
      for i := 0 to High(ASettings) do
      begin
        p[0] := Byte(ASettings[i].Id shr 8);
        p[1] := Byte(ASettings[i].Id);
        p[2] := Byte(ASettings[i].Value shr 24);
        p[3] := Byte(ASettings[i].Value shr 16);
        p[4] := Byte(ASettings[i].Value shr 8);
        p[5] := Byte(ASettings[i].Value);
        Inc(p, 6);
      end;
    end;
    Inc(APos, Integer(payloadLen));
  end;
end;

class procedure THorseHttp2FrameCodec.WriteSettingsAck(var AOutput: TBytes; var APos: Integer);
begin
  WriteHeader(AOutput, APos, 0, Byte(THorseHttp2FrameType.ftSettings), FLAG_ACK, 0);
end;

class procedure THorseHttp2FrameCodec.WritePingAck(APayload: PByte;
  var AOutput: TBytes; var APos: Integer);
begin
  WriteHeader(AOutput, APos, 8, Byte(THorseHttp2FrameType.ftPing), FLAG_ACK, 0);
  GrowIfNeeded(AOutput, APos, 8);
  if APayload <> nil then
    Move(APayload^, AOutput[APos], 8)
  else
    FillChar(AOutput[APos], 8, 0);
  Inc(APos, 8);
end;

class procedure THorseHttp2FrameCodec.WriteGoaway(ALastStreamId, AErrorCode: Cardinal;
  const ADebugMessage: string; var AOutput: TBytes; var APos: Integer);
var
  debugBytes: TBytes;
  payloadLen: Cardinal;
  p: PByte;
begin
  debugBytes := TEncoding.UTF8.GetBytes(ADebugMessage);
  payloadLen := 8 + Cardinal(Length(debugBytes));
  WriteHeader(AOutput, APos, payloadLen, Byte(THorseHttp2FrameType.ftGoaway), 0, 0);
  GrowIfNeeded(AOutput, APos, Integer(payloadLen));
  p := @AOutput[APos];
  p[0] := Byte((ALastStreamId shr 24) and $7F);
  p[1] := Byte(ALastStreamId shr 16);
  p[2] := Byte(ALastStreamId shr 8);
  p[3] := Byte(ALastStreamId);
  p[4] := Byte(AErrorCode shr 24);
  p[5] := Byte(AErrorCode shr 16);
  p[6] := Byte(AErrorCode shr 8);
  p[7] := Byte(AErrorCode);
  Inc(APos, 8);
  if Length(debugBytes) > 0 then
  begin
    Move(debugBytes[0], AOutput[APos], Length(debugBytes));
    Inc(APos, Length(debugBytes));
  end;
end;

class procedure THorseHttp2FrameCodec.WriteWindowUpdate(AStreamId: Cardinal;
  AIncrement: Cardinal; var AOutput: TBytes; var APos: Integer);
var
  p: PByte;
begin
  WriteHeader(AOutput, APos, 4, Byte(THorseHttp2FrameType.ftWindowUpdate), 0, AStreamId);
  GrowIfNeeded(AOutput, APos, 4);
  p := @AOutput[APos];
  p[0] := Byte((AIncrement shr 24) and $7F);
  p[1] := Byte(AIncrement shr 16);
  p[2] := Byte(AIncrement shr 8);
  p[3] := Byte(AIncrement);
  Inc(APos, 4);
end;

class procedure THorseHttp2FrameCodec.WriteRstStream(AStreamId: Cardinal;
  AErrorCode: Cardinal; var AOutput: TBytes; var APos: Integer);
var
  p: PByte;
begin
  WriteHeader(AOutput, APos, 4, Byte(THorseHttp2FrameType.ftRstStream), 0, AStreamId);
  GrowIfNeeded(AOutput, APos, 4);
  p := @AOutput[APos];
  p[0] := Byte(AErrorCode shr 24);
  p[1] := Byte(AErrorCode shr 16);
  p[2] := Byte(AErrorCode shr 8);
  p[3] := Byte(AErrorCode);
  Inc(APos, 4);
end;

class procedure THorseHttp2FrameCodec.WriteHeadersFrame(AStreamId: Cardinal;
  AHeaderBlock: PByte; AHeaderBlockLen: Integer;
  AEndStream, AEndHeaders: Boolean;
  var AOutput: TBytes; var APos: Integer);
var
  flags: Byte;
begin
  flags := 0;
  if AEndStream  then flags := flags or FLAG_END_STREAM;
  if AEndHeaders then flags := flags or FLAG_END_HEADERS;
  WriteHeader(AOutput, APos, AHeaderBlockLen, Byte(THorseHttp2FrameType.ftHeaders), flags, AStreamId);
  AppendBytes(AOutput, APos, AHeaderBlock, AHeaderBlockLen);
end;

class procedure THorseHttp2FrameCodec.WriteContinuationFrame(AStreamId: Cardinal;
  AHeaderBlock: PByte; AHeaderBlockLen: Integer;
  AEndHeaders: Boolean;
  var AOutput: TBytes; var APos: Integer);
var
  flags: Byte;
begin
  flags := 0;
  if AEndHeaders then flags := FLAG_END_HEADERS;
  WriteHeader(AOutput, APos, AHeaderBlockLen, Byte(THorseHttp2FrameType.ftContinuation), flags, AStreamId);
  AppendBytes(AOutput, APos, AHeaderBlock, AHeaderBlockLen);
end;

class procedure THorseHttp2FrameCodec.WriteDataFrame(AStreamId: Cardinal;
  AData: PByte; ADataLen: Integer;
  AEndStream: Boolean;
  var AOutput: TBytes; var APos: Integer);
var
  flags: Byte;
begin
  flags := 0;
  if AEndStream then flags := FLAG_END_STREAM;
  WriteHeader(AOutput, APos, ADataLen, Byte(THorseHttp2FrameType.ftData), flags, AStreamId);
  AppendBytes(AOutput, APos, AData, ADataLen);
end;

end.
