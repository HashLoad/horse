unit Horse.Core.Http2.Stream;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}
{$SCOPEDENUMS ON}

interface

uses
  System.Classes,
  System.SysUtils,
  Horse.Core.Http2.Hpack,
  Horse.Core.Http2.Framing;

type
  THorseHttp2StreamState = (
    ssIdle,
    ssOpen,
    ssHalfClosedRemote,
    ssHalfClosedLocal,
    ssReservedRemote,
    ssReservedLocal,
    ssClosed
  );

  THorseHttp2Stream = class
  private
    FStreamId: Cardinal;
    FState: THorseHttp2StreamState;
    FRecvWindowSize: Integer;
    FSendWindowSize: Integer;
    FHeaders: TNameValuePairs;
    FHeadersComplete: Boolean;
    FHeaderBlockBuffer: TBytes;
    FHeaderBlockLen: Integer;
    FDataBuffer: TBytes;
    FDataLen: Integer;
    FEndStreamReceived: Boolean;
    FErrorCode: Cardinal;
  public
    constructor Create(AStreamId: Cardinal; AInitialWindowSize: Integer);
    procedure Open;
    procedure RemoteEndStream;
    procedure LocalEndStream;
    procedure Reset(AErrorCode: Cardinal = 0);
    procedure AppendHeaderFragment(AData: PByte; ALen: Integer);
    function FinalizeHeaders(ADecoder: THpackDecoder): Boolean;
    procedure AppendData(AData: PByte; ALen: Integer);
    function ConsumeSendWindow(ABytes: Integer): Boolean;
    procedure IncreaseSendWindow(AIncrement: Integer);
    procedure ConsumeRecvWindow(ABytes: Integer);
    procedure RefillRecvWindow(ABytes: Integer);

    property StreamId: Cardinal read FStreamId;
    property State: THorseHttp2StreamState read FState;
    property Headers: TNameValuePairs read FHeaders;
    property HeadersComplete: Boolean read FHeadersComplete;
    property DataBuffer: TBytes read FDataBuffer;
    property DataLen: Integer read FDataLen;
    property EndStreamReceived: Boolean read FEndStreamReceived;
    property RecvWindowSize: Integer read FRecvWindowSize;
    property SendWindowSize: Integer read FSendWindowSize;
    property ErrorCode: Cardinal read FErrorCode;
  end;

  THorseHttp2StreamMap = class
  private
    FIds: array of Cardinal;
    FStreams: array of THorseHttp2Stream;
    FCount: Integer;
    FInitialWindowSize: Integer;
    function BinarySearch(AId: Cardinal): Integer;
  public
    constructor Create(AInitialWindowSize: Integer = 65535);
    destructor Destroy; override;
    function OpenStream(AStreamId: Cardinal): THorseHttp2Stream;
    function Find(AStreamId: Cardinal): THorseHttp2Stream;
    procedure Remove(AStreamId: Cardinal);
    function Count: Integer;
    function GetAt(AIndex: Integer): THorseHttp2Stream;
    procedure SetInitialWindowSize(ASize: Integer);
    procedure ApplyWindowSizeDelta(ADelta: Integer);
    procedure PurgeClosed;
  end;

implementation

{ THorseHttp2Stream }

constructor THorseHttp2Stream.Create(AStreamId: Cardinal; AInitialWindowSize: Integer);
begin
  inherited Create;
  FStreamId          := AStreamId;
  FState             := THorseHttp2StreamState.ssIdle;
  FRecvWindowSize    := AInitialWindowSize;
  FSendWindowSize    := AInitialWindowSize;
  FHeadersComplete   := False;
  FHeaderBlockLen    := 0;
  FDataLen           := 0;
  FEndStreamReceived := False;
  FErrorCode         := 0;
end;

procedure THorseHttp2Stream.Open;
begin
  if FState <> THorseHttp2StreamState.ssIdle then
    raise EInvalidOperation.CreateFmt(
      'Stream %d: cannot Open from state %d', [FStreamId, Ord(FState)]);
  FState := THorseHttp2StreamState.ssOpen;
end;

procedure THorseHttp2Stream.RemoteEndStream;
begin
  FEndStreamReceived := True;
  case FState of
    THorseHttp2StreamState.ssOpen:
      FState := THorseHttp2StreamState.ssHalfClosedRemote;
    THorseHttp2StreamState.ssHalfClosedLocal:
      FState := THorseHttp2StreamState.ssClosed;
  end;
end;

procedure THorseHttp2Stream.LocalEndStream;
begin
  case FState of
    THorseHttp2StreamState.ssOpen:
      FState := THorseHttp2StreamState.ssHalfClosedLocal;
    THorseHttp2StreamState.ssHalfClosedRemote:
      FState := THorseHttp2StreamState.ssClosed;
  end;
end;

procedure THorseHttp2Stream.Reset(AErrorCode: Cardinal);
begin
  FState     := THorseHttp2StreamState.ssClosed;
  FErrorCode := AErrorCode;
end;

procedure THorseHttp2Stream.AppendHeaderFragment(AData: PByte; ALen: Integer);
begin
  if FHeaderBlockLen + ALen > Length(FHeaderBlockBuffer) then
    SetLength(FHeaderBlockBuffer, FHeaderBlockLen + ALen + 256);
  if ALen > 0 then
    Move(AData^, FHeaderBlockBuffer[FHeaderBlockLen], ALen);
  Inc(FHeaderBlockLen, ALen);
end;

function THorseHttp2Stream.FinalizeHeaders(ADecoder: THpackDecoder): Boolean;
begin
  Result := ADecoder.Decode(@FHeaderBlockBuffer[0], FHeaderBlockLen, FHeaders);
  FHeadersComplete := Result;
end;

procedure THorseHttp2Stream.AppendData(AData: PByte; ALen: Integer);
begin
  if FDataLen + ALen > Length(FDataBuffer) then
    SetLength(FDataBuffer, FDataLen + ALen + 512);
  if ALen > 0 then
    Move(AData^, FDataBuffer[FDataLen], ALen);
  Inc(FDataLen, ALen);
end;

function THorseHttp2Stream.ConsumeSendWindow(ABytes: Integer): Boolean;
begin
  Result := FSendWindowSize >= ABytes;
  if Result then
    Dec(FSendWindowSize, ABytes);
end;

procedure THorseHttp2Stream.IncreaseSendWindow(AIncrement: Integer);
begin
  Inc(FSendWindowSize, AIncrement);
end;

procedure THorseHttp2Stream.ConsumeRecvWindow(ABytes: Integer);
begin
  Dec(FRecvWindowSize, ABytes);
end;

procedure THorseHttp2Stream.RefillRecvWindow(ABytes: Integer);
begin
  Inc(FRecvWindowSize, ABytes);
end;

{ THorseHttp2StreamMap }

constructor THorseHttp2StreamMap.Create(AInitialWindowSize: Integer);
begin
  inherited Create;
  FCount             := 0;
  FInitialWindowSize := AInitialWindowSize;
end;

destructor THorseHttp2StreamMap.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FStreams[i].Free;
  inherited;
end;

function THorseHttp2StreamMap.BinarySearch(AId: Cardinal): Integer;
var
  lo, hi, mid: Integer;
begin
  lo := 0;
  hi := FCount - 1;
  while lo <= hi do
  begin
    mid := (lo + hi) shr 1;
    if FIds[mid] = AId then Exit(mid);
    if FIds[mid] < AId then lo := mid + 1
    else hi := mid - 1;
  end;
  Result := -(lo + 1);
end;

function THorseHttp2StreamMap.OpenStream(AStreamId: Cardinal): THorseHttp2Stream;
var
  idx: Integer;
  insertAt: Integer;
  stream: THorseHttp2Stream;
begin
  idx := BinarySearch(AStreamId);
  if idx >= 0 then
    raise EInvalidOperation.CreateFmt('HTTP/2: stream %d already exists', [AStreamId]);

  insertAt := -(idx + 1);
  stream := THorseHttp2Stream.Create(AStreamId, FInitialWindowSize);

  if FCount >= Length(FIds) then
  begin
    SetLength(FIds, FCount + 16);
    SetLength(FStreams, FCount + 16);
  end;

  if insertAt < FCount then
  begin
    Move(FIds[insertAt], FIds[insertAt + 1], (FCount - insertAt) * SizeOf(Cardinal));
    Move(FStreams[insertAt], FStreams[insertAt + 1], (FCount - insertAt) * SizeOf(Pointer));
  end;

  FIds[insertAt]     := AStreamId;
  FStreams[insertAt] := stream;
  Inc(FCount);
  Result := stream;
end;

function THorseHttp2StreamMap.Find(AStreamId: Cardinal): THorseHttp2Stream;
var
  idx: Integer;
begin
  idx := BinarySearch(AStreamId);
  if idx >= 0 then
    Result := FStreams[idx]
  else
    Result := nil;
end;

procedure THorseHttp2StreamMap.Remove(AStreamId: Cardinal);
var
  idx: Integer;
begin
  idx := BinarySearch(AStreamId);
  if idx < 0 then Exit;
  FStreams[idx].Free;
  if idx < FCount - 1 then
  begin
    Move(FIds[idx + 1], FIds[idx], (FCount - idx - 1) * SizeOf(Cardinal));
    Move(FStreams[idx + 1], FStreams[idx], (FCount - idx - 1) * SizeOf(Pointer));
  end;
  Dec(FCount);
end;

function THorseHttp2StreamMap.Count: Integer;
begin
  Result := FCount;
end;

function THorseHttp2StreamMap.GetAt(AIndex: Integer): THorseHttp2Stream;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateFmt('StreamMap index %d out of range', [AIndex]);
  Result := FStreams[AIndex];
end;

procedure THorseHttp2StreamMap.SetInitialWindowSize(ASize: Integer);
begin
  FInitialWindowSize := ASize;
end;

procedure THorseHttp2StreamMap.ApplyWindowSizeDelta(ADelta: Integer);
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FStreams[i].IncreaseSendWindow(ADelta);
end;

procedure THorseHttp2StreamMap.PurgeClosed;
var
  i: Integer;
begin
  i := 0;
  while i < FCount do
  begin
    if FStreams[i].State = THorseHttp2StreamState.ssClosed then
      Remove(FStreams[i].StreamId)
    else
      Inc(i);
  end;
end;

end.
