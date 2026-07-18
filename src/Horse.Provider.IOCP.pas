unit Horse.Provider.IOCP;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IFDEF MSWINDOWS}
uses
  {$IF DEFINED(FPC)}
  SysUtils,
  Classes,
  syncobjs,
  Generics.Collections,
  Windows,
  WinSock2,
  {$ELSE}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.WinSock2,
  {$ENDIF}
  Horse.Provider.Abstract,
  Horse.Provider.Config,
  Horse.Request,
  Horse.Response,
  Horse.Provider.RawInterfaces,
  Horse.Provider.RawAdapters,
  Horse.Commons,
  Horse.Proc,
  Horse.Exception.Interrupted,
  Horse.Core.WebSocket,
  Horse.Provider.Socket.WebSocket;

type
  TIocpConnectionContext = class;

  {$IF DEFINED(FPC)}
  THorseProviderCallback = Horse.Proc.TProc;
  {$ELSE}
  THorseProviderCallback = System.SysUtils.TProc;
  {$ENDIF}

  THeaderSegment = record
    KeyStart: Integer;
    KeyLen: Integer;
    ValueStart: Integer;
    ValueLen: Integer;
  end;

  THeaderSegments = TArray<THeaderSegment>;

  { Parser HTTP incremental e extremamente rápido idêntico ao do Epoll }
  THorseHttpParser = class
  private
    class function FindByte(const ABuffer: TBytes; AStart, AEnd: Integer; AByte: Byte): Integer; static; inline;
    class function FindCRLF(const ABuffer: TBytes; AStart, AEnd: Integer): Integer; static; inline;
    class function CompareBytesCI(const ABuffer: TBytes; AStart, ALen: Integer; const AStr: string): Boolean; static; inline;
    class function GetMethodString(const ABuffer: TBytes; AStart, ALen: Integer): string; static; inline;
    class function ScanChunkedBody(var ABuffer: TBytes; ALength, ABodyOffset: Integer;
      ACompact: Boolean; out ADecodedLength: Int64): Boolean; static;
  public
    class function TryParseRequest(
      const ABuffer: TBytes; 
      ALength: Integer;
      out AMethod: string;
      out APath: string;
      out AQuery: string;
      out AVersion: string;
      out AHeaders: THeaderSegments;
      out ABodyOffset: Integer;
      out AContentLength: Int64;
      out AIsChunked: Boolean
    ): Boolean; static;
    { Fix G — Transfer-Encoding: chunked support. Returns False while the
      terminating zero-size chunk has not arrived yet (read more and retry).
      On True the chunk data has been compacted in place at ABodyOffset,
      ADecodedLength holds the de-chunked body size and ALength is adjusted
      to ABodyOffset + ADecodedLength. }
    class function TryDecodeChunkedBody(var ABuffer: TBytes; var ALength: Integer;
      ABodyOffset: Integer; out ADecodedLength: Int64): Boolean; static;
  end;

  TIocpReadOnlyBytesStream = class(TCustomMemoryStream)
  public
    constructor Create(const ABytes: TBytes; AOffset, ALen: Integer);
  end;

  { Adaptador de requisição que implementa IHorseRawRequest com Lazy Loading }
  TIocpRawRequest = class(TInterfacedObject, IHorseRawRequest)
  private
    FBuffer: TBytes;
    FMethod: string;
    FPath: string;
    FQuery: string;
    FVersion: string;
    FBodyOffset: Integer;
    FContentLength: Int64;
    FHeaders: THeaderSegments;
    FBodyStream: TStream;
    FTempFileName: string;
    FResolvedHeaders: TDictionary<string, string>;
    FClientIP: string;
    FClientPort: Integer;
    procedure EnsureBodyStream;
    function ResolveHeader(const AName: string): string;
  public
    constructor Create(
      const ABuffer: TBytes;
      ABufferLength: Integer;
      const AMethod, APath, AQuery, AVersion: string;
      AHeaders: THeaderSegments;
      ABodyOffset: Integer;
      AContentLength: Int64;
      ABodyStream: TStream;
      const ATempFileName: string;
      const AClientIP: string;
      AClientPort: Integer
    );
    destructor Destroy; override;

    function GetMethod: string;
    function GetProtocolVersion: string;
    function GetURL: string;
    function GetPathInfo: string;
    function GetQueryString: string;
    function GetHost: string;
    function GetRemoteAddr: string;
    function GetServerPort: Integer;
    function GetContentType: string;
    function GetContent: string;
    
    {$IFDEF FPC}
    function GetContentLength: Integer;
    {$ELSE}
      {$IF CompilerVersion >= 32.0}
      function GetContentLength: Int64;
      {$ELSE}
      function GetContentLength: Integer;
      {$ENDIF}
    {$ENDIF}

    function GetFieldByName(const AName: string): string;
    procedure PopulateHeaders(ADest: TStrings);
    procedure PopulateQueryFields(ADest: TStrings);
    procedure PopulateContentFields(ADest: TStrings);
    procedure PopulateCookieFields(ADest: TStrings);
    function ReadBody(var Buffer; Count: Integer): Integer;
    function GetBody: TStream;
  end;

  { Adaptador de resposta que implementa IHorseRawResponse }
  TIocpRawResponse = class(TInterfacedObject, IHorseRawResponse)
  private
    FContext: TIocpConnectionContext;
    FHeaders: TDictionary<string, string>;
    FHeadersSent: Boolean;
    FIsKeepAlive: Boolean;
    FStatusCode: Integer;
    FStatusReason: string;
    { REPEATHDR-1 — non-owning reference to ARes.RepeatHeaders (Set-Cookie et al).
      Emitted directly in PrepareHeaders, bypassing the FHeaders dedup dict.
      nil unless SendResponse assigned it this request (fresh per request). }
    FRepeatHeaders: TStrings;
    function PrepareHeaders: TBytes;
    procedure WriteV(const AHeaderBytes, ABodyBytes: TBytes);
    procedure SendHeaders;
    procedure SendStreamResponse(AStream: TStream; AHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF});
    procedure FinalizeResponse;
  public
    constructor Create(AContext: TIocpConnectionContext; AIsKeepAlive: Boolean = True);
    destructor Destroy; override;

    procedure SetCustomHeader(const AName, AValue: string);
    procedure SendResponse(const ARes: THorseResponse);
  end;

  { Tipos e estruturas do Winsock2 IOCP }
  TIocpOpType = (ioAccept, ioRead, ioWrite);

  PIocpOverlapped = ^TIocpOverlapped;
  TIocpOverlapped = record
    Overlapped: OVERLAPPED;
    OpType: TIocpOpType;
    Socket: TSocket;
    Buffer: array[0..16383] of Byte;
    BytesTransferred: DWORD;
    Context: Pointer;
  end;

  TAcceptEx = function(sListenSocket, sAcceptSocket: TSocket; lpOutputBuffer: Pointer;
    dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD;
    var lpdwBytesReceived: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;

  TGetAcceptExSockaddrs = procedure(lpOutputBuffer: Pointer;
    dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD;
    var LocalSockaddr: PSockAddr; var LocalSockaddrLength: Integer;
    var RemoteSockaddr: PSockAddr; var RemoteSockaddrLength: Integer); stdcall;

  { Contexto de Conexão ativa no IOCP (Com campos publicos para performance e compatibilidade var/out) }
  TIocpConnectionContext = class
  public
    Socket: TSocket;
    ReadOverlapped: TIocpOverlapped;
    WriteOverlapped: TIocpOverlapped;
    AcceptOverlapped: TIocpOverlapped;
    Buffer: TBytes;
    BytesReceived: Integer;
    BodyOffset: Integer;
    ContentLength: Int64;
    IsKeepAlive: Boolean;
    Processing: Boolean;
    Closed: Integer;
    RefCount: Integer;
    LastActive: Int64;
    ClientIP: string;
    ClientPort: Integer;

    constructor Create(ASocket: TSocket);
    destructor Destroy; override;
  end;

  { Thread worker que consome o Completion Port do Windows }
  THorseIocpWorker = class(TThread)
  private
    FListenSocket: TSocket;
    FIocpHandle: THandle;
    FRunning: Boolean;
    FConnections: TList<TIocpConnectionContext>;
    FConnectionsSync: TCriticalSection;
    procedure ProcessClientRead(AContext: TIocpConnectionContext; ABytesTransferred: DWORD);
    procedure ProcessClientWrite(AContext: TIocpConnectionContext; ABytesTransferred: DWORD);
    procedure CloseConnection(AContext: TIocpConnectionContext);
    procedure CheckTimeouts;
    procedure PostAccept;
    procedure PostRead(AContext: TIocpConnectionContext);
  protected
    procedure Execute; override;
  public
    constructor Create(AListenSocket: TSocket; AIocpHandle: THandle);
    destructor Destroy; override;
    procedure TerminateWorker;
  end;

  { Classe Provider concreta do Horse para Windows baseada em IOCP }
  THorseProviderIOCP = class(THorseProviderAbstract)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FListenSocket: TSocket;
    class var FIocpHandle: THandle;
    class var FWorkers: TObjectList<THorseIocpWorker>;

    class procedure SetPort(const AValue: Integer); static;
    class procedure SetHost(const AValue: string); static;
    class function GetPort: Integer; static;
    class function GetHost: string; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;

    class procedure InternalListen;
    class procedure InternalStopListen;
    class function CreateListenSocket(const APort: Integer; const AHost: string): TSocket; static;
    class procedure InternalListenLoop(const ACallbackListen: Horse.Proc.TProc; const ACallbackStopListen: Horse.Proc.TProc); static;
    class procedure PostReadConnection(AContext: TIocpConnectionContext); static;
  public
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0'; const ACallbackListen: Horse.Proc.TProc = nil; const ACallbackStopListen: Horse.Proc.TProc = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallbackListen: Horse.Proc.TProc; const ACallbackStopListen: Horse.Proc.TProc = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallbackListen: Horse.Proc.TProc = nil; const ACallbackStopListen: Horse.Proc.TProc = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: Horse.Proc.TProc; const ACallbackStopListen: Horse.Proc.TProc = nil); reintroduce; overload; static;
    class procedure ListenWithConfig(const APort: Integer; const AConfig: THorseCrossSocketConfig); override;
    class function GetActivePort: Integer; override;
    class procedure StopListen; override;
    class function IsRunning: Boolean;

    class constructor CreateClass;
    class destructor DestroyClass;
  end;

  THorseProvider = class(THorseProviderIOCP);

  TWorkItemData = record
    Req: THorseRequest;
    Res: THorseResponse;
    RawRes: TIocpRawResponse;
    {$IFNDEF FPC}
    WebRequest: TInterfacedWebRequest;
    {$ENDIF}
  end;
  PWorkItemData = ^TWorkItemData;

  function QueueUserWorkItem(Func: Pointer; Context: Pointer; Flags: ULONG): BOOL; stdcall; external kernel32 name 'QueueUserWorkItem';

const
  WT_EXECUTEDEFAULT = $00000000;
  SO_UPDATE_ACCEPT_CONTEXT = $700B;

{$ENDIF}

implementation

{$IFNDEF FPC}
  {$IF CompilerVersion < 31.0}
  function GetTickCount64: UInt64; stdcall; external 'kernel32.dll' name 'GetTickCount64';
  {$IFEND}
{$ENDIF}

{$IFDEF MSWINDOWS}

var
  fnAcceptEx: TAcceptEx = nil;
  fnGetAcceptExSockaddrs: TGetAcceptExSockaddrs = nil;

function GetWSAExtensionPointer(ASocket: TSocket; const AGUID: TGUID; out APointer: Pointer): Boolean;
var
  dwBytes: DWORD;
  LGuid: TGUID;
begin
  LGuid := AGUID;
  APointer := nil;
  Result := WSAIoctl(ASocket, SIO_GET_EXTENSION_FUNCTION_POINTER, @LGuid, SizeOf(LGuid),
    @APointer, SizeOf(APointer), dwBytes, nil, nil) <> SOCKET_ERROR;
end;

{ THorseHttpParser }

class function THorseHttpParser.FindByte(const ABuffer: TBytes; AStart, AEnd: Integer; AByte: Byte): Integer;
var
  I: Integer;
begin
  for I := AStart to AEnd - 1 do
    if ABuffer[I] = AByte then
      Exit(I);
  Result := -1;
end;

class function THorseHttpParser.FindCRLF(const ABuffer: TBytes; AStart, AEnd: Integer): Integer;
var
  I: Integer;
begin
  for I := AStart to AEnd - 2 do
    if (ABuffer[I] = 13) and (ABuffer[I + 1] = 10) then
      Exit(I);
  Result := -1;
end;

class function THorseHttpParser.CompareBytesCI(const ABuffer: TBytes; AStart, ALen: Integer; const AStr: string): Boolean;
var
  I: Integer;
  B: Byte;
begin
  if ALen <> Length(AStr) then Exit(False);
  for I := 0 to ALen - 1 do
  begin
    B := ABuffer[AStart + I];
    if (B >= 65) and (B <= 90) then B := B + 32;
    if Char(B) <> LowerCase(AStr[I + 1])[1] then
      Exit(False);
  end;
  Result := True;
end;

class function THorseHttpParser.GetMethodString(const ABuffer: TBytes; AStart, ALen: Integer): string;
begin
  SetString(Result, PAnsiChar(@ABuffer[AStart]), ALen);
end;

class function THorseHttpParser.TryParseRequest(
  const ABuffer: TBytes; 
  ALength: Integer;
  out AMethod: string;
  out APath: string;
  out AQuery: string;
  out AVersion: string;
  out AHeaders: THeaderSegments;
  out ABodyOffset: Integer;
  out AContentLength: Int64;
  out AIsChunked: Boolean
): Boolean;
var
  LHeaderEnd, LLineStart, LLineEnd, LSpace1, LSpace2, LColon, I: Integer;
  LMethodLen, LPathLen, LVersionLen: Integer;
  LRawPath, LRawQuery: string;
  LHasContentLength: Boolean;
begin
  Result := False;
  ABodyOffset := -1;
  AContentLength := 0;
  AIsChunked := False;
  AHeaders := nil;

  // Encontra o fim dos cabeçalhos (\r\n\r\n)
  LHeaderEnd := -1;
  for I := 0 to ALength - 4 do
  begin
    if (ABuffer[I] = 13) and (ABuffer[I + 1] = 10) and (ABuffer[I + 2] = 13) and (ABuffer[I + 3] = 10) then
    begin
      LHeaderEnd := I;
      ABodyOffset := I + 4;
      Break;
    end;
  end;

  if LHeaderEnd = -1 then Exit;

  // 1. Parse da Request Line
  LLineEnd := FindCRLF(ABuffer, 0, LHeaderEnd);
  if LLineEnd = -1 then Exit;

  LSpace1 := FindByte(ABuffer, 0, LLineEnd, 32);
  if LSpace1 = -1 then Exit;

  LSpace2 := FindByte(ABuffer, LSpace1 + 1, LLineEnd, 32);
  if LSpace2 = -1 then Exit;

  LMethodLen := LSpace1;
  LPathLen := LSpace2 - LSpace1 - 1;
  LVersionLen := LLineEnd - LSpace2 - 1;

  AMethod := GetMethodString(ABuffer, 0, LMethodLen);
  LRawPath := GetMethodString(ABuffer, LSpace1 + 1, LPathLen);
  AVersion := GetMethodString(ABuffer, LSpace2 + 1, LVersionLen);

  // Divide Path e Query String
  I := Pos('?', LRawPath);
  if I > 0 then
  begin
    APath := Copy(LRawPath, 1, I - 1);
    AQuery := Copy(LRawPath, I + 1, Length(LRawPath));
  end
  else
  begin
    APath := LRawPath;
    AQuery := '';
  end;

  // 2. Parse dos Headers
  LLineStart := LLineEnd + 2;
  LHasContentLength := False;

  while LLineStart < LHeaderEnd do
  begin
    LLineEnd := FindCRLF(ABuffer, LLineStart, LHeaderEnd);
    if LLineEnd = -1 then LLineEnd := LHeaderEnd;

    LColon := FindByte(ABuffer, LLineStart, LLineEnd, 58); // ':'
    if LColon <> -1 then
    begin
      SetLength(AHeaders, Length(AHeaders) + 1);
      AHeaders[High(AHeaders)].KeyStart := LLineStart;
      AHeaders[High(AHeaders)].KeyLen := LColon - LLineStart;
      
      // Trim espaços após os dois pontos
      I := LColon + 1;
      while (I < LLineEnd) and (ABuffer[I] = 32) do
        Inc(I);
      
      AHeaders[High(AHeaders)].ValueStart := I;
      AHeaders[High(AHeaders)].ValueLen := LLineEnd - I;

      // Verifica se é Content-Length
      if CompareBytesCI(ABuffer, AHeaders[High(AHeaders)].KeyStart, AHeaders[High(AHeaders)].KeyLen, 'content-length') then
      begin
        LRawQuery := GetMethodString(ABuffer, AHeaders[High(AHeaders)].ValueStart, AHeaders[High(AHeaders)].ValueLen);
        TryStrToInt64(LRawQuery, AContentLength);
        LHasContentLength := True;
      end
      { Fix G (re-derived onto merged 2026-07-17, upstream-PR candidate) —
        Transfer-Encoding: chunked request body (no Content-Length; framed by
        chunk-size lines, terminated by a zero-size chunk). TCrossHttpClient sends
        multipart uploads this way; merged IOCP only knew Content-Length, so it
        dispatched with an empty body and left the chunk bytes to poison the next
        keep-alive request (test 13 cascade on the pooling client). }
      else if CompareBytesCI(ABuffer, AHeaders[High(AHeaders)].KeyStart, AHeaders[High(AHeaders)].KeyLen, 'transfer-encoding') then
      begin
        LRawQuery := GetMethodString(ABuffer, AHeaders[High(AHeaders)].ValueStart, AHeaders[High(AHeaders)].ValueLen);
        if Pos('chunked', LowerCase(LRawQuery)) > 0 then
          AIsChunked := True;
      end;
    end;

    LLineStart := LLineEnd + 2;
  end;

  Result := True;
end;

class function THorseHttpParser.ScanChunkedBody(var ABuffer: TBytes;
  ALength, ABodyOffset: Integer; ACompact: Boolean;
  out ADecodedLength: Int64): Boolean;
var
  LSrc, LDst, LLineEnd, I, LDigit: Integer;
  LChunkSize: Int64;
  LB: Byte;
begin
  Result := False;
  ADecodedLength := 0;
  LSrc := ABodyOffset;
  LDst := ABodyOffset;

  while True do
  begin
    { chunk-size line: hex digits, optional ';extensions', terminated by CRLF }
    LLineEnd := FindCRLF(ABuffer, LSrc, ALength);
    if LLineEnd = -1 then Exit;   { size line not complete yet }

    LChunkSize := 0;
    I := LSrc;
    while I < LLineEnd do
    begin
      LB := ABuffer[I];
      if (LB >= 48) and (LB <= 57) then        { '0'..'9' }
        LDigit := LB - 48
      else if (LB >= 97) and (LB <= 102) then  { 'a'..'f' }
        LDigit := LB - 87
      else if (LB >= 65) and (LB <= 70) then   { 'A'..'F' }
        LDigit := LB - 55
      else
        Break;                                 { ';' extension or garbage }
      LChunkSize := (LChunkSize * 16) + LDigit;
      if LChunkSize > $7FFFFFFF then Exit;     { insane size — treat as incomplete }
      Inc(I);
    end;
    if I = LSrc then Exit;        { no hex digit at all — malformed }
    LSrc := LLineEnd + 2;

    if LChunkSize = 0 then
    begin
      { zero-size chunk: optional trailer header lines, then a final CRLF }
      while True do
      begin
        LLineEnd := FindCRLF(ABuffer, LSrc, ALength);
        if LLineEnd = -1 then Exit;   { trailer section not complete yet }
        if LLineEnd = LSrc then       { empty line — end of chunked body }
        begin
          ADecodedLength := LDst - ABodyOffset;
          Result := True;
          Exit;
        end;
        LSrc := LLineEnd + 2;         { skip trailer header line }
      end;
    end;

    { chunk data plus its trailing CRLF must be fully present }
    if LSrc + LChunkSize + 2 > ALength then Exit;
    if ACompact and (LDst <> LSrc) then
      Move(ABuffer[LSrc], ABuffer[LDst], LChunkSize);
    LDst := LDst + Integer(LChunkSize);
    LSrc := LSrc + Integer(LChunkSize) + 2;
  end;
end;

class function THorseHttpParser.TryDecodeChunkedBody(var ABuffer: TBytes;
  var ALength: Integer; ABodyOffset: Integer;
  out ADecodedLength: Int64): Boolean;
begin
  { pass 1 — scan only: verify the terminating zero-size chunk has arrived.
    No mutation happens, so an incomplete body can safely be re-scanned
    after the next read completion delivers more bytes. }
  Result := ScanChunkedBody(ABuffer, ALength, ABodyOffset, False, ADecodedLength);
  if not Result then Exit;

  { pass 2 — compact the chunk data in place over the chunk framing.
    LDst only ever trails LSrc, so the forward Move is safe. }
  ScanChunkedBody(ABuffer, ALength, ABodyOffset, True, ADecodedLength);
  ALength := ABodyOffset + Integer(ADecodedLength);
end;

{ TIocpReadOnlyBytesStream }

constructor TIocpReadOnlyBytesStream.Create(const ABytes: TBytes; AOffset, ALen: Integer);
begin
  inherited Create;
  SetPointer(Pointer(PByte(ABytes) + AOffset), ALen);
end;

{ TIocpRawRequest }

constructor TIocpRawRequest.Create(
  const ABuffer: TBytes;
  ABufferLength: Integer;
  const AMethod, APath, AQuery, AVersion: string;
  AHeaders: THeaderSegments;
  ABodyOffset: Integer;
  AContentLength: Int64;
  ABodyStream: TStream;
  const ATempFileName: string;
  const AClientIP: string;
  AClientPort: Integer
);
begin
  inherited Create;
  SetLength(FBuffer, ABufferLength);
  if ABufferLength > 0 then
    Move(ABuffer[0], FBuffer[0], ABufferLength);
  FMethod := AMethod;
  FPath := APath;
  FQuery := AQuery;
  FVersion := AVersion;
  FHeaders := AHeaders;
  FBodyOffset := ABodyOffset;
  FContentLength := AContentLength;
  FBodyStream := ABodyStream;
  FTempFileName := ATempFileName;
  FClientIP := AClientIP;
  FClientPort := AClientPort;
  FResolvedHeaders := TDictionary<string, string>.Create;
end;

destructor TIocpRawRequest.Destroy;
begin
  FResolvedHeaders.Free;
  if Assigned(FBodyStream) then
    FBodyStream.Free;
  if (FTempFileName <> '') and System.SysUtils.FileExists(FTempFileName) then
    System.SysUtils.DeleteFile(FTempFileName);
  inherited;
end;

procedure TIocpRawRequest.EnsureBodyStream;
begin
  if not Assigned(FBodyStream) then
  begin
    if (FBodyOffset >= 0) and (FContentLength > 0) then
      FBodyStream := TIocpReadOnlyBytesStream.Create(FBuffer, FBodyOffset, FContentLength)
    else
      FBodyStream := TIocpReadOnlyBytesStream.Create(nil, 0, 0);
  end;
end;

function TIocpRawRequest.ResolveHeader(const AName: string): string;
var
  I: Integer;
  Seg: THeaderSegment;
  LLowerName: string;
begin
  LLowerName := LowerCase(AName);
  if FResolvedHeaders.TryGetValue(LLowerName, Result) then Exit;

  for I := 0 to Length(FHeaders) - 1 do
  begin
    Seg := FHeaders[I];
    if THorseHttpParser.CompareBytesCI(FBuffer, Seg.KeyStart, Seg.KeyLen, LLowerName) then
    begin
      Result := TEncoding.UTF8.GetString(FBuffer, Seg.ValueStart, Seg.ValueLen).Trim;
      FResolvedHeaders.Add(LLowerName, Result);
      Exit;
    end;
  end;

  Result := '';
  FResolvedHeaders.Add(LLowerName, '');
end;

function TIocpRawRequest.GetMethod: string;
begin
  Result := FMethod;
end;

function TIocpRawRequest.GetProtocolVersion: string;
begin
  Result := FVersion;
end;

function TIocpRawRequest.GetURL: string;
begin
  if FQuery <> '' then
    Result := FPath + '?' + FQuery
  else
    Result := FPath;
end;

function TIocpRawRequest.GetPathInfo: string;
begin
  Result := FPath;
end;

function TIocpRawRequest.GetQueryString: string;
begin
  Result := FQuery;
end;

function TIocpRawRequest.GetHost: string;
begin
  Result := ResolveHeader('host');
end;

function TIocpRawRequest.GetRemoteAddr: string;
begin
  Result := FClientIP;
end;

function TIocpRawRequest.GetServerPort: Integer;
begin
  Result := THorseProviderIOCP.Port;
end;

function TIocpRawRequest.GetContentType: string;
begin
  Result := ResolveHeader('content-type');
end;

function TIocpRawRequest.GetContent: string;
var
  LBytes: TBytes;
begin
  EnsureBodyStream;
  if FBodyStream.Size = 0 then
    Exit('');
  SetLength(LBytes, FBodyStream.Size);
  FBodyStream.Position := 0;
  FBodyStream.ReadBuffer(LBytes[0], FBodyStream.Size);
  Result := TEncoding.UTF8.GetString(LBytes);
end;

{$IFDEF FPC}
function TIocpRawRequest.GetContentLength: Integer;
begin
  Result := FContentLength;
end;
{$ELSE}
  {$IF CompilerVersion >= 32.0}
  function TIocpRawRequest.GetContentLength: Int64;
  begin
    Result := FContentLength;
  end;
  {$ELSE}
  function TIocpRawRequest.GetContentLength: Integer;
  begin
    Result := FContentLength;
  end;
  {$ENDIF}
{$ENDIF}

function TIocpRawRequest.GetFieldByName(const AName: string): string;
begin
  Result := ResolveHeader(AName);
end;

procedure TIocpRawRequest.PopulateHeaders(ADest: TStrings);
var
  I: Integer;
  Seg: THeaderSegment;
  K, V: AnsiString;
begin
  for I := 0 to Length(FHeaders) - 1 do
  begin
    Seg := FHeaders[I];
    SetString(K, PAnsiChar(@FBuffer[Seg.KeyStart]), Seg.KeyLen);
    SetString(V, PAnsiChar(@FBuffer[Seg.ValueStart]), Seg.ValueLen);
    ADest.Add(string(Trim(K)) + ADest.NameValueSeparator + string(Trim(V)));
  end;
end;

procedure TIocpRawRequest.PopulateQueryFields(ADest: TStrings);
var
  LQuery: string;
  I, LStart, LLen, LEqPos: Integer;
  LName, LValue: string;
begin
  LQuery := GetQueryString;
  if LQuery = '' then Exit;
  if LQuery.StartsWith('?') then
    LQuery := LQuery.Substring(1);

  LStart := 1;
  LLen := Length(LQuery);
  for I := 1 to LLen do
  begin
    if (LQuery[I] = '&') or (I = LLen) then
    begin
      LEqPos := Pos('=', LQuery, LStart);
      if (LEqPos > 0) and (LEqPos < I) then
      begin
        LName := Copy(LQuery, LStart, LEqPos - LStart);
        LValue := Copy(LQuery, LEqPos + 1, I - LEqPos - 1);
        ADest.Add(LName + '=' + LValue);
      end;
      LStart := I + 1;
    end;
  end;
end;

procedure TIocpRawRequest.PopulateContentFields(ADest: TStrings);
var
  LBody: string;
  I, LStart, LLen, LEqPos: Integer;
  LName, LValue: string;
begin
  if not SameText(GetContentType, 'application/x-www-form-urlencoded') then
    Exit;

  LBody := GetContent;
  if LBody = '' then Exit;

  LStart := 1;
  LLen := Length(LBody);
  for I := 1 to LLen do
  begin
    if (LBody[I] = '&') or (I = LLen) then
    begin
      LEqPos := Pos('=', LBody, LStart);
      if (LEqPos > 0) and (LEqPos < I) then
      begin
        LName := Copy(LBody, LStart, LEqPos - LStart);
        LValue := Copy(LBody, LEqPos + 1, I - LEqPos - 1);
        ADest.Add(LName + '=' + LValue);
      end;
      LStart := I + 1;
    end;
  end;
end;

procedure TIocpRawRequest.PopulateCookieFields(ADest: TStrings);
var
  LCookies: string;
  I, LStart, LLen, LEqPos: Integer;
  LName, LValue, LPart: string;
begin
  LCookies := GetFieldByName('Cookie');
  if LCookies = '' then Exit;

  LStart := 1;
  LLen := Length(LCookies);
  for I := 1 to LLen do
  begin
    if (LCookies[I] = ';') or (I = LLen) then
    begin
      { #11 (2026-07-17, upstream-PR candidate) — the FINAL segment (I = LLen,
        no trailing ';') must INCLUDE position I, so its length is I-LStart+1.
        The old unconditional `I - LStart` was correct only for the ';'-delimited
        case (which excludes the ';'); on the last cookie it dropped the final
        character (test 11: `session=abc123` -> `abc12`). A trailing ';' still
        takes the exclude path and yields an empty segment (skipped by Pos). }
      if LCookies[I] = ';' then
        LPart := Copy(LCookies, LStart, I - LStart).Trim
      else
        LPart := Copy(LCookies, LStart, I - LStart + 1).Trim;
      LEqPos := Pos('=', LPart);
      if LEqPos > 0 then
      begin
        LName := Copy(LPart, 1, LEqPos - 1).Trim;
        LValue := Copy(LPart, LEqPos + 1, Length(LPart) - LEqPos).Trim;
        ADest.Add(LName + '=' + LValue);
      end;
      LStart := I + 1;
    end;
  end;
end;

function TIocpRawRequest.ReadBody(var Buffer; Count: Integer): Integer;
begin
  EnsureBodyStream;
  Result := FBodyStream.Read(Buffer, Count);
end;

function TIocpRawRequest.GetBody: TStream;
begin
  EnsureBodyStream;
  Result := FBodyStream;
end;

{ TIocpRawResponse }

constructor TIocpRawResponse.Create(AContext: TIocpConnectionContext; AIsKeepAlive: Boolean);
begin
  inherited Create;
  FContext := AContext;
  FHeaders := TDictionary<string, string>.Create;
  FHeadersSent := False;
  FIsKeepAlive := AIsKeepAlive;
  FStatusCode := 200;
  FStatusReason := 'OK';
end;

destructor TIocpRawResponse.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure TIocpRawResponse.SetCustomHeader(const AName, AValue: string);
begin
  FHeaders.AddOrSetValue(AName, AValue);
end;

function TIocpRawResponse.PrepareHeaders: TBytes;
var
  LBuilder: TStringBuilder;
  LPair: TPair<string, string>;
  LHeaderStr: string;
  LRepeatIdx: Integer;  { REPEATHDR-1 }
begin
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.Append('HTTP/1.1 ').Append(FStatusCode).Append(' ').Append(FStatusReason).Append(#13#10);
    
    if not FHeaders.ContainsKey('Content-Type') then
      LBuilder.Append('Content-Type: text/plain; charset=utf-8'#13#10);
      
    if not FHeaders.ContainsKey('Date') then
      LBuilder.Append('Date: ').Append(FormatDateTime('ddd, dd mmm yyyy hh:nn:ss" GMT"', System.SysUtils.Now)).Append(#13#10);

    if not FHeaders.ContainsKey('Server') then
      LBuilder.Append('Server: Horse IOCP Server/1.0'#13#10);

    if FIsKeepAlive then
      LBuilder.Append('Connection: keep-alive'#13#10)
    else
      LBuilder.Append('Connection: close'#13#10);

    for LPair in FHeaders do
      LBuilder.Append(LPair.Key).Append(': ').Append(LPair.Value).Append(#13#10);

    { REPEATHDR-1 — emit duplicate-preserving headers (Set-Cookie) verbatim,
      one wire line each, bypassing the FHeaders dict that would collapse them. }
    if FRepeatHeaders <> nil then
      for LRepeatIdx := 0 to FRepeatHeaders.Count - 1 do
        LBuilder.Append(FRepeatHeaders.Names[LRepeatIdx]).Append(': ')
          .Append(FRepeatHeaders.ValueFromIndex[LRepeatIdx]).Append(#13#10);

    LBuilder.Append(#13#10);
    LHeaderStr := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;

  Result := TEncoding.UTF8.GetBytes(LHeaderStr);
end;

procedure TIocpRawResponse.WriteV(const AHeaderBytes, ABodyBytes: TBytes);
var
  LBufs: array[0..1] of TWSABUF;
  LBufCount: DWORD;
  LBytesSent: DWORD;
  LFlags: DWORD;
begin
  LBufCount := 0;
  
  if Length(AHeaderBytes) > 0 then
  begin
    LBufs[LBufCount].buf := PAnsiChar(@AHeaderBytes[0]);
    LBufs[LBufCount].len := Length(AHeaderBytes);
    Inc(LBufCount);
  end;
  
  if Length(ABodyBytes) > 0 then
  begin
    LBufs[LBufCount].buf := PAnsiChar(@ABodyBytes[0]);
    LBufs[LBufCount].len := Length(ABodyBytes);
    Inc(LBufCount);
  end;
  
  if LBufCount > 0 then
  begin
    LFlags := 0;
    LBytesSent := 0;
    WSASend(FContext.Socket, @LBufs[0], LBufCount, LBytesSent, LFlags, nil, nil);
  end;
  FHeadersSent := True;
end;

procedure TIocpRawResponse.SendHeaders;
var
  LHeaderBytes: TBytes;
begin
  if FHeadersSent then Exit;
  LHeaderBytes := PrepareHeaders;
  if Length(LHeaderBytes) > 0 then
    send(FContext.Socket, LHeaderBytes[0], Length(LHeaderBytes), 0);
  FHeadersSent := True;
end;

procedure TIocpRawResponse.SendStreamResponse(AStream: TStream; AHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF});
var
  LStreamSize: Int64;
  LChunkBuf: TBytes;
  LReadCount: Integer;
  LPair: TPair<string, string>;
begin
  if AHeadersList <> nil then
  begin
    {$IFDEF FPC}
    for LReadCount := 0 to AHeadersList.Count - 1 do
      FHeaders.AddOrSetValue(AHeadersList.Names[LReadCount], AHeadersList.ValueFromIndex[LReadCount]);
    {$ELSE}
    for LPair in AHeadersList do
      FHeaders.AddOrSetValue(LPair.Key, LPair.Value);
    {$ENDIF}
  end;

  LStreamSize := AStream.Size - AStream.Position;
  if LStreamSize > 0 then
    FHeaders.AddOrSetValue('Content-Length', IntToStr(LStreamSize));

  SendHeaders;

  SetLength(LChunkBuf, 16384);
  while True do
  begin
    LReadCount := AStream.Read(LChunkBuf[0], Length(LChunkBuf));
    if LReadCount <= 0 then Break;
    send(FContext.Socket, LChunkBuf[0], LReadCount, 0);
  end;
  FinalizeResponse;
end;

procedure TIocpRawResponse.FinalizeResponse;
begin
  if FIsKeepAlive then
  begin
    FContext.BytesReceived := 0;
    FContext.Processing := False;
    THorseProviderIOCP.PostReadConnection(FContext);
  end
  else
  begin
    closesocket(FContext.Socket);
    FContext.Socket := INVALID_SOCKET;
  end;
end;

procedure TIocpRawResponse.SendResponse(const ARes: THorseResponse);
var
  LBodyBytes: TBytes;
  LHeaderBytes: TBytes;
  LPair: TPair<string, string>;
  LHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF};
  {$IFDEF FPC}
  LStatusCode: Integer;
  {$ENDIF}
  { REPEATHDR-1 / test-28 — iterate the response adapter's own CustomHeaders. }
  LAdapterHeaders: TStrings;
  LAdapterIdx: Integer;
begin
  FStatusCode := ARes.Status;
  { REPEATHDR-1 — borrow the ordered dup-preserving store for PrepareHeaders. }
  FRepeatHeaders := ARes.RepeatHeaders;
  
  if FStatusCode = 200 then FStatusReason := 'OK'
  else if FStatusCode = 204 then FStatusReason := 'No Content'
  else if FStatusCode = 404 then FStatusReason := 'Not Found'
  else if FStatusCode = 500 then FStatusReason := 'Internal Server Error'
  else FStatusReason := 'HTTP Response';

  { REPEATHDR-1 — copy the shadow CustomHeaders into FHeaders, but SKIP
    Set-Cookie: the dict would collapse repeats, and every Set-Cookie is emitted
    verbatim from FRepeatHeaders in PrepareHeaders instead (test 10). }
  LHeadersList := ARes.CustomHeaders;
  if LHeadersList <> nil then
  begin
    {$IFDEF FPC}
    for LStatusCode := 0 to LHeadersList.Count - 1 do
      if not SameText(LHeadersList.Names[LStatusCode], 'Set-Cookie') then
        FHeaders.AddOrSetValue(LHeadersList.Names[LStatusCode], LHeadersList.ValueFromIndex[LStatusCode]);
    {$ELSE}
    for LPair in LHeadersList do
      if not SameText(LPair.Key, 'Set-Cookie') then
        FHeaders.AddOrSetValue(LPair.Key, LPair.Value);
    {$ENDIF}
  end;

  { REPEATHDR-1 / test-28 — headers set via Res.RawWebResponse.SetCustomHeader
    land on the adapter's own CustomHeaders (inherited TWebResponse store), NOT
    the shadow FCustomHeaders read above. On IOCP FWebResponse is nil, so the
    two stores are disjoint — emit the UNION. (Reading the adapter INSTEAD of the
    shadow was the reverted Fix B: it dropped every Res.AddHeader'd header.)
    Set-Cookie is skipped here too — it flows through FRepeatHeaders. }
  if ARes.RawWebResponse <> nil then
  begin
    LAdapterHeaders := TInterfacedWebResponse(ARes.RawWebResponse).CustomHeaders;
    if LAdapterHeaders <> nil then
      for LAdapterIdx := 0 to LAdapterHeaders.Count - 1 do
        if not SameText(LAdapterHeaders.Names[LAdapterIdx], 'Set-Cookie') then
          FHeaders.AddOrSetValue(LAdapterHeaders.Names[LAdapterIdx],
            LAdapterHeaders.ValueFromIndex[LAdapterIdx]);
  end;

  { Fix A (re-derived onto merged 2026-07-17) — honour ARes.CSContentType set via
    Res.ContentType(...). Merged IOCP only populates FHeaders from CustomHeaders,
    so Res.ContentType was dropped and every response defaulted to text/plain
    (test 23). Upstream-PR candidate. }
  if (ARes.CSContentType <> '') and not FHeaders.ContainsKey('Content-Type') then
    FHeaders.AddOrSetValue('Content-Type', ARes.CSContentType);

  if ARes.ContentStream <> nil then
  begin
    SendStreamResponse(ARes.ContentStream, ARes.CustomHeaders);
    Exit;
  end
  else if (ARes.RawWebResponse <> nil) and (TInterfacedWebResponse(ARes.RawWebResponse).ContentStream <> nil) then
  begin
    SendStreamResponse(TInterfacedWebResponse(ARes.RawWebResponse).ContentStream, ARes.CustomHeaders);
    Exit;
  end;

  if Length(ARes.BodyBytes) > 0 then
    LBodyBytes := ARes.BodyBytes
  else if ARes.BodyText <> '' then
    LBodyBytes := TEncoding.UTF8.GetBytes(ARes.BodyText)
  else if (ARes.RawWebResponse <> nil) and (TInterfacedWebResponse(ARes.RawWebResponse).Content <> '') then
    LBodyBytes := TEncoding.UTF8.GetBytes(TInterfacedWebResponse(ARes.RawWebResponse).Content);

  if Length(LBodyBytes) > 0 then
    FHeaders.AddOrSetValue('Content-Length', IntToStr(Length(LBodyBytes)))
  else
    FHeaders.AddOrSetValue('Content-Length', '0');

  if not FHeadersSent then
  begin
    LHeaderBytes := PrepareHeaders;
    WriteV(LHeaderBytes, LBodyBytes);
  end
  else if Length(LBodyBytes) > 0 then
  begin
    send(FContext.Socket, LBodyBytes[0], Length(LBodyBytes), 0);
  end;
    
  FinalizeResponse;
end;

{ TIocpConnectionContext }

constructor TIocpConnectionContext.Create(ASocket: TSocket);
begin
  inherited Create;
  Socket := ASocket;
  ReadOverlapped.OpType := ioRead;
  ReadOverlapped.Socket := ASocket;
  ReadOverlapped.Context := Self;
  WriteOverlapped.OpType := ioWrite;
  WriteOverlapped.Socket := ASocket;
  WriteOverlapped.Context := Self;
  AcceptOverlapped.OpType := ioAccept;
  AcceptOverlapped.Socket := ASocket;
  AcceptOverlapped.Context := Self;
  SetLength(Buffer, 8192);
  BytesReceived := 0;
  BodyOffset := -1;
  ContentLength := 0;
  IsKeepAlive := True;
  Processing := False;
  Closed := 0;
  RefCount := 1;
  LastActive := GetTickCount64;
end;

destructor TIocpConnectionContext.Destroy;
begin
  if Socket <> INVALID_SOCKET then
  begin
    closesocket(Socket);
    Socket := INVALID_SOCKET;
  end;
  inherited;
end;

{ Callback de processamento no Windows Thread Pool }

function IocpWorkItemCallback(LPParameter: Pointer): DWORD; stdcall;
var
  LData: PWorkItemData;
  LContext: TIocpConnectionContext;
begin
  Result := 0;
  LData := PWorkItemData(LPParameter);
  if (LData.Req <> nil) and (LData.RawRes <> nil) and (LData.RawRes.FContext <> nil) then
  begin
    LData.Req.Services.Add(THorseWebSocketUpgrader,
      THorseWebSocketSocketUpgrader.Create(
        LData.RawRes.FContext.Socket,
        LData.Req.WebSocketKey,
        LData.RawRes.FContext.ClientIP,
        LData.RawRes.FContext.ClientPort
      ), True);
  end;
  try
    try
      THorseProviderIOCP.Execute(LData.Req, LData.Res);
      LData.RawRes.SendResponse(LData.Res);
    except
      on E: Exception do
      begin
        if E.InheritsFrom(EHorseCallbackInterrupted) then
        begin
          LData.RawRes.SendResponse(LData.Res);
        end
        else
        begin
          LData.Res.Status(500).Send(E.Message);
          LData.RawRes.SendResponse(LData.Res);
        end;
      end;
    end;
  finally
    if LData.RawRes <> nil then
      LContext := LData.RawRes.FContext
    else
      LContext := nil;

    LData.Req.Free;
    LData.Res.Free;
    {$IFNDEF FPC}
    LData.WebRequest.Free;
    {$ENDIF}
    Dispose(LData);

    if (LContext <> nil) and (InterlockedDecrement(LContext.RefCount) = 0) then
      LContext.Free;
  end;
end;

{ THorseIocpWorker }

constructor THorseIocpWorker.Create(AListenSocket: TSocket; AIocpHandle: THandle);
begin
  inherited Create(False);
  FListenSocket := AListenSocket;
  FIocpHandle := AIocpHandle;
  FRunning := True;
  FConnections := TList<TIocpConnectionContext>.Create;
  FConnectionsSync := TCriticalSection.Create;
end;

destructor THorseIocpWorker.Destroy;
var
  I: Integer;
  LContext: TIocpConnectionContext;
begin
  FConnectionsSync.Enter;
  try
    for I := 0 to FConnections.Count - 1 do
    begin
      LContext := FConnections[I];
      if LContext <> nil then
      begin
        if LContext.Socket <> INVALID_SOCKET then
        begin
          closesocket(LContext.Socket);
          LContext.Socket := INVALID_SOCKET;
        end;
        if InterlockedDecrement(LContext.RefCount) = 0 then
          LContext.Free;
      end;
    end;
    FConnections.Clear;
  finally
    FConnectionsSync.Leave;
  end;
  FConnections.Free;
  FConnectionsSync.Free;
  inherited;
end;

procedure THorseIocpWorker.TerminateWorker;
begin
  FRunning := False;
end;

procedure THorseIocpWorker.CloseConnection(AContext: TIocpConnectionContext);
var
  LSocketToClose: TSocket;
begin
  if AContext = nil then Exit;
  if InterlockedExchange(AContext.Closed, 1) = 0 then
  begin
    FConnectionsSync.Enter;
    try
      FConnections.Remove(AContext);
    finally
      FConnectionsSync.Leave;
    end;
    
    LSocketToClose := AContext.Socket;
    AContext.Socket := INVALID_SOCKET;
    if LSocketToClose <> INVALID_SOCKET then
      closesocket(LSocketToClose);

    if InterlockedDecrement(AContext.RefCount) = 0 then
      AContext.Free;
  end;
end;

procedure THorseIocpWorker.CheckTimeouts;
var
  LNow: Int64;
  I: Integer;
  LContext: TIocpConnectionContext;
  LExpired: TList<TIocpConnectionContext>;
begin
  LNow := GetTickCount64;
  LExpired := TList<TIocpConnectionContext>.Create;
  try
    FConnectionsSync.Enter;
    try
      for I := FConnections.Count - 1 downto 0 do
      begin
        LContext := FConnections[I];
        if LContext.Processing then Continue;
        if LNow - LContext.LastActive > 60000 then // Keep-Alive timeout de 60s
          LExpired.Add(LContext);
      end;
    finally
      FConnectionsSync.Leave;
    end;

    for I := 0 to LExpired.Count - 1 do
      CloseConnection(LExpired[I]);
  finally
    LExpired.Free;
  end;
end;

procedure THorseIocpWorker.PostAccept;
var
  LAcceptSocket: TSocket;
  LContext: TIocpConnectionContext;
  dwBytes: DWORD;
begin
  LAcceptSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if LAcceptSocket = INVALID_SOCKET then Exit;

  LContext := TIocpConnectionContext.Create(LAcceptSocket);
  FConnectionsSync.Enter;
  try
    FConnections.Add(LContext);
  finally
    FConnectionsSync.Leave;
  end;

  // Inicia AcceptEx assíncrono
  if not fnAcceptEx(FListenSocket, LAcceptSocket, @LContext.AcceptOverlapped.Buffer[0],
    0, SizeOf(TSockAddrIn) + 16, SizeOf(TSockAddrIn) + 16, dwBytes, @LContext.AcceptOverlapped.Overlapped) then
  begin
    if WSAGetLastError <> WSA_IO_PENDING then
      CloseConnection(LContext);
  end;
end;

procedure THorseIocpWorker.PostRead(AContext: TIocpConnectionContext);
begin
  THorseProviderIOCP.PostReadConnection(AContext);
end;

procedure THorseIocpWorker.ProcessClientRead(AContext: TIocpConnectionContext; ABytesTransferred: DWORD);
const
  { #16 — transport-level body cap. The header section is bounded separately
    (16 KB, parse-incomplete branch below); finer per-route body limits are the
    RequestGuard middleware's job. Generous so real uploads never trip it. }
  IOCP_MAX_BODY_SIZE = 16 * 1024 * 1024;
var
  LMethod, LPath, LQuery, LVersion: string;
  LHeaders: THeaderSegments;
  LBodyOffset, LNewLen: Integer;
  LContentLength: Int64;
  LIsChunked: Boolean;
  LRawReq: TIocpRawRequest;
  LRawRes: TIocpRawResponse;
  LReq: THorseRequest;
  LRes: THorseResponse;
  LData: PWorkItemData;
  {$IFNDEF FPC}
  LWebRequest: TInterfacedWebRequest;
  LWebResponse: TInterfacedWebResponse;
  {$ENDIF}
begin
  AContext.LastActive := GetTickCount64;

  // Copia dados recebidos para o buffer de acumulação da conexão
  LNewLen := AContext.BytesReceived + Integer(ABytesTransferred);

  if LNewLen > Length(AContext.Buffer) then
    SetLength(AContext.Buffer, LNewLen * 2);

  Move(AContext.ReadOverlapped.Buffer[0], AContext.Buffer[AContext.BytesReceived], ABytesTransferred);
  AContext.BytesReceived := LNewLen;

  // Tenta parsear a requisição
  if THorseHttpParser.TryParseRequest(AContext.Buffer, AContext.BytesReceived,
    LMethod, LPath, LQuery, LVersion, LHeaders, LBodyOffset, LContentLength, LIsChunked) then
  begin
    { #16 (2026-07-17, upstream-PR candidate) — headers parsed OK; a large body is
      now legitimate. The old `(not Processing) and (LNewLen > 16384)` guard above
      counted accumulated BODY bytes and closed the connection mid-upload on any
      body > 16 KB (test 16). Bound the body by an explicit cap instead. }
    if LContentLength > IOCP_MAX_BODY_SIZE then
    begin
      CloseConnection(AContext);
      Exit;
    end;

    { Fix G (re-derived onto merged 2026-07-17) — Transfer-Encoding: chunked
      request body (e.g. TCrossHttpClient multipart uploads). Wait for the
      terminating zero-size chunk, then de-chunk in place so the request sees a
      contiguous body with a known length. Without this the chunk bytes were
      mistaken for the start of the next request, poisoning the pooling client's
      keep-alive connection (test 13 timeout cascade on HorseIndyTestClient). }
    if LIsChunked then
    begin
      if not THorseHttpParser.TryDecodeChunkedBody(AContext.Buffer,
        AContext.BytesReceived, LBodyOffset, LContentLength) then
      begin
        PostRead(AContext);
        Exit;
      end;
    end
    // Se a requisição tem corpo (Content-Length > 0), aguarda o corpo inteiro.
    else if (LContentLength > 0) and (AContext.BytesReceived < LBodyOffset + LContentLength) then
    begin
      { #13 (2026-07-17) — re-arm HERE and return; ProcessClientRead is now the
        SOLE owner of re-arming. Previously the caller Execute() ALSO did
        `if not Processing then PostRead`, so an incomplete body left TWO overlapped
        WSARecv on the same buffer; when the close path later freed the context, the
        second completion dereferenced freed memory → worker crash / heap corruption
        → the keep-alive all-timeout cascade that wedged the server across runs.
        Owning the re-arm here also makes the CloseConnection paths safe: on close we
        simply do NOT PostRead, and Execute() no longer touches the (possibly freed)
        context afterwards. }
      PostRead(AContext);
      Exit;
    end;

    AContext.Processing := True;
    
    LRawReq := TIocpRawRequest.Create(
      AContext.Buffer,
      AContext.BytesReceived,
      LMethod,
      LPath,
      LQuery,
      LVersion,
      LHeaders,
      LBodyOffset,
      LContentLength,
      nil,
      '',
      AContext.ClientIP,
      AContext.ClientPort
    );
      
    LRawRes := TIocpRawResponse.Create(AContext, True);

    New(LData);
    LData.RawRes := LRawRes;

    {$IFDEF FPC}
    LReq := THorseRequest.Create(LRawReq);
    LRes := THorseResponse.Create(LRawRes);
    LData.Req := LReq;
    LData.Res := LRes;
    {$ELSE}
    LWebRequest := TInterfacedWebRequest.Create(LRawReq);
    LWebResponse := TInterfacedWebResponse.Create(LRawRes);
    
    LReq := THorseRequest.Create(LWebRequest);
    LRes := THorseResponse.Create(nil);
    LRes.SetCSRawWebResponse(LWebResponse);
    
    LData.Req := LReq;
    LData.Res := LRes;
    LData.WebRequest := LWebRequest;
    {$ENDIF}

    // Incrementa a referência do contexto antes de enfileirar no pool
    InterlockedIncrement(AContext.RefCount);

    // Despacha para o pool de threads assíncronas do Windows nativo
    if not QueueUserWorkItem(@IocpWorkItemCallback, LData, WT_EXECUTEDEFAULT) then
    begin
      InterlockedDecrement(AContext.RefCount);
      LReq.Free;
      LRes.Free;
      {$IFNDEF FPC}
      LWebRequest.Free;
      {$ENDIF}
      Dispose(LData);
      CloseConnection(AContext);
    end;
  end
  else
  begin
    { Header-size DoS cap — only while the header section is still incomplete
      (TryParseRequest failed). Previously the equivalent guard ran BEFORE the
      parse with a `not Processing` condition, so it also counted accumulated
      BODY bytes and closed >16 KB uploads (#16). Here it can only see an
      unfinished header block, which is what it was meant to bound. }
    if LNewLen > 16384 then
      CloseConnection(AContext)
    else
      { Headers still incomplete and within budget — re-arm for the rest
        (sole-owner re-arm; see #13 note above). }
      PostRead(AContext);
  end;
end;

procedure THorseIocpWorker.ProcessClientWrite(AContext: TIocpConnectionContext; ABytesTransferred: DWORD);
begin
  AContext.LastActive := GetTickCount64;
end;

procedure THorseIocpWorker.Execute;
var
  dwBytes: DWORD;
  dwKey: ULONG_PTR;
  pOverlap: POverlapped;
  LContext: TIocpConnectionContext;
  LOverlap: PIocpOverlapped;
  LResult: BOOL;
  LCurrentTime, LLastTimeoutCheck: Int64;
  LOptVal: Integer;
  { Fix C (re-derived onto merged 2026-07-17) — peer-address extraction }
  LLocalAddr, LRemoteAddr: PSockAddr;
  LLocalLen, LRemoteLen: Integer;
begin
  LLastTimeoutCheck := GetTickCount64;

  while FRunning do
  begin
    pOverlap := nil;
    LResult := GetQueuedCompletionStatus(FIocpHandle, dwBytes, dwKey, pOverlap, 500);

    if pOverlap <> nil then
    begin
      LOverlap := PIocpOverlapped(pOverlap);
      LContext := TIocpConnectionContext(LOverlap.Context);

      if LOverlap.OpType = ioAccept then
      begin
        // Atualiza o contexto do soquete aceito com as propriedades do soquete de escuta
        setsockopt(LOverlap.Socket, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT, PAnsiChar(@FListenSocket), SizeOf(FListenSocket));
        
        // Desativa Algoritmo de Nagle (TCP_NODELAY) para latência ultra-baixa em APIs
        LOptVal := 1;
        setsockopt(LOverlap.Socket, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@LOptVal), SizeOf(LOptVal));
        
        // Ativa TCP Keep-Alive em nível de SO para evitar soquetes órfãos/zumbis
        LOptVal := 1;
        setsockopt(LOverlap.Socket, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@LOptVal), SizeOf(LOptVal));

        { Fix C (re-derived onto merged 2026-07-17) — extract the remote IP from
          the AcceptEx output buffer into LContext.ClientIP. Merged IOCP resolves
          fnGetAcceptExSockaddrs but never calls it, so RemoteAddr was always
          empty (test 25). Upstream-PR candidate. }
        LLocalAddr := nil;
        LRemoteAddr := nil;
        LLocalLen := 0;
        LRemoteLen := 0;
        fnGetAcceptExSockaddrs(
          @LContext.AcceptOverlapped.Buffer[0],
          0,
          SizeOf(TSockAddrIn) + 16,
          SizeOf(TSockAddrIn) + 16,
          LLocalAddr, LLocalLen,
          LRemoteAddr, LRemoteLen);
        if LRemoteAddr <> nil then
          LContext.ClientIP := string(inet_ntoa(PSockAddrIn(LRemoteAddr)^.sin_addr));

        // Associa o soquete do cliente aceito ao Completion Port
        CreateIoCompletionPort(LOverlap.Socket, FIocpHandle, ULONG_PTR(LContext), 0);
        
        // Dispara a leitura inicial assíncrona usando WSARecv
        PostRead(LContext);
        
        // Posta o próximo accept assíncrono para fila do IOCP
        PostAccept;
      end
      else if LOverlap.OpType = ioRead then
      begin
        if (dwBytes = 0) or (not LResult) then
        begin
          CloseConnection(LContext);
        end
        else
        begin
          { ProcessClientRead is the sole owner of re-arming (#13): it PostReads
            itself when more of the request is still to come, and deliberately does
            NOT when it dispatched or closed the connection. Do not re-arm here — the
            old duplicate PostRead double-posted WSARecv and raced the close/free. }
          ProcessClientRead(LContext, dwBytes);
        end;
      end
      else if LOverlap.OpType = ioWrite then
      begin
        if (dwBytes = 0) or (not LResult) then
          CloseConnection(LContext)
        else
          ProcessClientWrite(LContext, dwBytes);
      end;
    end;

    LCurrentTime := GetTickCount64;
    if LCurrentTime - LLastTimeoutCheck > 1000 then
    begin
      CheckTimeouts;
      LLastTimeoutCheck := LCurrentTime;
    end;
  end;
end;

{ THorseProviderIOCP }

class constructor THorseProviderIOCP.CreateClass;
begin
  FPort := GetDefaultPort;
  FHost := GetDefaultHost;
  FWorkers := TObjectList<THorseIocpWorker>.Create(True);
  FRunning := False;
end;

class destructor THorseProviderIOCP.DestroyClass;
begin
  FWorkers.Free;
end;

class procedure THorseProviderIOCP.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

class procedure THorseProviderIOCP.SetHost(const AValue: string);
begin
  FHost := AValue;
end;

class function THorseProviderIOCP.GetPort: Integer;
begin
  Result := FPort;
end;

class function THorseProviderIOCP.GetHost: string;
begin
  Result := FHost;
end;

class function THorseProviderIOCP.GetDefaultPort: Integer;
begin
  Result := 9000;
end;

class function THorseProviderIOCP.GetDefaultHost: string;
begin
  Result := '0.0.0.0';
end;

class function THorseProviderIOCP.CreateListenSocket(const APort: Integer; const AHost: string): TSocket;
var
  LAddr: TSockAddrIn;
  wsaData: TWSAData;
  LOpt: Integer;
begin
  WSAStartup(MakeWord(2, 2), wsaData);

  Result := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
  if Result = INVALID_SOCKET then
    raise Exception.Create('Falha ao criar o socket de escuta (WSASocket).');

  LOpt := 1;
  setsockopt(Result, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@LOpt), SizeOf(LOpt));

  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(APort);
  if (AHost = '') or (AHost = '0.0.0.0') then
    LAddr.sin_addr.S_addr := INADDR_ANY
  else
    LAddr.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(AHost)));

  if bind(Result, PSockAddr(@LAddr)^, SizeOf(LAddr)) = SOCKET_ERROR then
  begin
    closesocket(Result);
    raise Exception.Create('Falha ao associar a porta (bind).');
  end;

  {$IF DEFINED(FPC)}
  if WinSock2.listen(Result, SOMAXCONN) = SOCKET_ERROR then
  {$ELSE}
  if Winapi.WinSock2.listen(Result, SOMAXCONN) = SOCKET_ERROR then
  {$ENDIF}
  begin
    closesocket(Result);
    raise Exception.Create('Falha ao escutar a porta (listen).');
  end;
end;

class function THorseProviderIOCP.GetActivePort: Integer;
begin
  Result := FPort;
end;

class procedure THorseProviderIOCP.InternalListen;
const
  GuidAcceptEx: TGUID = '{B5367DF1-CBAC-11CF-95CA-00805F48A192}';
  GuidGetAcceptExSockaddrs: TGUID = '{B5367DF2-CBAC-11CF-95CA-00805F48A192}';
var
  LCPUCount, I: Integer;
  LWorker: THorseIocpWorker;
  LPtr: Pointer;
begin
  TriggerBeforeListen;
  FListenSocket := CreateListenSocket(FPort, FHost);
  
  // Obtém ponteiros das extensões do Winsock
  if GetWSAExtensionPointer(FListenSocket, GuidAcceptEx, LPtr) then
    fnAcceptEx := LPtr
  else
    raise Exception.Create('Falha ao obter funcao AcceptEx.');
    
  if GetWSAExtensionPointer(FListenSocket, GuidGetAcceptExSockaddrs, LPtr) then
    fnGetAcceptExSockaddrs := LPtr
  else
    raise Exception.Create('Falha ao obter funcao GetAcceptExSockaddrs.');

  // Cria Completion Port associando o Listen Socket
  FIocpHandle := CreateIoCompletionPort(FListenSocket, 0, 0, 0);
  if FIocpHandle = 0 then
    raise Exception.Create('Falha ao criar o Completion Port.');

  FRunning := True;
  LCPUCount := CPUCount;
  
  // Cria os workers
  for I := 1 to LCPUCount do
  begin
    LWorker := THorseIocpWorker.Create(FListenSocket, FIocpHandle);
    FWorkers.Add(LWorker);
    
    // Inicia os primeiros aceites assíncronos no worker
    LWorker.PostAccept;
  end;
end;

class procedure THorseProviderIOCP.InternalStopListen;
var
  I: Integer;
begin
  TriggerBeforeStop;
  FRunning := False;
  
  for I := 0 to FWorkers.Count - 1 do
    FWorkers[I].TerminateWorker;

  if FIocpHandle <> 0 then
  begin
    CloseHandle(FIocpHandle);
    FIocpHandle := 0;
  end;

  if FListenSocket <> INVALID_SOCKET then
  begin
    closesocket(FListenSocket);
    FListenSocket := INVALID_SOCKET;
  end;

  FWorkers.Clear;
  WSACleanup;
end;

class procedure THorseProviderIOCP.InternalListenLoop(const ACallbackListen, ACallbackStopListen: Horse.Proc.TProc);
begin
  InternalListen;
  DoOnListen;
  while FRunning do
    Sleep(100);
  DoOnStopListen;
end;

class procedure THorseProviderIOCP.PostReadConnection(AContext: TIocpConnectionContext);
var
  LWSABuf: TWSABuf;
  dwFlags, dwBytes: DWORD;
begin
  FillChar(AContext.ReadOverlapped.Overlapped, SizeOf(OVERLAPPED), 0);
  LWSABuf.len := 16384;
  LWSABuf.buf := PAnsiChar(@AContext.ReadOverlapped.Buffer[0]);
  dwFlags := 0;
  dwBytes := 0;
  
  if WSARecv(AContext.Socket, @LWSABuf, 1, dwBytes, dwFlags, @AContext.ReadOverlapped.Overlapped, nil) = SOCKET_ERROR then
  begin
    if WSAGetLastError() <> WSA_IO_PENDING then
    begin
      closesocket(AContext.Socket);
      AContext.Socket := INVALID_SOCKET;
    end;
  end;
end;

class procedure THorseProviderIOCP.Listen;
begin
  InternalListenLoop(nil, nil);
end;

class procedure THorseProviderIOCP.Listen(const APort: Integer; const AHost: string; const ACallbackListen: Horse.Proc.TProc; const ACallbackStopListen: Horse.Proc.TProc);
begin
  FPort := APort;
  FHost := AHost;
  InternalListenLoop(ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProviderIOCP.Listen(const APort: Integer; const ACallbackListen: Horse.Proc.TProc; const ACallbackStopListen: Horse.Proc.TProc);
begin
  FPort := APort;
  InternalListenLoop(ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProviderIOCP.Listen(const AHost: string; const ACallbackListen: Horse.Proc.TProc; const ACallbackStopListen: Horse.Proc.TProc);
begin
  FHost := AHost;
  InternalListenLoop(ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProviderIOCP.Listen(const ACallbackListen: Horse.Proc.TProc; const ACallbackStopListen: Horse.Proc.TProc);
begin
  InternalListenLoop(ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProviderIOCP.ListenWithConfig(const APort: Integer; const AConfig: THorseCrossSocketConfig);
begin
  FPort := APort;
  THorseProviderIOCP.Listen(Horse.Proc.TProc(nil), Horse.Proc.TProc(nil));
end;

class procedure THorseProviderIOCP.StopListen;
begin
  InternalStopListen;
end;

class function THorseProviderIOCP.IsRunning: Boolean;
begin
  Result := FRunning;
end;

{ TIocpStreamWriter }

type
  TIocpStreamWriter = class(THorseStreamWriterBase)
  private
    FRawRes: TIocpRawResponse;
    procedure SendFully(const AData: TBytes; ALen: Integer);
  protected
    procedure SendRawHeaders; override;
    procedure WriteRawBytes(const ABytes: TBytes); override;
  public
    constructor Create(const AResponse: THorseResponse); override;
    function IsConnected: Boolean; override;
  end;

constructor TIocpStreamWriter.Create(const AResponse: THorseResponse);
var
  LRawWebResponse: TObject;
begin
  inherited Create(AResponse);
  LRawWebResponse := AResponse.RawWebResponse;
  if Assigned(LRawWebResponse) and (LRawWebResponse is TInterfacedWebResponse) then
    FRawRes := TIocpRawResponse(TInterfacedWebResponse(LRawWebResponse).RawRes);
end;

procedure TIocpStreamWriter.SendFully(const AData: TBytes; ALen: Integer);
var
  LTotal, LRet: Integer;
begin
  { IOCP-STREAM-SENDFULL-1 (2026-07-18) — a single blocking send() may return a
    PARTIAL count (documented in this environment; the fork's [IOCP-STREAM-1] looped
    for the same reason). Merged did one send() and ignored the result, so under
    concurrent streaming load a truncated chunk corrupted the response, leaving the
    just-streamed keep-alive connection unusable on reuse (test 36). Loop until every
    byte is sent; stop on error/close (IsConnected then reports false). }
  if not Assigned(FRawRes) then Exit;
  LTotal := 0;
  while LTotal < ALen do
  begin
    if FRawRes.FContext.Socket = INVALID_SOCKET then Exit;
    LRet := send(FRawRes.FContext.Socket, AData[LTotal], ALen - LTotal, 0);
    if LRet <= 0 then
      Exit;
    Inc(LTotal, LRet);
  end;
end;

procedure TIocpStreamWriter.SendRawHeaders;
var
  LHeaderBytes: TBytes;
  LHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF};
  LPair: TPair<string, string>;
  {$IFDEF FPC}
  I: Integer;
  {$ENDIF}
begin
  if not Assigned(FRawRes) then Exit;

  LHeadersList := FResponse.CustomHeaders;
  if LHeadersList <> nil then
  begin
    {$IFDEF FPC}
    for I := 0 to LHeadersList.Count - 1 do
      FRawRes.FHeaders.AddOrSetValue(LHeadersList.Names[I], LHeadersList.ValueFromIndex[I]);
    {$ELSE}
    for LPair in LHeadersList do
      FRawRes.FHeaders.AddOrSetValue(LPair.Key, LPair.Value);
    {$ENDIF}
  end;

  { Fix A (streaming, re-derived onto merged 2026-07-17) — honour the
    Res.ContentType(...) the handler set before SendStream. Like the non-stream
    SendResponse path, SendRawHeaders only copies CustomHeaders and never reads
    the CSContentType shadow, so a streamed response defaulted to text/plain
    (test 34). Transfer-Encoding: chunked flows separately (base writer). }
  if (FResponse.CSContentType <> '') and not FRawRes.FHeaders.ContainsKey('Content-Type') then
    FRawRes.FHeaders.AddOrSetValue('Content-Type', FResponse.CSContentType);

  FRawRes.FStatusCode := FResponse.Status;
  if FRawRes.FStatusCode = 200 then FRawRes.FStatusReason := 'OK'
  else if FRawRes.FStatusCode = 204 then FRawRes.FStatusReason := 'No Content'
  else if FRawRes.FStatusCode = 404 then FRawRes.FStatusReason := 'Not Found'
  else if FRawRes.FStatusCode = 500 then FRawRes.FStatusReason := 'Internal Server Error'
  else FRawRes.FStatusReason := 'HTTP Response';

  LHeaderBytes := FRawRes.PrepareHeaders;
  if Length(LHeaderBytes) > 0 then
    SendFully(LHeaderBytes, Length(LHeaderBytes));
    
  FRawRes.FHeadersSent := True;
end;

procedure TIocpStreamWriter.WriteRawBytes(const ABytes: TBytes);
begin
  if Length(ABytes) = 0 then Exit;
  SendFully(ABytes, Length(ABytes));
end;

function TIocpStreamWriter.IsConnected: Boolean;
begin
  Result := Assigned(FRawRes) and (FRawRes.FContext.Socket <> INVALID_SOCKET);
end;

function IocpStreamWriterFactory(const AResponse: THorseResponse): IHorseStreamWriter;
begin
  Result := TIocpStreamWriter.Create(AResponse);
end;

initialization
  THorseResponse.RegisterStreamWriterFactory(IocpStreamWriterFactory);

{$ENDIF}

end.
