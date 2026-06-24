unit Horse.Provider.HttpSys;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IFDEF MSWINDOWS}
uses
  {$IF DEFINED(FPC)}
    SysUtils,
    Classes,
    SyncObjs,
    Generics.Collections,
    Generics.Defaults,
    Windows,
    httpprotocol,
  {$ELSE}
    System.SysUtils,
    System.Classes,
    System.SyncObjs,
    System.Threading,
    System.NetEncoding,
    System.Generics.Collections,
    System.Generics.Defaults,
    Winapi.Windows,
  {$ENDIF}
  Horse.Provider.Abstract,
  Horse.Provider.Config,
  Horse.Request,
  Horse.Response,
  Horse.Provider.RawInterfaces,
  Horse.Provider.RawAdapters,
  Horse.Proc,
  Horse.Commons;

const
  HTTPAPI_DLL = 'httpapi.dll';

  HTTP_INITIALIZE_SERVER = $00000001;

  // HttpSendHttpResponse flags
  HTTP_SEND_RESPONSE_FLAG_DISCONNECT  = $00000001;
  HTTP_SEND_RESPONSE_FLAG_MORE_DATA   = $00000002;

  // Request flags
  HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS = $00000001;

{$MINENUMSIZE 4}

type
  ULONG = Cardinal;
  USHORT = Word;
  UCHAR = Byte;
  PUCHAR = ^Byte;
  HTTP_OPAQUE_ID = UInt64;
  HTTP_REQUEST_ID = HTTP_OPAQUE_ID;
  HTTP_CONNECTION_ID = HTTP_OPAQUE_ID;
  HTTP_RAW_CONNECTION_ID = HTTP_OPAQUE_ID;
  HTTP_URL_GROUP_ID = HTTP_OPAQUE_ID;
  HTTP_SERVER_SESSION_ID = HTTP_OPAQUE_ID;
  HTTP_URL_CONTEXT = HTTP_OPAQUE_ID;

  HTTPAPI_VERSION = record
    HttpApiMajorVersion: USHORT;
    HttpApiMinorVersion: USHORT;
  end;

  HTTP_VERSION = record
    MajorVersion: USHORT;
    MinorVersion: USHORT;
  end;

  THttpVerb = (
    HttpVerbUnparsed,
    HttpVerbUnknown,
    HttpVerbInvalid,
    HttpVerbOPTIONS,
    HttpVerbGET,
    HttpVerbHEAD,
    HttpVerbPOST,
    HttpVerbPUT,
    HttpVerbDELETE,
    HttpVerbTRACE,
    HttpVerbCONNECT,
    HttpVerbTRACK,
    HttpVerbMOVE,
    HttpVerbCOPY,
    HttpVerbPROPFIND,
    HttpVerbPROPPATCH,
    HttpVerbMKCOL,
    HttpVerbLOCK,
    HttpVerbUNLOCK,
    HttpVerbSEARCH,
    HttpVerbMaximum
  );

  HTTP_COOKED_URL = record
    FullUrlLength: USHORT;
    HostLength: USHORT;
    AbsPathLength: USHORT;
    QueryStringLength: USHORT;
    pFullUrl: PWideChar;
    pHost: PWideChar;
    pAbsPath: PWideChar;
    pQueryString: PWideChar;
  end;

  HTTP_TRANSPORT_ADDRESS = record
    pRemoteAddress: Pointer;
    pLocalAddress: Pointer;
  end;

  HTTP_UNKNOWN_HEADER = record
    NameLength: USHORT;
    RawValueLength: USHORT;
    pName: PAnsiChar;
    pRawValue: PAnsiChar;
  end;
  PHTTP_UNKNOWN_HEADER = ^HTTP_UNKNOWN_HEADER;
  THTTP_UNKNOWN_HEADER_ARRAY = array[0..65535] of HTTP_UNKNOWN_HEADER;
  PHTTP_UNKNOWN_HEADER_ARRAY = ^THTTP_UNKNOWN_HEADER_ARRAY;

  HTTP_KNOWN_HEADER = record
    RawValueLength: USHORT;
    pRawValue: PAnsiChar;
  end;

  HTTP_REQUEST_HEADERS = record
    UnknownHeaderCount: USHORT;
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    TrailerCount: USHORT;
    pTrailers: Pointer;
    KnownHeaders: array[0..40] of HTTP_KNOWN_HEADER;
  end;

  HTTP_RESPONSE_HEADERS = record
    UnknownHeaderCount: USHORT;
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    TrailerCount: USHORT;
    pTrailers: Pointer;
    KnownHeaders: array[0..29] of HTTP_KNOWN_HEADER;
  end;

  HTTP_SSL_INFO = record
    ServerCertKeySize: USHORT;
    ConnectionKeySize: USHORT;
    ServerCertIssuerSize: ULONG;
    ServerCertSubjectSize: ULONG;
    pServerCertIssuer: PAnsiChar;
    pServerCertSubject: PAnsiChar;
    pClientCertInfo: Pointer;
    SslClientCertNegotiated: ULONG;
  end;
  PHTTP_SSL_INFO = ^HTTP_SSL_INFO;

  HTTP_REQUEST = record
    Flags: ULONG;
    ConnectionId: HTTP_CONNECTION_ID;
    RequestId: HTTP_REQUEST_ID;
    UrlContext: HTTP_URL_CONTEXT;
    Version: HTTP_VERSION;
    Verb: THttpVerb;
    UnknownVerbLength: USHORT;
    RawUrlLength: USHORT;
    pUnknownVerb: PAnsiChar;
    pRawUrl: PAnsiChar;
    CookedUrl: HTTP_COOKED_URL;
    Address: HTTP_TRANSPORT_ADDRESS;
    Headers: HTTP_REQUEST_HEADERS;
    BytesReceived: UInt64;
    EntityChunkCount: USHORT;
    pEntityChunks: Pointer;
    RawConnectionId: HTTP_RAW_CONNECTION_ID;
    pSslInfo: PHTTP_SSL_INFO;
    RequestInfoCount: USHORT;
    pRequestInfo: Pointer;
  end;
  PHTTP_REQUEST = ^HTTP_REQUEST;

  THttpChunkType = (
    hctFromMemory,
    hctFromFileHandle,
    hctFromFragmentCache
  );

  HTTP_DATA_CHUNK_INMEMORY = record
    DataChunkType: THttpChunkType;
    Reserved1: ULONG;
    pBuffer: Pointer;
    BufferLength: ULONG;
    {$IFDEF CPUX64}
    Reserved2: array[0..2] of ULONG;
    {$ELSE}
    Reserved2: array[0..3] of ULONG;
    {$ENDIF}
  end;
  PHTTP_DATA_CHUNK_INMEMORY = ^HTTP_DATA_CHUNK_INMEMORY;

  HTTP_RESPONSE = record
    Flags: ULONG;
    Version: HTTP_VERSION;
    StatusCode: USHORT;
    ReasonLength: USHORT;
    pReason: PAnsiChar;
    Headers: HTTP_RESPONSE_HEADERS;
    EntityChunkCount: USHORT;
    pEntityChunks: Pointer;
    ResponseInfoCount: USHORT;
    pResponseInfo: Pointer;
  end;
  PHTTP_RESPONSE = ^HTTP_RESPONSE;

  HTTP_SERVER_PROPERTY = (
    HttpServerAuthenticationProperty,
    HttpServerLoggingProperty,
    HttpServerQosProperty,
    HttpServerTimeoutsProperty,
    HttpServerQueueLengthProperty,
    HttpServerStateProperty,
    HttpServer503VerbosityProperty,
    HttpServerBindingProperty
  );

  HTTP_BINDING_INFO = record
    Flags: ULONG;
    RequestQueueHandle: THandle;
  end;
  PHTTP_BINDING_INFO = ^HTTP_BINDING_INFO;

  TSockAddrIn = record
    sin_family: Word;
    sin_port: Word;
    sin_addr: array[0..3] of Byte;
    sin_zero: array[0..7] of Byte;
  end;
  PSockAddrIn = ^TSockAddrIn;

const
  HTTPAPI_VERSION_2: HTTPAPI_VERSION = (HttpApiMajorVersion: 2; HttpApiMinorVersion: 0);

  HTTP_KNOWN_REQUEST_HEADERS: array[0..40] of string = (
    'Cache-Control', 'Connection', 'Date', 'Keep-Alive', 'Pragma', 'Trailer',
    'Transfer-Encoding', 'Upgrade', 'Via', 'Warning', 'Allow', 'Content-Length',
    'Content-Type', 'Content-Encoding', 'Content-Language', 'Content-Location',
    'Content-MD5', 'Content-Range', 'Expires', 'Last-Modified', 'Accept',
    'Accept-Charset', 'Accept-Encoding', 'Accept-Language', 'Authorization',
    'Cookie', 'Expect', 'From', 'Host', 'If-Match', 'If-Modified-Since',
    'If-None-Match', 'If-Range', 'If-Unmodified-Since', 'Max-Forwards',
    'Proxy-Authorization', 'Referer', 'Range', 'TE', 'Translate', 'User-Agent'
  );

  HTTP_KNOWN_RESPONSE_HEADERS: array[0..29] of string = (
    'Cache-Control', 'Connection', 'Date', 'Keep-Alive', 'Pragma', 'Trailer',
    'Transfer-Encoding', 'Upgrade', 'Via', 'Warning', 'Allow', 'Content-Length',
    'Content-Type', 'Content-Encoding', 'Content-Language', 'Content-Location',
    'Content-MD5', 'Content-Range', 'Expires', 'Last-Modified', 'Accept-Ranges',
    'Age', 'ETag', 'Location', 'Proxy-Authenticate', 'Retry-After', 'Server',
    'Set-Cookie', 'Vary', 'Www-Authenticate'
  );

// Windows Http.sys native APIs
function HttpInitialize(Version: HTTPAPI_VERSION; Flags: ULONG; pReserved: Pointer): ULONG; stdcall; external HTTPAPI_DLL;
function HttpTerminate(Flags: ULONG; pReserved: Pointer): ULONG; stdcall; external HTTPAPI_DLL;
function HttpCreateServerSession(Version: HTTPAPI_VERSION; var ServerSessionId: HTTP_SERVER_SESSION_ID; Reserved: ULONG): ULONG; stdcall; external HTTPAPI_DLL;
function HttpCloseServerSession(ServerSessionId: HTTP_SERVER_SESSION_ID): ULONG; stdcall; external HTTPAPI_DLL;
function HttpCreateUrlGroup(ServerSessionId: HTTP_SERVER_SESSION_ID; var UrlGroupId: HTTP_URL_GROUP_ID; Reserved: ULONG): ULONG; stdcall; external HTTPAPI_DLL;
function HttpCloseUrlGroup(UrlGroupId: HTTP_URL_GROUP_ID): ULONG; stdcall; external HTTPAPI_DLL;
function HttpAddUrlToUrlGroup(UrlGroupId: HTTP_URL_GROUP_ID; pFullyQualifiedUrl: PWideChar; UrlContext: HTTP_URL_CONTEXT; Reserved: ULONG): ULONG; stdcall; external HTTPAPI_DLL;
function HttpRemoveUrlFromUrlGroup(UrlGroupId: HTTP_URL_GROUP_ID; pFullyQualifiedUrl: PWideChar; Flags: ULONG): ULONG; stdcall; external HTTPAPI_DLL;
function HttpCreateRequestQueue(Version: HTTPAPI_VERSION; pName: PWideChar; pSecurityAttributes: Pointer; Flags: ULONG; var ReqQueueHandle: THandle): ULONG; stdcall; external HTTPAPI_DLL;
function HttpCloseRequestQueue(ReqQueueHandle: THandle): ULONG; stdcall; external HTTPAPI_DLL;
function HttpReceiveHttpRequest(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID; Flags: ULONG; pRequestBuffer: PHTTP_REQUEST; RequestBufferLength: ULONG; var BytesReturned: ULONG; pOverlapped: POverlapped): ULONG; stdcall; external HTTPAPI_DLL;
function HttpReceiveRequestEntityBody(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID; Flags: ULONG; pBuffer: Pointer; BufferLength: ULONG; var BytesReceived: ULONG; pOverlapped: POverlapped): ULONG; stdcall; external HTTPAPI_DLL;
function HttpSendHttpResponse(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID; Flags: ULONG; pHttpResponse: PHTTP_RESPONSE; pReserved1: Pointer; var BytesSent: ULONG; pReserved2: Pointer; Reserved3: ULONG; pOverlapped: POverlapped; pLogData: Pointer): ULONG; stdcall; external HTTPAPI_DLL;
function HttpSendResponseEntityBody(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID; Flags: ULONG; EntityChunkCount: USHORT; pEntityChunks: Pointer; var BytesSent: ULONG; pReserved1: Pointer; pReserved2: Pointer; pOverlapped: POverlapped; pLogData: Pointer): ULONG; stdcall; external HTTPAPI_DLL;
function HttpSetUrlGroupProperty(UrlGroupId: HTTP_URL_GROUP_ID; PropertyId: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer; PropertyInformationLength: ULONG): ULONG; stdcall; external HTTPAPI_DLL;

{$MINENUMSIZE 1}

type
  THttpSysListenerThread = class;

  {$IF DEFINED(FPC)}
  THttpSysRequestThread = class(TThread)
  private
    FReqQueue: THandle;
    FBuffer: TBytes;
  protected
    procedure Execute; override;
  public
    constructor Create(AReqQueue: THandle; ABuffer: TBytes);
  end;
  {$ENDIF}

  // IHorseRawRequest implementation
  THttpSysRawRequest = class(TInterfacedObject, IHorseRawRequest)
  private
    FRequest: PHTTP_REQUEST;
    FBuffer: TBytes;
    FBodyStream: TMemoryStream;
    procedure EnsureBodyStream;
  public
    constructor Create(ARequest: PHTTP_REQUEST; const ABuffer: TBytes);
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
    {$IF DEFINED(FPC)}
    function GetContentLength: Integer;
    {$ELSEIF CompilerVersion >= 32.0}
    function GetContentLength: Int64;
    {$ELSE}
    function GetContentLength: Integer;
    {$IFEND}
    function GetFieldByName(const AName: string): string;

    procedure PopulateQueryFields(ADest: TStrings);
    procedure PopulateContentFields(ADest: TStrings);
    procedure PopulateCookieFields(ADest: TStrings);

    function ReadBody(var Buffer; Count: Integer): Integer;
  end;

  // IHorseRawResponse implementation
  THttpSysRawResponse = class(TInterfacedObject, IHorseRawResponse)
  private
    FReqQueue: THandle;
    FRequestId: HTTP_REQUEST_ID;
  public
    constructor Create(AReqQueue: THandle; ARequestId: HTTP_REQUEST_ID);
    procedure SetCustomHeader(const AName, AValue: string);
    procedure SendResponse(const ARes: THorseResponse);
  end;

  THttpSysWebResponse = class(TInterfacedWebResponse)
  {$IFNDEF FPC}
  private
    FStatusCode: Integer;
    FContent: string;
    FContentType: string;
    FContentEncoding: string;
  protected
    function  GetStringVariable(Index: Integer): string; override;
    procedure SetStringVariable(Index: Integer; const Value: string); override;
{$IF CompilerVersion >= 32.0}
    function  GetIntegerVariable(Index: Integer): Int64; override;
    procedure SetIntegerVariable(Index: Integer; Value: Int64); override;
{$ELSE}
    function  GetIntegerVariable(Index: Integer): Integer; override;
    procedure SetIntegerVariable(Index: Integer; Value: Integer); override;
{$IFEND}
    function  GetContent: string; override;
    procedure SetContent(const Value: string); override;
    function  GetStatusCode: Integer; override;
    procedure SetStatusCode(Value: Integer); override;
  {$ENDIF}
  public
    constructor Create(const ARawRes: IHorseRawResponse); reintroduce;
  end;

  // Listener Thread
  THttpSysListenerThread = class(TThread)
  private
    FReqQueue: THandle;
    FRunning: Boolean;
    procedure DispatchRequest(ABuffer: TBytes);
  protected
    procedure Execute; override;
  public
    constructor Create(AReqQueue: THandle);
    property Running: Boolean read FRunning write FRunning;
  end;

  // Concrete THorseProvider for HTTP.sys
  THorseProviderHttpSys = class(THorseProviderAbstract)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FServerSessionId: HTTP_SERVER_SESSION_ID;
    class var FUrlGroupId: HTTP_URL_GROUP_ID;
    class var FReqQueue: THandle;
    class var FListenerThread: THttpSysListenerThread;
    class var FKnownRequestHeadersMap: TDictionary<string, Integer>;
    class var FKnownResponseHeadersMap: TDictionary<string, Integer>;

    class procedure SetPort(const AValue: Integer); static;
    class procedure SetHost(const AValue: string); static;
    class function GetPort: Integer; static;
    class function GetHost: string; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;

    class procedure InternalListen;
    class procedure InternalStopListen;
  public
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = 'localhost'; const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallbackListen: TProc; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: TProc; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure ListenWithConfig(const APort: Integer; const AConfig: THorseCrossSocketConfig); override;
    class procedure StopListen; override;
    class function IsRunning: Boolean;

    class property ServerSessionId: HTTP_SERVER_SESSION_ID read FServerSessionId;
    class property UrlGroupId: HTTP_URL_GROUP_ID read FUrlGroupId;
    class property ReqQueue: THandle read FReqQueue;
    class property KnownRequestHeadersMap: TDictionary<string, Integer> read FKnownRequestHeadersMap;
    class property KnownResponseHeadersMap: TDictionary<string, Integer> read FKnownResponseHeadersMap;

    class constructor CreateClass;
    class destructor DestroyClass;
  end;

  THorseProvider = class(THorseProviderHttpSys);

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

{$IF DEFINED(FPC)}
{ THttpSysRequestThread }

constructor THttpSysRequestThread.Create(AReqQueue: THandle; ABuffer: TBytes);
begin
  inherited Create(False);
  FReqQueue := AReqQueue;
  FBuffer := ABuffer;
  FreeOnTerminate := True;
end;

procedure THttpSysRequestThread.Execute;
var
  LRawReq: IHorseRawRequest;
  LRawRes: IHorseRawResponse;
  LConcreteRes: THttpSysRawResponse;
  LWebRequest: TInterfacedWebRequest;
  LWebResponse: TInterfacedWebResponse;
  LReq: THorseRequest;
  LRes: THorseResponse;
  LRequest: PHTTP_REQUEST;
begin
  try
    LRequest := PHTTP_REQUEST(@FBuffer[0]);
    LRawReq := THttpSysRawRequest.Create(LRequest, FBuffer);
    LConcreteRes := THttpSysRawResponse.Create(FReqQueue, LRequest.RequestId);
    LRawRes := LConcreteRes;
    LWebRequest := TInterfacedWebRequest.Create(LRawReq);
    LWebResponse := THttpSysWebResponse.Create(LRawRes);
    try
      LReq := THorseRequest.Create(LWebRequest);
      LRes := THorseResponse.Create(LWebResponse);
      try
        THorseProviderHttpSys.Execute(LReq, LRes);
      finally
        LConcreteRes.SendResponse(LRes);
        LReq.Free;
        LRes.Free;
      end;
    finally
      LWebRequest.Free;
      LWebResponse.Free;
    end;
  except
    on E: Exception do
      // Silencia erros de Dispatch
      ;
  end;
end;
{$ENDIF}

{ THttpSysRawRequest }

constructor THttpSysRawRequest.Create(ARequest: PHTTP_REQUEST; const ABuffer: TBytes);
begin
  inherited Create;
  FRequest := ARequest;
  FBuffer := ABuffer;
  FBodyStream := nil;
end;

destructor THttpSysRawRequest.Destroy;
begin
  if Assigned(FBodyStream) then
    FBodyStream.Free;
  inherited;
end;

procedure THttpSysRawRequest.EnsureBodyStream;
var
  PChunk: PHTTP_DATA_CHUNK_INMEMORY;
  I: Integer;
  BytesReceived: ULONG;
  Ret: ULONG;
  TempBuf: TBytes;
begin
  if FBodyStream <> nil then Exit;

  FBodyStream := TMemoryStream.Create;
  
  // 1. Copy initial body chunk if any
  if (FRequest.EntityChunkCount > 0) and (FRequest.pEntityChunks <> nil) then
  begin
    PChunk := PHTTP_DATA_CHUNK_INMEMORY(FRequest.pEntityChunks);
    for I := 0 to FRequest.EntityChunkCount - 1 do
    begin
      if PChunk.DataChunkType = hctFromMemory then
      begin
        if (PChunk.pBuffer <> nil) and (PChunk.BufferLength > 0) then
          FBodyStream.WriteBuffer(PChunk.pBuffer^, PChunk.BufferLength);
      end;
      Inc(PChunk);
    end;
  end;

  // 2. Retrieve remaining request body entity chunks from HTTP.sys queue
  if (FRequest.Flags and HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS) <> 0 then
  begin
    SetLength(TempBuf, 32768);
    while True do
    begin
      BytesReceived := 0;
      Ret := HttpReceiveRequestEntityBody(
        THorseProviderHttpSys.ReqQueue,
        FRequest.RequestId,
        0,
        @TempBuf[0],
        Length(TempBuf),
        BytesReceived,
        nil
      );
      
      if Ret = ERROR_SUCCESS then
      begin
        if BytesReceived > 0 then
          FBodyStream.WriteBuffer(TempBuf[0], BytesReceived)
        else
          Break;
      end
      else if Ret = ERROR_HANDLE_EOF then
      begin
        if BytesReceived > 0 then
          FBodyStream.WriteBuffer(TempBuf[0], BytesReceived);
        Break;
      end
      else
        Break;
    end;
  end;

  FBodyStream.Position := 0;
end;

function THttpSysRawRequest.GetMethod: string;
begin
  case FRequest.Verb of
    HttpVerbGET: Result := 'GET';
    HttpVerbPOST: Result := 'POST';
    HttpVerbPUT: Result := 'PUT';
    HttpVerbDELETE: Result := 'DELETE';
    HttpVerbOPTIONS: Result := 'OPTIONS';
    HttpVerbHEAD: Result := 'HEAD';
    HttpVerbTRACE: Result := 'TRACE';
    HttpVerbCONNECT: Result := 'CONNECT';
  else
    if FRequest.pUnknownVerb <> nil then
      SetString(Result, PAnsiChar(FRequest.pUnknownVerb), FRequest.UnknownVerbLength)
    else
      Result := 'GET';
  end;
end;

function THttpSysRawRequest.GetProtocolVersion: string;
begin
  Result := Format('HTTP/%d.%d', [FRequest.Version.MajorVersion, FRequest.Version.MinorVersion]);
end;

function THttpSysRawRequest.GetURL: string;
begin
  if FRequest.pRawUrl <> nil then
    SetString(Result, PAnsiChar(FRequest.pRawUrl), FRequest.RawUrlLength)
  else
    Result := '/';
end;

function THttpSysRawRequest.GetPathInfo: string;
begin
  if FRequest.CookedUrl.pAbsPath <> nil then
    SetString(Result, FRequest.CookedUrl.pAbsPath, FRequest.CookedUrl.AbsPathLength div SizeOf(WideChar))
  else
    Result := '/';
end;

function THttpSysRawRequest.GetQueryString: string;
begin
  if FRequest.CookedUrl.pQueryString <> nil then
    SetString(Result, FRequest.CookedUrl.pQueryString, FRequest.CookedUrl.QueryStringLength div SizeOf(WideChar))
  else
    Result := '';
end;

function THttpSysRawRequest.GetHost: string;
begin
  Result := GetFieldByName('Host');
  if (Result = '') and (FRequest.CookedUrl.pHost <> nil) then
    SetString(Result, FRequest.CookedUrl.pHost, FRequest.CookedUrl.HostLength div SizeOf(WideChar));
end;

function THttpSysRawRequest.GetRemoteAddr: string;
var
  LFamily: Word;
  LIPv4: PSockAddrIn;
begin
  if FRequest.Address.pRemoteAddress = nil then
    Exit('127.0.0.1');

  LFamily := PWord(FRequest.Address.pRemoteAddress)^;
  if LFamily = 2 then // AF_INET
  begin
    LIPv4 := PSockAddrIn(FRequest.Address.pRemoteAddress);
    Result := Format('%d.%d.%d.%d', [LIPv4.sin_addr[0], LIPv4.sin_addr[1], LIPv4.sin_addr[2], LIPv4.sin_addr[3]]);
  end
  else
    Result := '127.0.0.1'; // Fallback
end;

function THttpSysRawRequest.GetServerPort: Integer;
var
  LFamily: Word;
  LIPv4: PSockAddrIn;
begin
  if FRequest.Address.pLocalAddress = nil then
    Exit(80);

  LFamily := PWord(FRequest.Address.pLocalAddress)^;
  if LFamily = 2 then // AF_INET
  begin
    LIPv4 := PSockAddrIn(FRequest.Address.pLocalAddress);
    Result := (LIPv4.sin_port shl 8) or (LIPv4.sin_port shr 8); // Swap endianness
  end
  else
    Result := 80;
end;

function THttpSysRawRequest.GetContentType: string;
begin
  Result := GetFieldByName('Content-Type');
end;

function THttpSysRawRequest.GetContent: string;
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

{$IF DEFINED(FPC)}
function THttpSysRawRequest.GetContentLength: Integer;
begin
  Result := StrToIntDef(GetFieldByName('Content-Length'), 0);
end;
{$ELSEIF CompilerVersion >= 32.0}
function THttpSysRawRequest.GetContentLength: Int64;
begin
  Result := StrToInt64Def(GetFieldByName('Content-Length'), 0);
end;
{$ELSE}
function THttpSysRawRequest.GetContentLength: Integer;
begin
  Result := StrToIntDef(GetFieldByName('Content-Length'), 0);
end;
{$IFEND}

function THttpSysRawRequest.GetFieldByName(const AName: string): string;
var
  LIndex: Integer;
  I: Integer;
  LUnknownName: string;
begin
  Result := '';
  if THorseProviderHttpSys.KnownRequestHeadersMap.TryGetValue(AName, LIndex) then
  begin
    if FRequest.Headers.KnownHeaders[LIndex].RawValueLength > 0 then
    begin
      SetString(Result, PAnsiChar(FRequest.Headers.KnownHeaders[LIndex].pRawValue), FRequest.Headers.KnownHeaders[LIndex].RawValueLength);
      Exit;
    end;
  end;

  if (FRequest.Headers.UnknownHeaderCount > 0) and (FRequest.Headers.pUnknownHeaders <> nil) then
  begin
    for I := 0 to FRequest.Headers.UnknownHeaderCount - 1 do
    begin
      SetString(LUnknownName, PAnsiChar(PHTTP_UNKNOWN_HEADER_ARRAY(FRequest.Headers.pUnknownHeaders)^[I].pName), PHTTP_UNKNOWN_HEADER_ARRAY(FRequest.Headers.pUnknownHeaders)^[I].NameLength);
      if SameText(LUnknownName, AName) then
      begin
        SetString(Result, PAnsiChar(PHTTP_UNKNOWN_HEADER_ARRAY(FRequest.Headers.pUnknownHeaders)^[I].pRawValue), PHTTP_UNKNOWN_HEADER_ARRAY(FRequest.Headers.pUnknownHeaders)^[I].RawValueLength);
        Exit;
      end;
    end;
  end;
end;

procedure THttpSysRawRequest.PopulateQueryFields(ADest: TStrings);
var
  LQuery: string;
  LFields: TArray<string>;
  LField: string;
  LPos: Integer;
  LName, LValue: string;
begin
  LQuery := GetQueryString;
  if LQuery = '' then Exit;
  if LQuery.StartsWith('?') then
    LQuery := LQuery.Substring(1);

  LFields := LQuery.Split(['&']);
  for LField in LFields do
  begin
    LPos := LField.IndexOf('=');
    if LPos >= 0 then
    begin
      LName := LField.Substring(0, LPos);
      LValue := LField.Substring(LPos + 1);
      ADest.Add(
        {$IF DEFINED(FPC)}HTTPDecode(LName){$ELSE}TNetEncoding.URL.Decode(LName){$ENDIF} + '=' +
        {$IF DEFINED(FPC)}HTTPDecode(LValue){$ELSE}TNetEncoding.URL.Decode(LValue){$ENDIF}
      );
    end
    else
      ADest.Add({$IF DEFINED(FPC)}HTTPDecode(LField){$ELSE}TNetEncoding.URL.Decode(LField){$ENDIF} + '=');
  end;
end;

procedure THttpSysRawRequest.PopulateContentFields(ADest: TStrings);
var
  LBody: string;
  LFields: TArray<string>;
  LField: string;
  LPos: Integer;
  LName, LValue: string;
begin
  if not SameText(GetContentType, 'application/x-www-form-urlencoded') then
    Exit;

  LBody := GetContent;
  if LBody = '' then Exit;

  LFields := LBody.Split(['&']);
  for LField in LFields do
  begin
    LPos := LField.IndexOf('=');
    if LPos >= 0 then
    begin
      LName := LField.Substring(0, LPos);
      LValue := LField.Substring(LPos + 1);
      ADest.Add(
        {$IF DEFINED(FPC)}HTTPDecode(LName){$ELSE}TNetEncoding.URL.Decode(LName){$ENDIF} + '=' +
        {$IF DEFINED(FPC)}HTTPDecode(LValue){$ELSE}TNetEncoding.URL.Decode(LValue){$ENDIF}
      );
    end
    else
      ADest.Add({$IF DEFINED(FPC)}HTTPDecode(LField){$ELSE}TNetEncoding.URL.Decode(LField){$ENDIF} + '=');
  end;
end;

procedure THttpSysRawRequest.PopulateCookieFields(ADest: TStrings);
var
  LCookies: string;
  LParts: TArray<string>;
  LPart: string;
  LPartTrimmed: string;
  LPos: Integer;
  LName, LValue: string;
begin
  LCookies := GetFieldByName('Cookie');
  if LCookies = '' then Exit;

  LParts := LCookies.Split([';']);
  for LPart in LParts do
  begin
    LPartTrimmed := LPart.Trim;
    LPos := LPartTrimmed.IndexOf('=');
    if LPos >= 0 then
    begin
      LName := LPartTrimmed.Substring(0, LPos);
      LValue := LPartTrimmed.Substring(LPos + 1);
      ADest.Add(LName + '=' + LValue);
    end;
  end;
end;

function THttpSysRawRequest.ReadBody(var Buffer; Count: Integer): Integer;
begin
  EnsureBodyStream;
  Result := FBodyStream.Read(Buffer, Count);
end;


{ THttpSysRawResponse }

constructor THttpSysRawResponse.Create(AReqQueue: THandle; ARequestId: HTTP_REQUEST_ID);
begin
  inherited Create;
  FReqQueue := AReqQueue;
  FRequestId := ARequestId;
end;

procedure THttpSysRawResponse.SetCustomHeader(const AName, AValue: string);
begin
  // SetCustomHeader is inherited from TWebResponse and handled on SendResponse.
end;

procedure THttpSysRawResponse.SendResponse(const ARes: THorseResponse);
var
  LResponse: HTTP_RESPONSE;
  LChunk: HTTP_DATA_CHUNK_INMEMORY;
  LBytesSent: ULONG;
  LRet: ULONG;
  LBodyBytes: TBytes;
  LHeaders: TArray<HTTP_UNKNOWN_HEADER>;
  LHeaderCount: Integer;
  LContentType: AnsiString;
  LStatusCode: Word;
  LReason: AnsiString;
  {$IF DEFINED(FPC)}
  LHeadersList: TStringList;
  {$ELSE}
  LHeadersList: TDictionary<string, string>;
  LPair: TPair<string, string>;
  {$ENDIF}
  I: Integer;
  LContentLengthAnsi: AnsiString;
  LHeaderStrings: TArray<AnsiString>;
begin
  FillChar(LResponse, SizeOf(LResponse), 0);
  
  LStatusCode := ARes.Status;
  LResponse.StatusCode := LStatusCode;
  
  LReason := 'OK';
  if LStatusCode = 201 then LReason := 'Created'
  else if LStatusCode = 204 then LReason := 'No Content'
  else if LStatusCode = 400 then LReason := 'Bad Request'
  else if LStatusCode = 401 then LReason := 'Unauthorized'
  else if LStatusCode = 403 then LReason := 'Forbidden'
  else if LStatusCode = 404 then LReason := 'Not Found'
  else if LStatusCode = 500 then LReason := 'Internal Server Error';
  
  LResponse.ReasonLength := Length(LReason);
  LResponse.pReason := PAnsiChar(LReason);
  LResponse.Version.MajorVersion := 1;
  LResponse.Version.MinorVersion := 1;

  // Set Content-Type
  if (ARes.RawWebResponse <> nil) and (ARes.RawWebResponse.ContentType <> '') then
    LContentType := AnsiString(ARes.RawWebResponse.ContentType)
  else
    LContentType := AnsiString(ARes.CSContentType);

  if LContentType = '' then
    LContentType := 'text/html; charset=utf-8';
  
  LResponse.Headers.KnownHeaders[12].pRawValue := PAnsiChar(LContentType);
  LResponse.Headers.KnownHeaders[12].RawValueLength := Length(LContentType);

  // Set Custom Headers
  LHeadersList := ARes.CustomHeaders;
  if LHeadersList <> nil then
    LHeaderCount := LHeadersList.Count
  else
    LHeaderCount := 0;

  if LHeaderCount > 0 then
  begin
    SetLength(LHeaders, LHeaderCount);
    SetLength(LHeaderStrings, LHeaderCount * 2);
    {$IF DEFINED(FPC)}
    for I := 0 to LHeaderCount - 1 do
    begin
      LHeaderStrings[I * 2] := AnsiString(LHeadersList.Names[I]);
      LHeaderStrings[I * 2 + 1] := AnsiString(LHeadersList.ValueFromIndex[I]);
      
      LHeaders[I].NameLength := Length(LHeaderStrings[I * 2]);
      LHeaders[I].RawValueLength := Length(LHeaderStrings[I * 2 + 1]);
      LHeaders[I].pName := PAnsiChar(LHeaderStrings[I * 2]);
      LHeaders[I].pRawValue := PAnsiChar(LHeaderStrings[I * 2 + 1]);
    end;
    {$ELSE}
    I := 0;
    for LPair in LHeadersList do
    begin
      LHeaderStrings[I * 2] := AnsiString(LPair.Key);
      LHeaderStrings[I * 2 + 1] := AnsiString(LPair.Value);
      
      LHeaders[I].NameLength := Length(LHeaderStrings[I * 2]);
      LHeaders[I].RawValueLength := Length(LHeaderStrings[I * 2 + 1]);
      LHeaders[I].pName := PAnsiChar(LHeaderStrings[I * 2]);
      LHeaders[I].pRawValue := PAnsiChar(LHeaderStrings[I * 2 + 1]);
      Inc(I);
    end;
    {$ENDIF}
    LResponse.Headers.UnknownHeaderCount := LHeaderCount;
    LResponse.Headers.pUnknownHeaders := @LHeaders[0];
  end;

  // Read Body Stream if any
  if ARes.ContentStream <> nil then
  begin
    SetLength(LBodyBytes, ARes.ContentStream.Size);
    ARes.ContentStream.Position := 0;
    if ARes.ContentStream.Size > 0 then
      ARes.ContentStream.ReadBuffer(LBodyBytes[0], ARes.ContentStream.Size);
  end;

  // Fallback to BodyText or WebResponse.Content
  if Length(LBodyBytes) = 0 then
  begin
    if ARes.BodyText <> '' then
      LBodyBytes := TEncoding.UTF8.GetBytes(ARes.BodyText)
    else if (ARes.RawWebResponse <> nil) and (ARes.RawWebResponse.Content <> '') then
      LBodyBytes := TEncoding.UTF8.GetBytes(ARes.RawWebResponse.Content);
  end;

  if Length(LBodyBytes) > 0 then
  begin
    // Set Content-Length
    LContentLengthAnsi := AnsiString(IntToStr(Length(LBodyBytes)));
    LResponse.Headers.KnownHeaders[11].pRawValue := PAnsiChar(LContentLengthAnsi);
    LResponse.Headers.KnownHeaders[11].RawValueLength := Length(LContentLengthAnsi);

    FillChar(LChunk, SizeOf(LChunk), 0);
    LChunk.DataChunkType := hctFromMemory;
    LChunk.pBuffer := @LBodyBytes[0];
    LChunk.BufferLength := Length(LBodyBytes);

    LResponse.EntityChunkCount := 1;
    LResponse.pEntityChunks := @LChunk;
  end
  else
  begin
    // Set Content-Length to 0
    LResponse.Headers.KnownHeaders[11].pRawValue := '0';
    LResponse.Headers.KnownHeaders[11].RawValueLength := 1;
  end;

  LRet := HttpSendHttpResponse(
    FReqQueue,
    FRequestId,
    0,
    @LResponse,
    nil,
    LBytesSent,
    nil,
    0,
    nil,
    nil
  );
  if LRet <> ERROR_SUCCESS then
    raise EOSError.Create('HttpSendHttpResponse failed with error code: ' + IntToStr(LRet));
end;


{ THttpSysWebResponse }

constructor THttpSysWebResponse.Create(const ARawRes: IHorseRawResponse);
begin
  inherited Create(ARawRes);
  {$IFNDEF FPC}
  FStatusCode := 200;
  {$ENDIF}
end;

{$IFNDEF FPC}
function THttpSysWebResponse.GetStringVariable(Index: Integer): string;
begin
  case Index of
    25: Result := FContent;
    15: Result := FContentType;
    14: Result := FContentEncoding;
  else
    Result := '';
  end;
end;

procedure THttpSysWebResponse.SetStringVariable(Index: Integer; const Value: string);
begin
  case Index of
    25: FContent := Value;
    15: FContentType := Value;
    14: FContentEncoding := Value;
  end;
end;

{$IF CompilerVersion >= 32.0}
function THttpSysWebResponse.GetIntegerVariable(Index: Integer): Int64;
{$ELSE}
function THttpSysWebResponse.GetIntegerVariable(Index: Integer): Integer;
{$IFEND}
begin
  if Index = 0 then
    Result := FStatusCode
  else
    Result := 0;
end;

{$IF CompilerVersion >= 32.0}
procedure THttpSysWebResponse.SetIntegerVariable(Index: Integer; Value: Int64);
{$ELSE}
procedure THttpSysWebResponse.SetIntegerVariable(Index: Integer; Value: Integer);
{$IFEND}
begin
  if Index = 0 then
    FStatusCode := Value;
end;

function THttpSysWebResponse.GetContent: string;
begin
  Result := FContent;
end;

procedure THttpSysWebResponse.SetContent(const Value: string);
begin
  FContent := Value;
end;

function THttpSysWebResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

procedure THttpSysWebResponse.SetStatusCode(Value: Integer);
begin
  FStatusCode := Value;
end;
{$ENDIF}


{ THttpSysListenerThread }

constructor THttpSysListenerThread.Create(AReqQueue: THandle);
begin
  inherited Create(True);
  FReqQueue := AReqQueue;
  FRunning := False;
  FreeOnTerminate := False;
end;

procedure THttpSysListenerThread.DispatchRequest(ABuffer: TBytes);
begin
  {$IF DEFINED(FPC)}
  THttpSysRequestThread.Create(FReqQueue, ABuffer);
  {$ELSE}
  TTask.Run(
    procedure
    var
      LRawReq: IHorseRawRequest;
      LRawRes: IHorseRawResponse;
      LConcreteRes: THttpSysRawResponse;
      LWebRequest: TInterfacedWebRequest;
      LWebResponse: TInterfacedWebResponse;
      LReq: THorseRequest;
      LRes: THorseResponse;
      LRequest: PHTTP_REQUEST;
    begin
      try
        LRequest := PHTTP_REQUEST(@ABuffer[0]);
        LRawReq := THttpSysRawRequest.Create(LRequest, ABuffer);
        LConcreteRes := THttpSysRawResponse.Create(FReqQueue, LRequest.RequestId);
        LRawRes := LConcreteRes;
        LWebRequest := TInterfacedWebRequest.Create(LRawReq);
        LWebResponse := THttpSysWebResponse.Create(LRawRes);
        try
          LReq := THorseRequest.Create(LWebRequest);
          LRes := THorseResponse.Create(LWebResponse);
          try
            THorseProviderHttpSys.Execute(LReq, LRes);
          finally
            LConcreteRes.SendResponse(LRes);
            LReq.Free;
            LRes.Free;
          end;
        finally
          LWebRequest.Free;
          LWebResponse.Free;
        end;
      except
        on E: Exception do
          Writeln('Erro no Dispatch: ' + E.ClassName + ': ' + E.Message);
      end;
    end
  );
  {$ENDIF}
end;

procedure THttpSysListenerThread.Execute;
var
  LBuffer: TBytes;
  LRequest: PHTTP_REQUEST;
  LBytesReturned: ULONG;
  LRet: ULONG;
  LRequestId: HTTP_REQUEST_ID;
begin
  LRequestId := 0;
  LRequest := nil;
  while not Terminated and FRunning do
  begin
    if LRequestId = 0 then
    begin
      SetLength(LBuffer, 16384); // Pre-allocated 16KB Request Buffer
      LRequest := PHTTP_REQUEST(@LBuffer[0]);
      FillChar(LRequest^, SizeOf(HTTP_REQUEST), 0);
    end;
    LBytesReturned := 0;

    LRet := HttpReceiveHttpRequest(
      FReqQueue,
      LRequestId,
      0,
      LRequest,
      Length(LBuffer),
      LBytesReturned,
      nil
    );

    if LRet = ERROR_SUCCESS then
    begin
      // Dispatch processing concurrently to Delphi Thread Pool
      DispatchRequest(LBuffer);
      LRequestId := 0;
    end
    else if LRet = ERROR_MORE_DATA then
    begin
      // Re-allocate enough space for large request headers and try again
      LRequestId := LRequest.RequestId;
      SetLength(LBuffer, LBytesReturned);
      LRequest := PHTTP_REQUEST(@LBuffer[0]);
    end
    else
    begin
      LRequestId := 0;
      if (LRet = 1229) or (LRet = ERROR_CONNECTION_INVALID) then // 1229 = ERROR_CONNECTION_INVALID
        // Connection lost, continue
      else if not FRunning then
        Break;
    end;
  end;
end;


{ THorseProviderHttpSys }

class constructor THorseProviderHttpSys.CreateClass;
var
  I: Integer;
begin
  FRunning := False;
  FPort := 0;
  FHost := '';
  FServerSessionId := 0;
  FUrlGroupId := 0;
  FReqQueue := 0;
  FListenerThread := nil;

  {$IF DEFINED(FPC)}
  FKnownRequestHeadersMap := TDictionary<string, Integer>.Create;
  {$ELSE}
  FKnownRequestHeadersMap := TDictionary<string, Integer>.Create(TIStringComparer.Ordinal);
  {$ENDIF}
  for I := 0 to 40 do
    FKnownRequestHeadersMap.Add(HTTP_KNOWN_REQUEST_HEADERS[I], I);

  {$IF DEFINED(FPC)}
  FKnownResponseHeadersMap := TDictionary<string, Integer>.Create;
  {$ELSE}
  FKnownResponseHeadersMap := TDictionary<string, Integer>.Create(TIStringComparer.Ordinal);
  {$ENDIF}
  for I := 0 to 29 do
    FKnownResponseHeadersMap.Add(HTTP_KNOWN_RESPONSE_HEADERS[I], I);
end;

class destructor THorseProviderHttpSys.DestroyClass;
begin
  FKnownRequestHeadersMap.Free;
  FKnownResponseHeadersMap.Free;
end;

class procedure THorseProviderHttpSys.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

class procedure THorseProviderHttpSys.SetHost(const AValue: string);
begin
  FHost := AValue.Trim;
end;

class function THorseProviderHttpSys.GetPort: Integer;
begin
  Result := FPort;
end;

class function THorseProviderHttpSys.GetHost: string;
begin
  Result := FHost;
end;

class function THorseProviderHttpSys.GetDefaultPort: Integer;
begin
  Result := 9000;
end;

class function THorseProviderHttpSys.GetDefaultHost: string;
begin
  Result := '0.0.0.0';
end;

class procedure THorseProviderHttpSys.InternalListen;
var
  LPrefix: string;
  LRet: ULONG;
  LBinding: HTTP_BINDING_INFO;
begin
  if FRunning then Exit;

  if FPort <= 0 then
    FPort := GetDefaultPort;
  if FHost.IsEmpty then
    FHost := GetDefaultHost;

  // 1. Initialize HTTP.sys Session
  LRet := HttpInitialize(HTTPAPI_VERSION_2, HTTP_INITIALIZE_SERVER, nil);
  if LRet <> ERROR_SUCCESS then
    raise EOSError.Create('HttpInitialize failed with error code: ' + IntToStr(LRet));

  try
    LRet := HttpCreateServerSession(HTTPAPI_VERSION_2, FServerSessionId, 0);
    if LRet <> ERROR_SUCCESS then
      raise EOSError.Create('HttpCreateServerSession failed with error code: ' + IntToStr(LRet));

    try
      // 2. Create URL Group
      LRet := HttpCreateUrlGroup(FServerSessionId, FUrlGroupId, 0);
      if LRet <> ERROR_SUCCESS then
        raise EOSError.Create('HttpCreateUrlGroup failed with error code: ' + IntToStr(LRet));

      try
        // 3. Create Request Queue
        LRet := HttpCreateRequestQueue(HTTPAPI_VERSION_2, nil, nil, 0, FReqQueue);
        if LRet <> ERROR_SUCCESS then
          raise EOSError.Create('HttpCreateRequestQueue failed with error code: ' + IntToStr(LRet));

        // 4. Set Request Queue Binding Property
        LBinding.Flags := 1;
        LBinding.RequestQueueHandle := FReqQueue;
        LRet := HttpSetUrlGroupProperty(FUrlGroupId, HttpServerBindingProperty, @LBinding, SizeOf(LBinding));
        if LRet <> ERROR_SUCCESS then
          raise EOSError.Create('HttpSetUrlGroupProperty (Binding) failed with error code: ' + IntToStr(LRet));

        // 5. Add URL namespace prefix
        // Map 0.0.0.0 or empty host to '+' wildcard for global listening (requires admin/urlacl)
        if (FHost = '0.0.0.0') or (FHost = '') then
          LPrefix := Format('http://+:%d/', [FPort])
        else
          LPrefix := Format('http://%s:%d/', [FHost, FPort]);

        LRet := HttpAddUrlToUrlGroup(FUrlGroupId, PWideChar(WideString(LPrefix)), 0, 0);
        if LRet <> ERROR_SUCCESS then
          raise EOSError.Create('HttpAddUrlToUrlGroup failed to register ' + LPrefix + ' with error code: ' + IntToStr(LRet));

        // 6. Start Listener Thread
        FRunning := True;
        FListenerThread := THttpSysListenerThread.Create(FReqQueue);
        FListenerThread.Running := True;
        FListenerThread.Start;

        DoOnListen;

        if IsConsole then
        begin
          while FRunning do
            Sleep(100);
        end;

      except
        if FReqQueue <> 0 then
        begin
          HttpCloseRequestQueue(FReqQueue);
          FReqQueue := 0;
        end;
        raise;
      end;
    except
      if FUrlGroupId <> 0 then
      begin
        HttpCloseUrlGroup(FUrlGroupId);
        FUrlGroupId := 0;
      end;
      raise;
    end;
  except
    if FServerSessionId <> 0 then
    begin
      HttpCloseServerSession(FServerSessionId);
      FServerSessionId := 0;
    end;
    HttpTerminate(HTTP_INITIALIZE_SERVER, nil);
    FRunning := False;
    raise;
  end;
end;

class procedure THorseProviderHttpSys.InternalStopListen;
begin
  if not FRunning then Exit;

  FRunning := False;

  // Signal and stop listener thread
  if FListenerThread <> nil then
  begin
    FListenerThread.Running := False;
    FListenerThread.Terminate;
  end;

  // Closing the request queue cancels any pending HttpReceiveHttpRequest API calls, forcing thread to exit
  if FReqQueue <> 0 then
  begin
    HttpCloseRequestQueue(FReqQueue);
    FReqQueue := 0;
  end;

  if FListenerThread <> nil then
  begin
    FListenerThread.WaitFor;
    FreeAndNil(FListenerThread);
  end;

  if FUrlGroupId <> 0 then
  begin
    HttpCloseUrlGroup(FUrlGroupId);
    FUrlGroupId := 0;
  end;

  if FServerSessionId <> 0 then
  begin
    HttpCloseServerSession(FServerSessionId);
    FServerSessionId := 0;
  end;

  HttpTerminate(HTTP_INITIALIZE_SERVER, nil);
  DoOnStopListen;
end;

class procedure THorseProviderHttpSys.Listen;
begin
  InternalListen;
end;

class procedure THorseProviderHttpSys.Listen(const APort: Integer; const AHost: string; const ACallbackListen, ACallbackStopListen: TProc);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallbackListen);
  SetOnStopListen(ACallbackStopListen);
  InternalListen;
end;

class procedure THorseProviderHttpSys.Listen(const AHost: string; const ACallbackListen, ACallbackStopListen: TProc);
begin
  Listen(FPort, AHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProviderHttpSys.Listen(const ACallbackListen, ACallbackStopListen: TProc);
begin
  Listen(FPort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProviderHttpSys.Listen(const APort: Integer; const ACallbackListen, ACallbackStopListen: TProc);
begin
  Listen(APort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProviderHttpSys.ListenWithConfig(const APort: Integer; const AConfig: THorseCrossSocketConfig);
begin
  SetPort(APort);
  InternalListen;
end;

class procedure THorseProviderHttpSys.StopListen;
begin
  InternalStopListen;
end;

class function THorseProviderHttpSys.IsRunning: Boolean;
begin
  Result := FRunning;
end;

{$ENDIF}

end.
