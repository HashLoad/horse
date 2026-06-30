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
    Generics.Collections,
    Generics.Defaults,
    Windows,
    SyncObjs,
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

  HTTP_DATA_CHUNK_FILE = record
    DataChunkType: THttpChunkType;
    {$IFDEF CPUX64}
    Reserved1: ULONG;
    {$ENDIF}
    StartingOffset: UInt64;
    Length: UInt64;
    FileHandle: THandle;
    {$IFNDEF CPUX64}
    Reserved2: array[0..1] of ULONG;
    {$ENDIF}
  end;

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

  THTTP_SERVER_PROPERTY = (
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
function HttpSetUrlGroupProperty(UrlGroupId: HTTP_URL_GROUP_ID; PropertyId: THTTP_SERVER_PROPERTY; pPropertyInformation: Pointer; PropertyInformationLength: ULONG): ULONG; stdcall; external HTTPAPI_DLL;

{$MINENUMSIZE 1}

type
  THttpSysBufferPool = class
  private
    FBuffers: TQueue<TBytes>;
    FSync: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    function Acquire(AMinSize: Integer): TBytes;
    procedure Release(var ABuffer: TBytes);
  end;

  THttpSysListenerThread = class;

  {$IF DEFINED(FPC)}
  THttpSysThreadPool = class;

  THttpSysWorkerThread = class(TThread)
  private
    FPool: THttpSysThreadPool;
    FReqQueue: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(APool: THttpSysThreadPool; AReqQueue: THandle);
  end;

  THttpSysThreadPool = class
  private
    FQueue: TQueue<TBytes>;
    FSync: TCriticalSection;
    FEvent: TEvent;
    FWorkers: TList<TThread>;
    FActive: Boolean;
    class var FInstance: THttpSysThreadPool;
  public
    constructor Create(AMaxThreads: Integer);
    destructor Destroy; override;
    procedure QueueRequest(const ABuffer: TBytes);
    class property Instance: THttpSysThreadPool read FInstance;
  end;
  {$ENDIF}

  // IHorseRawRequest implementation
  THttpSysRawRequest = class(TInterfacedObject, IHorseRawRequest)
  private
    FRequest: PHTTP_REQUEST;
    FBuffer: TBytes;
    FBodyStream: TStream;
    FTempFileName: string;
    FHeadersCache: TDictionary<string, string>;
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
    class var FBufferPool: THttpSysBufferPool;

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
    class property BufferPool: THttpSysBufferPool read FBufferPool;

    class constructor CreateClass;
    class destructor DestroyClass;
  end;

  THorseProvider = class(THorseProviderHttpSys);

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

type
  PHttpSysWorkItemData = ^THttpSysWorkItemData;
  THttpSysWorkItemData = record
    Buffer: TBytes;
    ReqQueue: THandle;
  end;

function QueueUserWorkItem(Func: Pointer; Context: Pointer; Flags: ULONG): BOOL; stdcall; external 'kernel32.dll' name 'QueueUserWorkItem';
const
  WT_EXECUTEDEFAULT = $00000000;

function HttpSysWorkItemCallback(LPParameter: Pointer): DWORD; stdcall;
var
  LData: PHttpSysWorkItemData;
  LRawReq: IHorseRawRequest;
  LRawRes: IHorseRawResponse;
  LConcreteRes: THttpSysRawResponse;
  LWebRequest: TInterfacedWebRequest;
  LWebResponse: TInterfacedWebResponse;
  LReq: THorseRequest;
  LRes: THorseResponse;
  LRequest: PHTTP_REQUEST;
  LCurrentBuf: TBytes;
begin
  Result := 0;
  LData := PHttpSysWorkItemData(LPParameter);
  LCurrentBuf := LData.Buffer;
  try
    try
      LRequest := PHTTP_REQUEST(@LCurrentBuf[0]);
      LRawReq := THttpSysRawRequest.Create(LRequest, LCurrentBuf);
      LConcreteRes := THttpSysRawResponse.Create(LData.ReqQueue, LRequest.RequestId);
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
  finally
    THorseProviderHttpSys.BufferPool.Release(LCurrentBuf);
    Dispose(LData);
  end;
end;

function GetWindowsTempPath: string;
var
  LBuffer: array[0..MAX_PATH] of Char;
begin
  if GetTempPath(MAX_PATH, LBuffer) > 0 then
    Result := string(LBuffer)
  else
    Result := 'C:\Temp\';
  Result := IncludeTrailingPathDelimiter(Result);
end;

function HttpSysDeleteFile(const AFileName: string): Boolean;
begin
  {$IF DEFINED(FPC)}
  Result := SysUtils.DeleteFile(AFileName);
  {$ELSE}
  Result := System.SysUtils.DeleteFile(AFileName);
  {$ENDIF}
end;



{ THttpSysBufferPool }

constructor THttpSysBufferPool.Create;
begin
  inherited Create;
  FBuffers := TQueue<TBytes>.Create;
  FSync := TCriticalSection.Create;
end;

destructor THttpSysBufferPool.Destroy;
begin
  FSync.Acquire;
  try
    while FBuffers.Count > 0 do
      FBuffers.Dequeue;
    FBuffers.Free;
  finally
    FSync.Release;
    FSync.Free;
  end;
  inherited;
end;

function THttpSysBufferPool.Acquire(AMinSize: Integer): TBytes;
begin
  FSync.Acquire;
  try
    if FBuffers.Count > 0 then
    begin
      Result := FBuffers.Dequeue;
      if Length(Result) < AMinSize then
        SetLength(Result, AMinSize);
    end
    else
    begin
      SetLength(Result, AMinSize);
    end;
  finally
    FSync.Release;
  end;
end;

procedure THttpSysBufferPool.Release(var ABuffer: TBytes);
begin
  if ABuffer = nil then Exit;
  FSync.Acquire;
  try
    if (FBuffers.Count < 1000) and (Length(ABuffer) <= 65536) then
    begin
      FBuffers.Enqueue(ABuffer);
    end;
  finally
    FSync.Release;
  end;
  ABuffer := nil;
end;

function AnsiStrEqualNoCase(const AStr1: PAnsiChar; const AStr2: PAnsiChar; ALen: Integer): Boolean;
var
  I: Integer;
  C1, C2: Byte;
begin
  for I := 0 to ALen - 1 do
  begin
    C1 := Byte(AStr1[I]);
    C2 := Byte(AStr2[I]);
    if C1 <> C2 then
    begin
      if (C1 >= 65) and (C1 <= 90) then Inc(C1, 32);
      if (C2 >= 65) and (C2 <= 90) then Inc(C2, 32);
      if C1 <> C2 then
        Exit(False);
    end;
  end;
  Result := True;
end;

function FastIntToAnsiString(AValue: Int64): AnsiString;
var
  LBuffer: array[0..31] of AnsiChar;
  LPtr: PAnsiChar;
  LVal: UInt64;
  LDigit: UInt64;
begin
  if AValue = 0 then
    Exit('0');
  
  LPtr := @LBuffer[31];
  LPtr^ := #0;
  
  if AValue < 0 then
    LVal := UInt64(-AValue)
  else
    LVal := UInt64(AValue);
    
  while LVal > 0 do
  begin
    Dec(LPtr);
    LDigit := LVal mod 10;
    LPtr^ := AnsiChar(Ord('0') + LDigit);
    LVal := LVal div 10;
  end;
  
  if AValue < 0 then
  begin
    Dec(LPtr);
    LPtr^ := '-';
  end;
  
  SetString(Result, LPtr, @LBuffer[31] - LPtr);
end;

{$IF DEFINED(FPC)}
{ THttpSysWorkerThread }

constructor THttpSysWorkerThread.Create(APool: THttpSysThreadPool; AReqQueue: THandle);
begin
  inherited Create(False);
  FPool := APool;
  FReqQueue := AReqQueue;
  FreeOnTerminate := False;
end;

procedure THttpSysWorkerThread.Execute;
var
  LBuffer: TBytes;
  LRawReq: IHorseRawRequest;
  LRawRes: IHorseRawResponse;
  LConcreteRes: THttpSysRawResponse;
  LWebRequest: TInterfacedWebRequest;
  LWebResponse: TInterfacedWebResponse;
  LReq: THorseRequest;
  LRes: THorseResponse;
  LRequest: PHTTP_REQUEST;
  LHasItem: Boolean;
begin
  while not Terminated and FPool.FActive do
  begin
    LHasItem := False;
    LBuffer := nil;

    FPool.FSync.Acquire;
    try
      if FPool.FQueue.Count > 0 then
      begin
        LBuffer := FPool.FQueue.Dequeue;
        LHasItem := True;
      end;
    finally
      FPool.FSync.Release;
    end;

    if LHasItem then
    begin
      try
        LRequest := PHTTP_REQUEST(@LBuffer[0]);
        LRawReq := THttpSysRawRequest.Create(LRequest, LBuffer);
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
      THorseProviderHttpSys.BufferPool.Release(LBuffer);
    end
    else
    begin
      FPool.FEvent.WaitFor(1000);
    end;
  end;
end;

{ THttpSysThreadPool }

constructor THttpSysThreadPool.Create(AMaxThreads: Integer);
var
  I: Integer;
begin
  FQueue := TQueue<TBytes>.Create;
  FSync := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, False, False, '');
  FWorkers := TList<TThread>.Create;
  FActive := True;

  for I := 0 to AMaxThreads - 1 do
    FWorkers.Add(THttpSysWorkerThread.Create(Self, THorseProviderHttpSys.ReqQueue));
end;

destructor THttpSysThreadPool.Destroy;
var
  LWorker: TThread;
begin
  FActive := False;
  FEvent.SetEvent;

  for LWorker in FWorkers do
    LWorker.Terminate;

  FEvent.SetEvent;

  for LWorker in FWorkers do
  begin
    LWorker.WaitFor;
    LWorker.Free;
  end;

  FWorkers.Free;
  FEvent.Free;

  while FQueue.Count > 0 do
    FQueue.Dequeue;
  FQueue.Free;

  FSync.Free;
  inherited;
end;

procedure THttpSysThreadPool.QueueRequest(const ABuffer: TBytes);
begin
  FSync.Acquire;
  try
    FQueue.Enqueue(ABuffer);
  finally
    FSync.Release;
  end;
  FEvent.SetEvent;
end;
{$ENDIF}

{ THttpSysRawRequest }

constructor THttpSysRawRequest.Create(ARequest: PHTTP_REQUEST; const ABuffer: TBytes);
begin
  inherited Create;
  FRequest := ARequest;
  FBuffer := ABuffer;
  FBodyStream := nil;
  FTempFileName := '';
  FHeadersCache := nil;
end;

destructor THttpSysRawRequest.Destroy;
begin
  if Assigned(FBodyStream) then
    FBodyStream.Free;
  if FTempFileName <> '' then
    HttpSysDeleteFile(FTempFileName);
  if Assigned(FHeadersCache) then
    FHeadersCache.Free;
  inherited;
end;

procedure THttpSysRawRequest.EnsureBodyStream;
var
  PChunk: PHTTP_DATA_CHUNK_INMEMORY;
  I: Integer;
  BytesReceived: ULONG;
  Ret: ULONG;
  TempBuf: TBytes;
  LTempPath, LTempFile: string;
  LFileStream: TFileStream;
  LContentLength: Int64;
  LChunkLength: ULONG;
begin
  if FBodyStream <> nil then Exit;

  LContentLength := GetContentLength;

  if (LContentLength > 0) and (LContentLength >= 2097152) then
  begin
    LTempPath := GetWindowsTempPath;
    LTempFile := LTempPath + 'horse_httpsys_spool_' + IntToStr(GetTickCount64) + '_' + IntToStr(FRequest.RequestId) + '.tmp';
    FTempFileName := LTempFile;
    FBodyStream := TFileStream.Create(LTempFile, fmCreate or fmOpenReadWrite);
  end
  else
  begin
    FBodyStream := TMemoryStream.Create;
  end;

  try
    // 1. Copy initial body chunk if any
    if (FRequest.EntityChunkCount > 0) and (FRequest.pEntityChunks <> nil) then
    begin
      PChunk := PHTTP_DATA_CHUNK_INMEMORY(FRequest.pEntityChunks);
      for I := 0 to FRequest.EntityChunkCount - 1 do
      begin
        if PChunk.DataChunkType = hctFromMemory then
        begin
          if (PChunk.pBuffer <> nil) and (PChunk.BufferLength > 0) then
          begin
            LChunkLength := PChunk.BufferLength;

            if (FBodyStream is TMemoryStream) and (FBodyStream.Size + LChunkLength >= 2097152) then
            begin
              LTempPath := GetWindowsTempPath;
              LTempFile := LTempPath + 'horse_httpsys_spool_' + IntToStr(GetTickCount64) + '_' + IntToStr(FRequest.RequestId) + '.tmp';
              FTempFileName := LTempFile;
              LFileStream := TFileStream.Create(LTempFile, fmCreate or fmOpenReadWrite);
              try
                if FBodyStream.Size > 0 then
                begin
                  TMemoryStream(FBodyStream).Position := 0;
                  LFileStream.CopyFrom(FBodyStream, FBodyStream.Size);
                end;
                FBodyStream.Free;
                FBodyStream := LFileStream;
              except
                LFileStream.Free;
                HttpSysDeleteFile(LTempFile);
                raise;
              end;
            end;

            FBodyStream.WriteBuffer(PChunk.pBuffer^, LChunkLength);
          end;
        end;
        Inc(PChunk);
      end;
    end;

    // 2. Retrieve remaining request body entity chunks from HTTP.sys queue
    if (FRequest.Flags and HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS) <> 0 then
    begin
      SetLength(TempBuf, 65536);
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

        if (Ret = ERROR_SUCCESS) or (Ret = ERROR_HANDLE_EOF) then
        begin
          if BytesReceived > 0 then
          begin
            if (FBodyStream is TMemoryStream) and (FBodyStream.Size + BytesReceived >= 2097152) then
            begin
              LTempPath := GetWindowsTempPath;
              LTempFile := LTempPath + 'horse_httpsys_spool_' + IntToStr(GetTickCount64) + '_' + IntToStr(FRequest.RequestId) + '.tmp';
              FTempFileName := LTempFile;
              LFileStream := TFileStream.Create(LTempFile, fmCreate or fmOpenReadWrite);
              try
                if FBodyStream.Size > 0 then
                begin
                  TMemoryStream(FBodyStream).Position := 0;
                  LFileStream.CopyFrom(FBodyStream, FBodyStream.Size);
                end;
                FBodyStream.Free;
                FBodyStream := LFileStream;
              except
                LFileStream.Free;
                HttpSysDeleteFile(LTempFile);
                raise;
              end;
            end;

            FBodyStream.WriteBuffer(TempBuf[0], BytesReceived);
          end;

          if Ret = ERROR_HANDLE_EOF then
            Break;
        end
        else
          Break;
      end;
    end;

    FBodyStream.Position := 0;
  except
    FreeAndNil(FBodyStream);
    if FTempFileName <> '' then
    begin
      HttpSysDeleteFile(FTempFileName);
      FTempFileName := '';
    end;
    raise;
  end;
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
  if (FRequest.Version.MajorVersion = 1) and (FRequest.Version.MinorVersion = 1) then
    Result := 'HTTP/1.1'
  else if (FRequest.Version.MajorVersion = 1) and (FRequest.Version.MinorVersion = 0) then
    Result := 'HTTP/1.0'
  else if (FRequest.Version.MajorVersion = 2) and (FRequest.Version.MinorVersion = 0) then
    Result := 'HTTP/2.0'
  else
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
  LAnsiName: AnsiString;
begin
  if FHeadersCache <> nil then
  begin
    if FHeadersCache.TryGetValue(AName, Result) then
      Exit;
  end;

  Result := '';
  
  if THorseProviderHttpSys.KnownRequestHeadersMap.TryGetValue(AName, LIndex) then
  begin
    if FRequest.Headers.KnownHeaders[LIndex].RawValueLength > 0 then
    begin
      SetString(Result, PAnsiChar(FRequest.Headers.KnownHeaders[LIndex].pRawValue), FRequest.Headers.KnownHeaders[LIndex].RawValueLength);
      if FHeadersCache = nil then
      begin
        {$IF DEFINED(FPC)}
        FHeadersCache := TDictionary<string, string>.Create;
        {$ELSE}
        FHeadersCache := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
        {$ENDIF}
      end;
      FHeadersCache.Add(AName, Result);
      Exit;
    end;
  end;

  if (FRequest.Headers.UnknownHeaderCount > 0) and (FRequest.Headers.pUnknownHeaders <> nil) then
  begin
    LAnsiName := AnsiString(AName);
    for I := 0 to FRequest.Headers.UnknownHeaderCount - 1 do
    begin
      with PHTTP_UNKNOWN_HEADER_ARRAY(FRequest.Headers.pUnknownHeaders)^[I] do
      begin
        if (NameLength = Length(LAnsiName)) and AnsiStrEqualNoCase(pName, PAnsiChar(LAnsiName), NameLength) then
        begin
          SetString(Result, pRawValue, RawValueLength);
          if FHeadersCache = nil then
          begin
            {$IF DEFINED(FPC)}
            FHeadersCache := TDictionary<string, string>.Create;
            {$ELSE}
            FHeadersCache := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
            {$ENDIF}
          end;
          FHeadersCache.AddOrSetValue(AName, Result);
          Exit;
        end;
      end;
    end;
  end;

  if FHeadersCache = nil then
  begin
    {$IF DEFINED(FPC)}
    FHeadersCache := TDictionary<string, string>.Create;
    {$ELSE}
    FHeadersCache := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
    {$ENDIF}
  end;
  FHeadersCache.Add(AName, '');
end;

procedure THttpSysRawRequest.PopulateQueryFields(ADest: TStrings);
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
  while LStart <= LLen do
  begin
    I := LStart;
    LEqPos := 0;
    while (I <= LLen) and (LQuery[I] <> '&') do
    begin
      if (LQuery[I] = '=') and (LEqPos = 0) then
        LEqPos := I;
      Inc(I);
    end;

    if LEqPos > 0 then
    begin
      LName := Copy(LQuery, LStart, LEqPos - LStart);
      LValue := Copy(LQuery, LEqPos + 1, I - LEqPos - 1);
      
      if Pos('%', LName) > 0 then
        LName := {$IF DEFINED(FPC)}HTTPDecode(LName){$ELSE}TNetEncoding.URL.Decode(LName){$ENDIF};
      if Pos('%', LValue) > 0 then
        LValue := {$IF DEFINED(FPC)}HTTPDecode(LValue){$ELSE}TNetEncoding.URL.Decode(LValue){$ENDIF};
        
      ADest.Add(LName + '=' + LValue);
    end
    else
    begin
      LName := Copy(LQuery, LStart, I - LStart);
      if Pos('%', LName) > 0 then
        LName := {$IF DEFINED(FPC)}HTTPDecode(LName){$ELSE}TNetEncoding.URL.Decode(LName){$ENDIF};
      ADest.Add(LName + '=');
    end;

    LStart := I + 1;
  end;
end;

procedure THttpSysRawRequest.PopulateContentFields(ADest: TStrings);
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
  while LStart <= LLen do
  begin
    I := LStart;
    LEqPos := 0;
    while (I <= LLen) and (LBody[I] <> '&') do
    begin
      if (LBody[I] = '=') and (LEqPos = 0) then
        LEqPos := I;
      Inc(I);
    end;

    if LEqPos > 0 then
    begin
      LName := Copy(LBody, LStart, LEqPos - LStart);
      LValue := Copy(LBody, LEqPos + 1, I - LEqPos - 1);
      
      if Pos('%', LName) > 0 then
        LName := {$IF DEFINED(FPC)}HTTPDecode(LName){$ELSE}TNetEncoding.URL.Decode(LName){$ENDIF};
      if Pos('%', LValue) > 0 then
        LValue := {$IF DEFINED(FPC)}HTTPDecode(LValue){$ELSE}TNetEncoding.URL.Decode(LValue){$ENDIF};
        
      ADest.Add(LName + '=' + LValue);
    end
    else
    begin
      LName := Copy(LBody, LStart, I - LStart);
      if Pos('%', LName) > 0 then
        LName := {$IF DEFINED(FPC)}HTTPDecode(LName){$ELSE}TNetEncoding.URL.Decode(LName){$ENDIF};
      ADest.Add(LName + '=');
    end;

    LStart := I + 1;
  end;
end;

procedure THttpSysRawRequest.PopulateCookieFields(ADest: TStrings);
var
  LCookies: string;
  I, LStart, LLen, LEqPos: Integer;
  LName, LValue, LPart: string;
begin
  LCookies := GetFieldByName('Cookie');
  if LCookies = '' then Exit;

  LStart := 1;
  LLen := Length(LCookies);
  while LStart <= LLen do
  begin
    I := LStart;
    while (I <= LLen) and (LCookies[I] <> ';') do
      Inc(I);

    LPart := Trim(Copy(LCookies, LStart, I - LStart));
    if LPart <> '' then
    begin
      LEqPos := Pos('=', LPart);
      if LEqPos > 0 then
      begin
        LName := Copy(LPart, 1, LEqPos - 1);
        LValue := Copy(LPart, LEqPos + 1, Length(LPart) - LEqPos);
        ADest.Add(LName + '=' + LValue);
      end;
    end;

    LStart := I + 1;
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
  LFileChunk: HTTP_DATA_CHUNK_FILE;
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
  LChunkBytesRead: Integer;
  LSendFlags: ULONG;
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

  if (ARes.ContentStream <> nil) and (ARes.ContentStream.Size > 0) then
  begin
    LContentLengthAnsi := FastIntToAnsiString(ARes.ContentStream.Size);
    LResponse.Headers.KnownHeaders[11].pRawValue := PAnsiChar(LContentLengthAnsi);
    LResponse.Headers.KnownHeaders[11].RawValueLength := Length(LContentLengthAnsi);

    if ARes.ContentStream is TFileStream then
    begin
      FillChar(LFileChunk, SizeOf(LFileChunk), 0);
      LFileChunk.DataChunkType := hctFromFileHandle;
      LFileChunk.StartingOffset := ARes.ContentStream.Position;
      LFileChunk.Length := ARes.ContentStream.Size - ARes.ContentStream.Position;
      LFileChunk.FileHandle := TFileStream(ARes.ContentStream).Handle;

      LResponse.EntityChunkCount := 1;
      LResponse.pEntityChunks := @LFileChunk;

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
    end
    else if ARes.ContentStream is TCustomMemoryStream then
    begin
      FillChar(LChunk, SizeOf(LChunk), 0);
      LChunk.DataChunkType := hctFromMemory;
      LChunk.pBuffer := TCustomMemoryStream(ARes.ContentStream).Memory;
      LChunk.BufferLength := ARes.ContentStream.Size - ARes.ContentStream.Position;

      LResponse.EntityChunkCount := 1;
      LResponse.pEntityChunks := @LChunk;

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
    end
    else
    begin
      LResponse.EntityChunkCount := 0;
      LResponse.pEntityChunks := nil;

      LRet := HttpSendHttpResponse(
        FReqQueue,
        FRequestId,
        HTTP_SEND_RESPONSE_FLAG_MORE_DATA,
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

      ARes.ContentStream.Position := 0;
      SetLength(LBodyBytes, 65536);
      while ARes.ContentStream.Position < ARes.ContentStream.Size do
      begin
        LChunkBytesRead := ARes.ContentStream.Read(LBodyBytes[0], Length(LBodyBytes));
        if LChunkBytesRead <= 0 then Break;

        FillChar(LChunk, SizeOf(LChunk), 0);
        LChunk.DataChunkType := hctFromMemory;
        LChunk.pBuffer := @LBodyBytes[0];
        LChunk.BufferLength := LChunkBytesRead;

        if ARes.ContentStream.Position < ARes.ContentStream.Size then
          LSendFlags := HTTP_SEND_RESPONSE_FLAG_MORE_DATA
        else
          LSendFlags := 0;

        LRet := HttpSendResponseEntityBody(
          FReqQueue,
          FRequestId,
          LSendFlags,
          1,
          @LChunk,
          LBytesSent,
          nil,
          nil,
          nil,
          nil
        );
        if LRet <> ERROR_SUCCESS then
          raise EOSError.Create('HttpSendResponseEntityBody failed with error code: ' + IntToStr(LRet));
      end;
    end;
  end
  else
  begin
    if ARes.BodyText <> '' then
      LBodyBytes := TEncoding.UTF8.GetBytes(ARes.BodyText)
    else if (ARes.RawWebResponse <> nil) and (ARes.RawWebResponse.Content <> '') then
      LBodyBytes := TEncoding.UTF8.GetBytes(ARes.RawWebResponse.Content);

    if Length(LBodyBytes) > 0 then
    begin
      LContentLengthAnsi := FastIntToAnsiString(Length(LBodyBytes));
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
var
  LData: PHttpSysWorkItemData;
begin
  {$IF DEFINED(FPC)}
  if Assigned(THttpSysThreadPool.Instance) then
    THttpSysThreadPool.Instance.QueueRequest(ABuffer);
  {$ELSE}
  New(LData);
  LData.Buffer := ABuffer;
  LData.ReqQueue := FReqQueue;
  if not QueueUserWorkItem(@HttpSysWorkItemCallback, LData, WT_EXECUTEDEFAULT) then
  begin
    THorseProviderHttpSys.BufferPool.Release(ABuffer);
    Dispose(LData);
  end;
  {$ENDIF}
end;

procedure THttpSysListenerThread.Execute;
var
  LBuffer: TBytes;
  LRequest: PHTTP_REQUEST;
  LBytesReturned: ULONG;
  LRet: ULONG;
  LRequestId: HTTP_REQUEST_ID;
  LCurrentSize: Integer;
begin
  LRequestId := 0;
  LBuffer := nil;
  LRequest := nil;
  LCurrentSize := 16384;
  while not Terminated and FRunning do
  begin
    if LRequestId = 0 then
    begin
      LBuffer := THorseProviderHttpSys.BufferPool.Acquire(LCurrentSize);
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
      // Dispatch processing concurrently
      DispatchRequest(LBuffer);
      LBuffer := nil;
      LRequestId := 0;
      LCurrentSize := 16384;
    end
    else if LRet = ERROR_MORE_DATA then
    begin
      // Re-allocate enough space for large request headers and try again
      LRequestId := LRequest.RequestId;
      LCurrentSize := LBytesReturned;
      SetLength(LBuffer, LCurrentSize);
      LRequest := PHTTP_REQUEST(@LBuffer[0]);
    end
    else
    begin
      LRequestId := 0;
      if LBuffer <> nil then
        THorseProviderHttpSys.BufferPool.Release(LBuffer);
      if (LRet = 1229) or (LRet = ERROR_CONNECTION_INVALID) then // 1229 = ERROR_CONNECTION_INVALID
        // Connection lost, continue
      else if not FRunning then
        Break;
    end;
  end;
  if LBuffer <> nil then
    THorseProviderHttpSys.BufferPool.Release(LBuffer);
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
  FBufferPool := THttpSysBufferPool.Create;

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
  FBufferPool.Free;
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
        {$IF DEFINED(FPC)}
        THttpSysThreadPool.FInstance := THttpSysThreadPool.Create(32);
        {$ENDIF}
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
        {$IF DEFINED(FPC)}
        if Assigned(THttpSysThreadPool.FInstance) then
        begin
          THttpSysThreadPool.FInstance.Free;
          THttpSysThreadPool.FInstance := nil;
        end;
        {$ENDIF}
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

  {$IF DEFINED(FPC)}
  if Assigned(THttpSysThreadPool.FInstance) then
  begin
    THttpSysThreadPool.FInstance.Free;
    THttpSysThreadPool.FInstance := nil;
  end;
  {$ENDIF}

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
