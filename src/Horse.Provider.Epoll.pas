unit Horse.Provider.Epoll;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
  {$DEFINE HORSE_EPOLL_SYNCHRONOUS}
{$ENDIF}
{$POINTERMATH ON}

interface

{$IFDEF LINUX}
uses
  {$IFDEF FPC}
    SysUtils,
    Classes,
    SyncObjs,
    Generics.Collections,
    BaseUnix,
    Unix,
    Linux,
    sockets,
    httpprotocol,
    Horse.Core.ByteSpan,
  {$ELSE}
    System.SysUtils,
    System.Classes,
    System.SyncObjs,
    System.Generics.Collections,
    System.Generics.Defaults,
    System.Threading,
    System.NetEncoding,
    Posix.Base,
    Posix.SysTypes,
    Posix.SysSocket,
    Posix.Unistd,
    Posix.Fcntl,
    Posix.ArpaInet,
    Posix.NetinetIn,
    Posix.Errno,
  {$ENDIF}
  Horse.Provider.Abstract,
  Horse.Provider.Config,
  Horse.Request,
  Horse.Response,
  Horse.Provider.RawInterfaces,
  Horse.Provider.RawAdapters,
  Horse.Proc,
  Horse.Commons,
  Horse.Core,
  Horse.Core.Router.Radix,
  Horse.Callback,
  Horse.Core.Router.Contract,
  Horse.Core.WebSocket,
  Horse.Provider.Socket.WebSocket;

type
  { Estrutura que representa os segmentos de cabeÃ§alhos indexados durante o
    parsing preguiÃ§oso para evitar alocaÃ§Ãµes desnecessÃ¡rias na heap. }
  TEpollConnectionContext = class;

  THeaderSegment = record
    KeyStart: Integer;
    KeyLen: Integer;
    ValueStart: Integer;
    ValueLen: Integer;
  end;

  THeaderSegments = TArray<THeaderSegment>;

  { Parser HTTP incremental e extremamente rÃ¡pido que opera diretamente sobre
    buffers de bytes, realizando Lazy Parsing nos cabeÃ§alhos. }
  THorseHttpParser = class
  private
    class function FindByte(const ABuffer: TBytes; AStart, AEnd: Integer; AByte: Byte): Integer; static; inline;
    class function FindCRLF(const ABuffer: TBytes; AStart, AEnd: Integer): Integer; static; inline;
    class function CompareBytesCI(const ABuffer: TBytes; AStart, ALen: Integer; const AStr: string): Boolean; static; inline;
    class function GetMethodString(const ABuffer: TBytes; AStart, ALen: Integer): string; static; inline;
  public
    class function TryParseRequest(
      const ABuffer: TBytes; 
      ALength: Integer;
      out AMethod: string;
      {$IFDEF FPC}
      out APathSpan: TByteSpan;
      out AQuerySpan: TByteSpan;
      out AVersionSpan: TByteSpan;
      {$ELSE}
      out APath: string;
      out AQuery: string;
      out AVersion: string;
      {$ENDIF}
      out AHeaders: THeaderSegments;
      out ABodyOffset: Integer;
      out AContentLength: Int64
    ): Boolean; static;
  end;

  TEpollReadOnlyBytesStream = class(TCustomMemoryStream)
  public
    constructor Create(const ABytes: TBytes; AOffset, ALen: Integer);
  end;

  { Adaptador de requisiÃ§Ã£o que implementa IHorseRawRequest e resolve os
    cabeÃ§alhos sob demanda (Lazy Loading). }
  TEpollRawRequest = class(TInterfacedObject, IHorseRawRequest)
  private
    FContext: TEpollConnectionContext;
    FBuffer: TBytes;
    FMethod: string;
    {$IFDEF FPC}
    FPathSpan: TByteSpan;
    FQuerySpan: TByteSpan;
    FVersionSpan: TByteSpan;
    FPathCache: string;
    FQueryCache: string;
    FVersionCache: string;
    {$ELSE}
    FPath: string;
    FQuery: string;
    FVersion: string;
    {$ENDIF}
    FBodyOffset: Integer;
    FContentLength: Int64;
    FHeaders: THeaderSegments;
    FBodyStream: TStream;
    FTempFileName: string;
    FClientIP: string;
    FClientPort: Integer;
    procedure EnsureBodyStream;
    function ResolveHeader(const AName: string): string;
  public
    constructor Create(
      var ABuffer: TBytes;
      const AMethod: string;
      {$IFDEF FPC}
      APathSpan, AQuerySpan, AVersionSpan: TByteSpan;
      {$ELSE}
      const APath, AQuery, AVersion: string;
      {$ENDIF}
      AHeaders: THeaderSegments;
      ABodyOffset: Integer;
      AContentLength: Int64;
      ABodyStream: TStream;
      const ATempFileName: string;
      const AClientIP: string;
      AClientPort: Integer
    );
    destructor Destroy; override;

    // Interface redirects
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
    // Interface redirects (cont)
    function GetFieldByName(const AName: string): string;

    procedure PopulateHeaders(ADest: TStrings);
    procedure PopulateQueryFields(ADest: TStrings);
    procedure PopulateContentFields(ADest: TStrings);
    procedure PopulateCookieFields(ADest: TStrings);

    function ReadBody(var Buffer; Count: Integer): Integer;
  end;

  { Adaptador de resposta que envia status, cabeÃ§alhos e corpo diretamente
    atravÃ©s do socket do cliente Linux. }
  TEpollRawResponse = class(TInterfacedObject, IHorseRawResponse)
  private
    FContext: TEpollConnectionContext;
    FSocket: Integer;
    FHeadersSent: Boolean;
    FStatusCode: Integer;
    FReason: string;
    FIsKeepAlive: Boolean;
    procedure SendHeaders(AHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF}; const AContentLength: string = ''; const AContentType: string = '');
    procedure SendStreamResponse(AStream: TStream; AHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF});
    function WriteNonBlocking(ABuffer: Pointer; ALength: Integer): Boolean;
    function WriteNonBlockingV(AIovCnt: Integer; AIov: Pointer): Boolean;
  public
    constructor Create(AContext: TEpollConnectionContext; AIsKeepAlive: Boolean = True);
    destructor Destroy; override;
    procedure SetCustomHeader(const AName, AValue: string);
    procedure SendResponse(const ARes: THorseResponse);
  end;

  { Unit de conexÃ£o cliente bÃ¡sica associada ao epoll }
  TEpollConnectionContext = class
  public
    Socket: Integer;
    EpollFd: Integer;
    Buffer: TBytes;
    BytesReceived: Integer;
    Method: string;
    {$IFDEF FPC}
    PathSpan: TByteSpan;
    QuerySpan: TByteSpan;
    VersionSpan: TByteSpan;
    {$ELSE}
    Path: string;
    Query: string;
    Version: string;
    {$ENDIF}
    Headers: THeaderSegments;
    BodyOffset: Integer;
    ContentLength: Int64;
    LastActive: Int64;
    FBodyStream: TStream;
    FTempFileName: string;
    FChunked: Boolean;
    FChunkState: Integer;
    FChunkSize: Int64;
    FChunkRemaining: Int64;
    FChunkLineBytes: TBytes;
    FChunkLineLen: Integer;
    FIsKeepAlive: Boolean;
    FProcessing: Boolean;
    FClosed: Integer;
    ClientIP: string;
    ClientPort: Integer;
    FWriteBuffer: TBytes;
    FWriteOffset: Integer;
    FWriteLen: Integer;

    constructor Create(ASocket: Integer);
    destructor Destroy; override;
    procedure ClearRequest;
    procedure WriteToBodyStream(const ABuffer: TBytes; AOffset, ALength: Integer);
    function ProcessChunkedBytes(const ABuffer: TBytes; AOffset, ALength: Integer): Boolean;
  end;



  { Thread worker que escuta seu prÃ³prio descritor de epoll (SO_REUSEPORT) }
  THorseEpollWorker = class(TThread)
  private
    FListenSocket: Integer;
    FEpollFd: Integer;
    {$IFDEF FPC}
    FPipeFds: TFilDes;
    {$ELSE}
    FPipeFds: array[0..1] of Integer;
    {$ENDIF}
    FRunning: Boolean;
    FConnections: TList<TEpollConnectionContext>;
    FConnectionsSync: TCriticalSection;
    FListenContext: TEpollConnectionContext;
    FPipeContext: TEpollConnectionContext;
    procedure ProcessClientRead(AContext: TEpollConnectionContext);
    procedure ProcessClientWrite(AContext: TEpollConnectionContext);
    procedure CloseConnection(AContext: TEpollConnectionContext);
    procedure CheckTimeouts;
  protected
    procedure Execute; override;
  public
    constructor Create(AListenSocket: Integer);
    destructor Destroy; override;
    procedure TerminateWorker;
  end;

  { Classe de Provider concreta do Horse para Linux baseada no Epoll }
  THorseProviderEpoll = class(THorseProviderAbstract)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FListenSockets: TList<Integer>;
    class var FWorkers: TObjectList<THorseEpollWorker>;

    class procedure SetPort(const AValue: Integer); static;
    class procedure SetHost(const AValue: string); static;
    class function GetPort: Integer; static;
    class function GetHost: string; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;

    class procedure InternalListen;
    class procedure InternalStopListen;
    class function CreateListenSocket(const APort: Integer; const AHost: string): Integer; static;
  public
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0'; const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallbackListen: TProc; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: TProc; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure ListenWithConfig(const APort: Integer; const AConfig: THorseCrossSocketConfig); override;
    class function GetActivePort: Integer; override;
    class procedure StopListen; override;
    class function IsRunning: Boolean;

    class constructor CreateClass;
    class destructor DestroyClass;
  end;

  THorseProvider = class(THorseProviderEpoll);

{$ENDIF}

implementation

{$IFDEF LINUX}

uses
  Horse.Core.MemoryBufferPool;

function FastFindHeaderEndAndContentLength(const ABuffer: TBytes; ABytesReceived: Integer; out ABodyOffset: Integer; out AContentLength: Int64): Boolean;
var
  I, J: Integer;
  B: Byte;
  LFound: Boolean;
  LVal: Int64;
begin
  Result := False;
  ABodyOffset := -1;
  AContentLength := 0;
  if ABytesReceived < 4 then
    Exit;
  I := 0;
  LFound := False;
  while I <= ABytesReceived - 4 do
  begin
    if (ABuffer[I] = 13) and (ABuffer[I + 1] = 10) and (ABuffer[I + 2] = 13) and (ABuffer[I + 3] = 10) then
    begin
      ABodyOffset := I + 4;
      LFound := True;
      Break;
    end;
    Inc(I);
  end;
  if not LFound then
    Exit;

  I := 0;
  while I <= ABodyOffset - 18 do
  begin
    B := ABuffer[I];
    if (B = 99) or (B = 67) then
    begin
      if ((ABuffer[I + 1] = 111) or (ABuffer[I + 1] = 79)) and 
         ((ABuffer[I + 2] = 110) or (ABuffer[I + 2] = 78)) and 
         ((ABuffer[I + 3] = 116) or (ABuffer[I + 3] = 84)) and 
         ((ABuffer[I + 4] = 101) or (ABuffer[I + 4] = 69)) and 
         ((ABuffer[I + 5] = 110) or (ABuffer[I + 5] = 78)) and 
         ((ABuffer[I + 6] = 116) or (ABuffer[I + 6] = 84)) and 
         (ABuffer[I + 7] = 45) and 
         ((ABuffer[I + 8] = 108) or (ABuffer[I + 8] = 76)) and 
         ((ABuffer[I + 9] = 101) or (ABuffer[I + 9] = 69)) and 
         ((ABuffer[I + 10] = 110) or (ABuffer[I + 10] = 78)) and 
         ((ABuffer[I + 11] = 103) or (ABuffer[I + 11] = 71)) and 
         ((ABuffer[I + 12] = 116) or (ABuffer[I + 12] = 84)) and 
         ((ABuffer[I + 13] = 104) or (ABuffer[I + 13] = 72)) then
      begin
        J := I + 14;
        while (J < ABodyOffset) and ((ABuffer[J] = 32) or (ABuffer[J] = 58) or (ABuffer[J] = 9)) do
          Inc(J);
        LVal := 0;
        while (J < ABodyOffset) and (ABuffer[J] >= 48) and (ABuffer[J] <= 57) do
        begin
          LVal := LVal * 10 + (ABuffer[J] - 48);
          Inc(J);
        end;
        AContentLength := LVal;
        Break;
      end;
    end;
    Inc(I);
  end;
  Result := True;
end;

function FastIsChunked(const ABuffer: TBytes; ABodyOffset: Integer): Boolean;
var
  I, J: Integer;
  B: Byte;
begin
  Result := False;
  I := 0;
  while I <= ABodyOffset - 25 do
  begin
    B := ABuffer[I];
    if (B = 116) or (B = 84) then
    begin
      if ((ABuffer[I + 1] = 114) or (ABuffer[I + 1] = 82)) and 
         ((ABuffer[I + 2] = 97) or (ABuffer[I + 2] = 65)) and 
         ((ABuffer[I + 3] = 110) or (ABuffer[I + 3] = 78)) and 
         ((ABuffer[I + 4] = 115) or (ABuffer[I + 4] = 83)) and 
         ((ABuffer[I + 5] = 102) or (ABuffer[I + 5] = 70)) and 
         ((ABuffer[I + 6] = 101) or (ABuffer[I + 6] = 69)) and 
         ((ABuffer[I + 7] = 114) or (ABuffer[I + 7] = 82)) and 
         (ABuffer[I + 8] = 45) and 
         ((ABuffer[I + 9] = 101) or (ABuffer[I + 9] = 69)) and 
         ((ABuffer[I + 10] = 110) or (ABuffer[I + 10] = 78)) and 
         ((ABuffer[I + 11] = 99) or (ABuffer[I + 11] = 67)) and 
         ((ABuffer[I + 12] = 111) or (ABuffer[I + 12] = 79)) and 
         ((ABuffer[I + 13] = 100) or (ABuffer[I + 13] = 68)) and 
         ((ABuffer[I + 14] = 105) or (ABuffer[I + 14] = 73)) and 
         ((ABuffer[I + 15] = 110) or (ABuffer[I + 15] = 78)) and 
         ((ABuffer[I + 16] = 103) or (ABuffer[I + 16] = 71)) then
      begin
        J := I + 17;
        while (J < ABodyOffset) and ((ABuffer[J] = 32) or (ABuffer[J] = 58) or (ABuffer[J] = 9)) do
          Inc(J);
        if (J + 7 <= ABodyOffset) and 
           ((ABuffer[J] = 99) or (ABuffer[J] = 67)) and 
           ((ABuffer[J + 1] = 104) or (ABuffer[J + 1] = 72)) and 
           ((ABuffer[J + 2] = 117) or (ABuffer[J + 2] = 85)) and 
           ((ABuffer[J + 3] = 110) or (ABuffer[J + 3] = 78)) and 
           ((ABuffer[J + 4] = 107) or (ABuffer[J + 4] = 75)) and 
           ((ABuffer[J + 5] = 101) or (ABuffer[J + 5] = 69)) and 
           ((ABuffer[J + 6] = 100) or (ABuffer[J + 6] = 68)) then
        begin
          Exit(True);
        end;
      end;
    end;
    Inc(I);
  end;
end;

{$IFDEF FPC}
type
  TEpollFPCTask = class
  public
    Context: TEpollConnectionContext;
    KeepAlive: Boolean;
    EpollFd: Integer;
    Worker: TThread;
    constructor Create(AContext: TEpollConnectionContext; AKeepAlive: Boolean; AEpollFd: Integer; AWorker: TThread);
  end;

  TEpollFPCWorkerThread = class(TThread)
  private
    FPool: TObject;
  protected
    procedure Execute; override;
  public
    constructor Create(APool: TObject);
  end;

  TEpollFPCTaskPool = class
  private
    FTasks: array[0..2047] of TEpollFPCTask;
    FHead: Integer;
    FTail: Integer;
    FWorkers: TList<TEpollFPCWorkerThread>;
    FEvent: TEvent;
    FActive: Boolean;
    function DequeueTask: TEpollFPCTask;
  public
    constructor Create(AThreadCount: Integer);
    destructor Destroy; override;
    procedure QueueTask(ATask: TEpollFPCTask);
  end;

var
  GTaskPool: TEpollFPCTaskPool = nil;
{$ENDIF}

const
  EPOLLIN      = $00000001;
  EPOLLOUT     = $00000004;
  EPOLLERR     = $00000008;
  EPOLLHUP     = $00000010;
  EPOLLRDHUP   = $00002000;
  EPOLLET      = $80000000;
  EPOLLONESHOT = $40000000;

  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_DEL = 2;
  EPOLL_CTL_MOD = 3;

  SO_REUSEPORT = 15;
  TCP_DEFER_ACCEPT = 9;
  TCP_NODELAY = 1;

  EAGAIN = 11;
  EWOULDBLOCK = 11;
  EINTR = 4;

type
  epoll_data = record
    case Integer of
      0: (ptr: Pointer);
      1: (fd: Integer);
      2: (u32: Cardinal);
      3: (u64: UInt64);
  end;

  epoll_event = packed record
    events: Cardinal;
    data: epoll_data;
  end;
  pepoll_event = ^epoll_event;

  iovec = record
    iov_base: Pointer;
    iov_len: NativeUInt;
  end;
  piovec = ^iovec;

  TEpollFDSet = record
    fds_bits: array[0..31] of LongInt;
  end;
  PEpollFDSet = ^TEpollFDSet;

  TEpollTimeVal = record
    tv_sec: Int64;
    tv_usec: Int64;
  end;
  PEpollTimeVal = ^TEpollTimeVal;

{$IFDEF FPC}
function writev(fd: Integer; iov: piovec; iovcnt: Integer): NativeInt; cdecl; external 'libc' name 'writev';
{$ELSE}
function writev(fd: Integer; iov: piovec; iovcnt: Integer): NativeInt; cdecl; external libc name 'writev';
{$ENDIF}

{$IFDEF FPC}
function select(nfds: Integer; readfds: PEpollFDSet; writefds: PEpollFDSet; exceptfds: PEpollFDSet; timeout: PEpollTimeVal): Integer; cdecl; external 'libc' name 'select';
{$ELSE}
function select(nfds: Integer; readfds: PEpollFDSet; writefds: PEpollFDSet; exceptfds: PEpollFDSet; timeout: PEpollTimeVal): Integer; cdecl; external libc name 'select';
{$ENDIF}

{$IFNDEF FPC}
const
  RLIMIT_NOFILE = 7;

type
  rlimit = record
    rlim_cur: UInt64;
    rlim_max: UInt64;
  end;
  prlimit = ^rlimit;

  TEpollTimeSpec = record
    tv_sec: Int64;
    tv_nsec: Int64;
  end;

function epoll_create1(flags: Integer): Integer; cdecl; external libc name 'epoll_create1';
function epoll_ctl(epfd: Integer; op: Integer; fd: Integer; event: pepoll_event): Integer; cdecl; external libc name 'epoll_ctl';
function epoll_wait(epfd: Integer; events: pepoll_event; maxevents: Integer; timeout: Integer): Integer; cdecl; external libc name 'epoll_wait';
function pipe(filedes: PInteger): Integer; cdecl; external libc name 'pipe';
function setrlimit(resource: Integer; const rlim: rlimit): Integer; cdecl; external libc name 'setrlimit';
function clock_gettime(clk_id: Integer; var tp: TEpollTimeSpec): Integer; cdecl; external libc name 'clock_gettime';

{$ENDIF}

{$IFDEF FPC}
const
  libc = 'libc.so.6';
{$ENDIF}

function sendfile(out_fd: Integer; in_fd: Integer; offset: PInt64; count: NativeUInt): NativeInt; cdecl; external libc name 'sendfile';

{$IFNDEF FPC}
function GetTickCount64: Int64;
var
  LTime: TEpollTimeSpec;
begin
  if clock_gettime(1, LTime) = 0 then
    Result := (LTime.tv_sec * 1000) + (LTime.tv_nsec div 1000000)
  else
    Result := 0;
end;
{$ENDIF}

procedure Epoll_FD_ZERO(var fds: TEpollFDSet); inline;
begin
  FillChar(fds, SizeOf(fds), 0);
end;

procedure Epoll_FD_SET(fd: Integer; var fds: TEpollFDSet); inline;
begin
  if (fd >= 0) and (fd < 1024) then
    fds.fds_bits[fd div 32] := fds.fds_bits[fd div 32] or (1 shl (fd mod 32));
end;

function SendAllV(ASocket: Integer; AIov: piovec; AIovCnt: Integer): Boolean;
var
  LWritten: NativeInt;
  LTotalBytes: NativeUInt;
  I: Integer;
  LFDSet: TEpollFDSet;
  LTimeVal: TEpollTimeVal;
  LRet: Integer;
  LErr: Integer;
begin
  Result := False;
  LTotalBytes := 0;
  for I := 0 to AIovCnt - 1 do
    Inc(LTotalBytes, AIov[I].iov_len);

  while LTotalBytes > 0 do
  begin
    LWritten := writev(ASocket, AIov, AIovCnt);
    if LWritten >= 0 then
    begin
      Dec(LTotalBytes, LWritten);
      if LTotalBytes = 0 then
      begin
        Result := True;
        Exit;
      end;
      
      while (AIovCnt > 0) and (LWritten >= NativeInt(AIov^.iov_len)) do
      begin
        Dec(LWritten, AIov^.iov_len);
        Inc(AIov);
        Dec(AIovCnt);
      end;
      if AIovCnt > 0 then
      begin
        AIov^.iov_base := Pointer(PByte(AIov^.iov_base) + LWritten);
        Dec(AIov^.iov_len, LWritten);
      end;
      Continue;
    end;

    {$IFDEF FPC}
    LErr := fpGetErrno;
    {$ELSE}
    LErr := errno;
    {$ENDIF}

    if (LErr = EAGAIN) or (LErr = EWOULDBLOCK) or (LErr = EINTR) then
    begin
      if LErr = EINTR then
        Continue;

      Epoll_FD_ZERO(LFDSet);
      Epoll_FD_SET(ASocket, LFDSet);
      LTimeVal.tv_sec := 5;
      LTimeVal.tv_usec := 0;
      
      LRet := select(ASocket + 1, nil, @LFDSet, nil, @LTimeVal);
      if LRet <= 0 then
        Exit;
    end
    else
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function SendAll(ASocket: Integer; ABuffer: Pointer; ALength: Integer): Boolean;
var
  LIov: iovec;
begin
  if ALength <= 0 then
    Exit(True);
  LIov.iov_base := ABuffer;
  LIov.iov_len := ALength;
  Result := SendAllV(ASocket, @LIov, 1);
end;

{$IFDEF FPC}
{ TEpollFPCTask }

constructor TEpollFPCTask.Create(AContext: TEpollConnectionContext; AKeepAlive: Boolean; AEpollFd: Integer; AWorker: TThread);
begin
  inherited Create;
  Context := AContext;
  KeepAlive := AKeepAlive;
  EpollFd := AEpollFd;
  Worker := AWorker;
end;

{ TEpollFPCWorkerThread }

constructor TEpollFPCWorkerThread.Create(APool: TObject);
begin
  inherited Create(False);
  FPool := APool;
  FreeOnTerminate := False;
end;

procedure TEpollFPCWorkerThread.Execute;
var
  LPool: TEpollFPCTaskPool;
  LTask: TEpollFPCTask;
  LHorseReq: THorseRequest;
  LHorseRes: THorseResponse;
  LLocalEvent: epoll_event;
  HasPendingWrite: Boolean;
  LMethod: string;
  LPathSpan, LQuerySpan, LVersionSpan: TByteSpan;
  LHeaders: THeaderSegments;
  LBodyOffset: Integer;
  LContentLength: Int64;
  LRawReq: IHorseRawRequest;
  LRawRes: IHorseRawResponse;
  LRawResObj: TEpollRawResponse;
  LWebRequest: TInterfacedWebRequest;
  LWebResponse: TInterfacedWebResponse;
  LConnHeader: string;
begin
  LPool := TEpollFPCTaskPool(FPool);
  
  try
    while not Terminated and LPool.FActive do
    begin
      LTask := LPool.DequeueTask;
      if LTask = nil then
      begin
        if Terminated or not LPool.FActive then
          Break;
        LPool.FEvent.WaitFor(1000);
        Continue;
      end;

      try
        try
          if THorseHttpParser.TryParseRequest(
            LTask.Context.Buffer,
            LTask.Context.BytesReceived,
            LMethod,
            LPathSpan,
            LQuerySpan,
            LVersionSpan,
            LHeaders,
            LBodyOffset,
            LContentLength
          ) then
          begin
            LTask.Context.Method := LMethod;
            LTask.Context.PathSpan := LPathSpan;
            LTask.Context.QuerySpan := LQuerySpan;
            LTask.Context.VersionSpan := LVersionSpan;
            LTask.Context.Headers := LHeaders;
            LTask.Context.BodyOffset := LBodyOffset;
            LTask.Context.ContentLength := LContentLength;
          end;

          LRawReq := TEpollRawRequest.Create(
            LTask.Context.Buffer,
            LTask.Context.Method,
            LTask.Context.PathSpan,
            LTask.Context.QuerySpan,
            LTask.Context.VersionSpan,
            LTask.Context.Headers,
            LTask.Context.BodyOffset,
            LTask.Context.ContentLength,
            LTask.Context.FBodyStream,
            LTask.Context.FTempFileName,
            LTask.Context.ClientIP,
            LTask.Context.ClientPort
          );
          LTask.Context.Buffer := nil;
          LTask.Context.FBodyStream := nil;
          LTask.Context.FTempFileName := '';

          LConnHeader := LRawReq.GetFieldByName('connection');
          if SameText(LRawReq.GetProtocolVersion, 'HTTP/1.1') then
            LTask.KeepAlive := not SameText(LConnHeader, 'close')
          else
            LTask.KeepAlive := SameText(LConnHeader, 'keep-alive');

          LRawResObj := TEpollRawResponse.Create(LTask.Context, LTask.KeepAlive);
          LRawRes := LRawResObj;
          LWebRequest := TInterfacedWebRequest.Create(LRawReq);
          LWebResponse := TInterfacedWebResponse.Create(LRawRes);

          try
            LHorseReq := THorseRequest.Create(LWebRequest);
            LHorseRes := THorseResponse.Create(nil);
            LHorseRes.SetCSRawWebResponse(LWebResponse);
            if LTask.Context <> nil then
            begin
              LHorseReq.Services.Add(THorseWebSocketUpgrader,
                THorseWebSocketSocketUpgrader.Create(
                  LTask.Context.Socket,
                  LHorseReq.WebSocketKey,
                  LTask.Context.ClientIP,
                  LTask.Context.ClientPort
                ), True);
            end;
            try
              THorseProviderEpoll.Execute(LHorseReq, LHorseRes);
            finally
              LRawResObj.SendResponse(LHorseRes);
              LHorseReq.Free;
              LHorseRes.Free;
            end;
          finally
            LWebRequest.Free;
          end;

          HasPendingWrite := False;
          if LTask.Context <> nil then
            HasPendingWrite := LTask.Context.FWriteLen > 0;

          if not HasPendingWrite then
          begin
            if LTask.KeepAlive then
            begin
              LTask.Context.ClearRequest;
              SetLength(LTask.Context.Buffer, 8192);
              LTask.Context.FIsKeepAlive := True;
              LTask.Context.BytesReceived := 0;
              LTask.Context.FProcessing := False;

              LLocalEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
              LLocalEvent.data.ptr := LTask.Context;
              epoll_ctl(LTask.EpollFd, EPOLL_CTL_MOD, LTask.Context.Socket, @LLocalEvent);
            end
            else
            begin
              THorseEpollWorker(LTask.Worker).CloseConnection(LTask.Context);
            end;
          end;
        except
          on E: Exception do
          begin
            try
              if (LTask <> nil) and (LTask.Context <> nil) and (LTask.Worker <> nil) then
                THorseEpollWorker(LTask.Worker).CloseConnection(LTask.Context);
            except
            end;
          end;
        end;
      finally
        LTask.Free;
      end;
    end;
  finally
  end;
end;

{ TEpollFPCTaskPool }

constructor TEpollFPCTaskPool.Create(AThreadCount: Integer);
var
  I: Integer;
begin
  inherited Create;
  FActive := True;
  FHead := 0;
  FTail := 0;
  FillChar(FTasks, SizeOf(FTasks), 0);
  FWorkers := TList<TEpollFPCWorkerThread>.Create;
  FEvent := TEvent.Create(nil, False, False, '');
  
  for I := 1 to AThreadCount do
    FWorkers.Add(TEpollFPCWorkerThread.Create(Self));
end;

destructor TEpollFPCTaskPool.Destroy;
var
  I: Integer;
  LTask: TEpollFPCTask;
begin
  FActive := False;
  FEvent.SetEvent;
  
  for I := 0 to FWorkers.Count - 1 do
    FWorkers[I].Terminate;
  
  FEvent.SetEvent;
  
  for I := 0 to FWorkers.Count - 1 do
  begin
    FWorkers[I].WaitFor;
    FWorkers[I].Free;
  end;
  FWorkers.Free;

  for I := 0 to 2047 do
  begin
    LTask := FTasks[I];
    if LTask <> nil then
      LTask.Free;
  end;
  
  FEvent.Free;
  inherited;
end;

procedure TEpollFPCTaskPool.QueueTask(ATask: TEpollFPCTask);
var
  LTail, LNextTail, LHead: Integer;
begin
  if not FActive then
  begin
    ATask.Free;
    Exit;
  end;
  
  LTail := FTail;
  LNextTail := (LTail + 1) and 2047;
  LHead := FHead;
  
  if LNextTail = LHead then
  begin
    ATask.Free;
    Exit;
  end;
  
  FTasks[LTail] := ATask;
  FTail := LNextTail;
  FEvent.SetEvent;
end;

function TEpollFPCTaskPool.DequeueTask: TEpollFPCTask;
var
  LHead, LTail, LNextHead: Integer;
  LTask: TEpollFPCTask;
begin
  Result := nil;
  while FActive do
  begin
    LHead := FHead;
    LTail := FTail;
    
    if LHead = LTail then
      Exit;
      
    LTask := FTasks[LHead];
    if LTask = nil then
      Continue;
      
    LNextHead := (LHead + 1) and 2047;
    if InterlockedCompareExchange(FHead, LNextHead, LHead) = LHead then
    begin
      FTasks[LHead] := nil;
      Result := LTask;
      Exit;
    end;
  end;
end;
{$ENDIF}



{ TEpollConnectionContext }

constructor TEpollConnectionContext.Create(ASocket: Integer);
begin
  inherited Create;
  Socket := ASocket;
  SetLength(Buffer, 8192);
  BytesReceived := 0;
  Headers := nil;
  FBodyStream := nil;
  FTempFileName := '';
  FChunked := False;
  FChunkState := 0;
  FChunkSize := 0;
  FChunkRemaining := 0;
  SetLength(FChunkLineBytes, 256);
  FChunkLineLen := 0;
  FIsKeepAlive := False;
  FProcessing := False;
  FClosed := 0;
  ClientIP := '';
  ClientPort := 0;
  FWriteBuffer := nil;
  FWriteOffset := 0;
  FWriteLen := 0;
  LastActive := GetTickCount64;

  ClearRequest;
end;

destructor TEpollConnectionContext.Destroy;
begin

  Headers := nil;
  Buffer := nil;
  FWriteBuffer := nil;
  if Assigned(FBodyStream) then
    FBodyStream.Free;
  if FTempFileName <> '' then
    DeleteFile(FTempFileName);
  inherited;
end;

procedure TEpollConnectionContext.ClearRequest;
begin
  Method := '';
  {$IFDEF FPC}
  PathSpan := TByteSpan.Create(0, 0);
  QuerySpan := TByteSpan.Create(0, 0);
  VersionSpan := TByteSpan.Create(0, 0);
  {$ELSE}
  Path := '';
  Query := '';
  Version := '';
  {$ENDIF}
  Headers := nil;
  BodyOffset := -1;
  ContentLength := 0;
  FBodyStream := nil;
  FTempFileName := '';
  FChunked := False;
  FChunkState := 0;
  FChunkSize := 0;
  FChunkRemaining := 0;
  FChunkLineLen := 0;
  FWriteBuffer := nil;
  FWriteOffset := 0;
  FWriteLen := 0;
end;

procedure TEpollConnectionContext.WriteToBodyStream(const ABuffer: TBytes; AOffset, ALength: Integer);
var
  LTempPath, LTempFile: string;
  LFileStream: TFileStream;
begin
  if FBodyStream = nil then
  begin
    if (ContentLength > 0) and (ContentLength >= 2097152) then
    begin
      LTempPath := '/tmp/';
      LTempFile := LTempPath + 'horse_spool_' + IntToStr(GetTickCount64) + '_' + IntToStr(Socket) + '.tmp';
      FTempFileName := LTempFile;
      FBodyStream := TFileStream.Create(LTempFile, fmCreate or fmOpenWrite);
    end
    else
    begin
      FBodyStream := THorseMemoryBufferPool.DefaultPool.AcquireStream;
    end;
  end;

  if not (FBodyStream is TFileStream) and (FBodyStream.Size + ALength >= 2097152) then
  begin
    LTempPath := '/tmp/';
    LTempFile := LTempPath + 'horse_spool_' + IntToStr(GetTickCount64) + '_' + IntToStr(Socket) + '.tmp';
    FTempFileName := LTempFile;
    LFileStream := TFileStream.Create(LTempFile, fmCreate or fmOpenWrite);
    try
      if FBodyStream.Size > 0 then
      begin
        FBodyStream.Position := 0;
        LFileStream.CopyFrom(FBodyStream, FBodyStream.Size);
      end;
      FBodyStream.Free;
      FBodyStream := LFileStream;
    except
      LFileStream.Free;
      DeleteFile(LTempFile);
      raise;
    end;
  end;

  FBodyStream.WriteBuffer(ABuffer[AOffset], ALength);
end;

function TEpollConnectionContext.ProcessChunkedBytes(const ABuffer: TBytes; AOffset, ALength: Integer): Boolean;
var
  I: Integer;
  LByte: Byte;
  LHexStr: string;
  LWriteCount: Integer;
begin
  Result := False;
  I := AOffset;
  while I < AOffset + ALength do
  begin
    case FChunkState of
      0: // Lendo o tamanho do chunk em Hexa
      begin
        LByte := ABuffer[I];
        Inc(I);
        
        if LByte = 10 then // '\n'
        begin
          if (FChunkLineLen > 0) and (FChunkLineBytes[FChunkLineLen - 1] = 13) then
            Dec(FChunkLineLen);
            
          if FChunkLineLen > 0 then
          begin
            LHexStr := Trim(TEncoding.ASCII.GetString(FChunkLineBytes, 0, FChunkLineLen));
            if LHexStr.Contains(';') then
              LHexStr := LHexStr.Split([';'])[0];
            FChunkSize := StrToInt64Def('$' + LHexStr, 0);
            FChunkRemaining := FChunkSize;
            FChunkLineLen := 0;
            
            if FChunkSize = 0 then
            begin
              FChunkState := 3; // Fim
              Result := True;
              Exit;
            end
            else
            begin
              FChunkState := 1;
            end;
          end
          else
          begin
            FChunkLineLen := 0;
          end;
        end
        else
        begin
          if FChunkLineLen < Length(FChunkLineBytes) then
          begin
            FChunkLineBytes[FChunkLineLen] := LByte;
            Inc(FChunkLineLen);
          end;
        end;
      end;
      
      1: // Lendo dados do chunk
      begin
        LWriteCount := AOffset + ALength - I;
        if LWriteCount > FChunkRemaining then
          LWriteCount := FChunkRemaining;
          
        if LWriteCount > 0 then
        begin
          WriteToBodyStream(ABuffer, I, LWriteCount);
          FChunkRemaining := FChunkRemaining - LWriteCount;
          I := I + LWriteCount;
        end;
        
        if FChunkRemaining = 0 then
          FChunkState := 2;
      end;
      
      2: // Aguardar CRLF
      begin
        LByte := ABuffer[I];
        Inc(I);
        if LByte = 10 then
          FChunkState := 0;
      end;
      
      3: // Fim do chunked
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
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
    if (ABuffer[I] = 13) and (ABuffer[I+1] = 10) then
      Exit(I);
  Result := -1;
end;

class function THorseHttpParser.CompareBytesCI(const ABuffer: TBytes; AStart, ALen: Integer; const AStr: string): Boolean;
var
  I: Integer;
  B1, B2: Byte;
begin
  if ALen <> Length(AStr) then Exit(False);
  for I := 0 to ALen - 1 do
  begin
    B1 := ABuffer[AStart + I];
    B2 := Byte(AStr[I + 1]);
    if (B1 >= 65) and (B1 <= 90) then B1 := B1 + 32;
    if (B2 >= 65) and (B2 <= 90) then B2 := B2 + 32;
    if B1 <> B2 then Exit(False);
  end;
  Result := True;
end;

class function THorseHttpParser.GetMethodString(const ABuffer: TBytes; AStart, ALen: Integer): string;
begin
  case ALen of
    3:
      if (ABuffer[AStart] = 71) and (ABuffer[AStart+1] = 69) and (ABuffer[AStart+2] = 84) then
        Exit('GET')
      else if (ABuffer[AStart] = 80) and (ABuffer[AStart+1] = 85) and (ABuffer[AStart+2] = 84) then
        Exit('PUT');
    4:
      if (ABuffer[AStart] = 80) and (ABuffer[AStart+1] = 79) and (ABuffer[AStart+2] = 83) and (ABuffer[AStart+3] = 84) then
        Exit('POST')
      else if (ABuffer[AStart] = 72) and (ABuffer[AStart+1] = 69) and (ABuffer[AStart+2] = 65) and (ABuffer[AStart+3] = 68) then
        Exit('HEAD');
    5:
      if (ABuffer[AStart] = 80) and (ABuffer[AStart+1] = 65) and (ABuffer[AStart+2] = 84) and (ABuffer[AStart+3] = 67) and (ABuffer[AStart+4] = 72) then
        Exit('PATCH');
    6:
      if (ABuffer[AStart] = 68) and (ABuffer[AStart+1] = 69) and (ABuffer[AStart+2] = 76) and (ABuffer[AStart+3] = 69) and (ABuffer[AStart+4] = 84) and (ABuffer[AStart+5] = 69) then
        Exit('DELETE');
    7:
      if (ABuffer[AStart] = 79) and (ABuffer[AStart+1] = 80) and (ABuffer[AStart+2] = 84) and (ABuffer[AStart+3] = 73) and (ABuffer[AStart+4] = 79) and (ABuffer[AStart+5] = 78) and (ABuffer[AStart+6] = 83) then
        Exit('OPTIONS');
  end;
  Result := TEncoding.UTF8.GetString(ABuffer, AStart, ALen);
end;

class function THorseHttpParser.TryParseRequest(
  const ABuffer: TBytes; 
  ALength: Integer;
  out AMethod: string;
  {$IFDEF FPC}
  out APathSpan: TByteSpan;
  out AQuerySpan: TByteSpan;
  out AVersionSpan: TByteSpan;
  {$ELSE}
  out APath: string;
  out AQuery: string;
  out AVersion: string;
  {$ENDIF}
  out AHeaders: THeaderSegments;
  out ABodyOffset: Integer;
  out AContentLength: Int64
): Boolean;
var
  HeaderEnd: Integer;
  I: Integer;
  LineStart: Integer;
  LineEnd: Integer;
  Space1: Integer;
  Space2: Integer;
  QueryStart: Integer;
  Colon: Integer;
  Segment: THeaderSegment;
  SegCount: Integer;
begin
  AMethod := '';
  {$IFDEF FPC}
  APathSpan := TByteSpan.Create(0, 0);
  AQuerySpan := TByteSpan.Create(0, 0);
  AVersionSpan := TByteSpan.Create(0, 0);
  {$ELSE}
  APath := '';
  AQuery := '';
  AVersion := '';
  {$ENDIF}
  ABodyOffset := -1;
  AContentLength := 0;
  SetLength(AHeaders, 0);

  if ALength < 4 then
    Exit(False);

  // 1. Localiza o fim dos cabeÃ§alhos (\r\n\r\n)
  HeaderEnd := -1;
  for I := 0 to ALength - 4 do
  begin
    if (ABuffer[I] = 13) and (ABuffer[I+1] = 10) and (ABuffer[I+2] = 13) and (ABuffer[I+3] = 10) then
    begin
      HeaderEnd := I;
      Break;
    end;
  end;

  if HeaderEnd = -1 then
    Exit(False);

  // 2. Processa a linha inicial (Request Line)
  LineEnd := FindCRLF(ABuffer, 0, HeaderEnd);
  if LineEnd = -1 then Exit(False);

  Space1 := FindByte(ABuffer, 0, LineEnd, 32); // EspaÃ§o
  if Space1 = -1 then Exit(False);

  Space2 := FindByte(ABuffer, Space1 + 1, LineEnd, 32);
  if Space2 = -1 then Exit(False);

  // Method (cached/optimized)
  AMethod := GetMethodString(ABuffer, 0, Space1);
  
  QueryStart := FindByte(ABuffer, Space1 + 1, Space2, 63); // '?'
  {$IFDEF FPC}
  if QueryStart <> -1 then
  begin
    APathSpan := TByteSpan.Create(Space1 + 1, QueryStart - (Space1 + 1));
    AQuerySpan := TByteSpan.Create(QueryStart + 1, Space2 - (QueryStart + 1));
  end
  else
  begin
    APathSpan := TByteSpan.Create(Space1 + 1, Space2 - (Space1 + 1));
    AQuerySpan := TByteSpan.Create(0, 0);
  end;
  AVersionSpan := TByteSpan.Create(Space2 + 1, LineEnd - (Space2 + 1));
  {$ELSE}
  if QueryStart <> -1 then
  begin
    if (QueryStart - (Space1 + 1) = 1) and (ABuffer[Space1 + 1] = 47) then
      APath := '/'
    else
      APath := TEncoding.UTF8.GetString(ABuffer, Space1 + 1, QueryStart - (Space1 + 1));
    AQuery := TEncoding.UTF8.GetString(ABuffer, QueryStart + 1, Space2 - (QueryStart + 1));
  end
  else
  begin
    if (Space2 - (Space1 + 1) = 1) and (ABuffer[Space1 + 1] = 47) then
      APath := '/'
    else
      APath := TEncoding.UTF8.GetString(ABuffer, Space1 + 1, Space2 - (Space1 + 1));
    AQuery := '';
  end;
  AVersion := TEncoding.UTF8.GetString(ABuffer, Space2 + 1, LineEnd - (Space2 + 1));
  {$ENDIF}

  // 3. Processa os cabeÃ§alhos linha a linha indexando os offsets
  SegCount := 0;
  SetLength(AHeaders, 16);

  LineStart := LineEnd + 2;
  while LineStart < HeaderEnd do
  begin
    LineEnd := FindCRLF(ABuffer, LineStart, HeaderEnd);
    if LineEnd = -1 then LineEnd := HeaderEnd;

    if LineEnd > LineStart then
    begin
      Colon := FindByte(ABuffer, LineStart, LineEnd, 58); // ':'
      if Colon <> -1 then
      begin
        Segment.KeyStart := LineStart;
        Segment.KeyLen := Colon - LineStart;
        
        // Remove espaÃ§os do valor (Trim rÃ¡pido dos bytes)
        I := Colon + 1;
        while (I < LineEnd) and (ABuffer[I] = 32) do Inc(I);
        Segment.ValueStart := I;
        Segment.ValueLen := LineEnd - I;

        if SegCount >= Length(AHeaders) then
          SetLength(AHeaders, SegCount + 8);

        AHeaders[SegCount] := Segment;
        Inc(SegCount);

        // Parsing rÃ¡pido de Content-Length diretamente dos bytes
        if CompareBytesCI(ABuffer, Segment.KeyStart, Segment.KeyLen, 'content-length') then
        begin
          AContentLength := 0;
          for I := 0 to Segment.ValueLen - 1 do
          begin
            if (ABuffer[Segment.ValueStart + I] >= 48) and (ABuffer[Segment.ValueStart + I] <= 57) then
              AContentLength := AContentLength * 10 + (ABuffer[Segment.ValueStart + I] - 48);
          end;
        end;
      end;
    end;

    LineStart := LineEnd + 2;
  end;

  SetLength(AHeaders, SegCount);
  ABodyOffset := HeaderEnd + 4;
  Result := True;
end;

{ TEpollReadOnlyBytesStream }

constructor TEpollReadOnlyBytesStream.Create(const ABytes: TBytes; AOffset, ALen: Integer);
begin
  inherited Create;
  if ALen > 0 then
    SetPointer(@ABytes[AOffset], ALen)
  else
    SetPointer(nil, 0);
end;

{ TEpollRawRequest }

constructor TEpollRawRequest.Create(
  var ABuffer: TBytes;
  const AMethod: string;
  {$IFDEF FPC}
  APathSpan, AQuerySpan, AVersionSpan: TByteSpan;
  {$ELSE}
  const APath, AQuery, AVersion: string;
  {$ENDIF}
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
  FContext := nil;
  if ABodyOffset > 0 then
  begin
    if (ABodyStream = nil) and (ATempFileName = '') and (AContentLength > 0) then
      FBuffer := Copy(ABuffer, 0, ABodyOffset + AContentLength)
    else
      FBuffer := Copy(ABuffer, 0, ABodyOffset);
  end
  else
    FBuffer := nil;

  ABuffer := nil;

  FMethod := AMethod;
  {$IFDEF FPC}
  FPathSpan := APathSpan;
  FQuerySpan := AQuerySpan;
  FVersionSpan := AVersionSpan;
  FPathCache := '';
  FQueryCache := '';
  FVersionCache := '';
  {$ELSE}
  FPath := APath;
  FQuery := AQuery;
  FVersion := AVersion;
  {$ENDIF}
  FHeaders := AHeaders;
  FBodyOffset := ABodyOffset;
  FContentLength := AContentLength;
  
  if (ABodyStream = nil) and (ATempFileName = '') and (AContentLength > 0) then
    FBodyStream := TEpollReadOnlyBytesStream.Create(FBuffer, FBodyOffset, FContentLength)
  else
    FBodyStream := ABodyStream;

  FTempFileName := ATempFileName;
  FClientIP := AClientIP;
  FClientPort := AClientPort;
end;

destructor TEpollRawRequest.Destroy;
begin
  if Assigned(FBodyStream) then
    FBodyStream.Free;
  if FTempFileName <> '' then
    DeleteFile(FTempFileName);
  inherited;
end;

procedure TEpollRawRequest.EnsureBodyStream;
begin
  if FBodyStream = nil then
    FBodyStream := TEpollReadOnlyBytesStream.Create(nil, 0, 0);
end;

function TEpollRawRequest.ResolveHeader(const AName: string): string;
var
  LSegment: THeaderSegment;
  LRawVal: AnsiString;
  I: Integer;
begin
  for I := 0 to Length(FHeaders) - 1 do
  begin
    LSegment := FHeaders[I];
    if THorseHttpParser.CompareBytesCI(FBuffer, LSegment.KeyStart, LSegment.KeyLen, AName) then
    begin
      if (LSegment.ValueLen > 0) and (LSegment.ValueStart >= 0) and (LSegment.ValueStart + LSegment.ValueLen <= Length(FBuffer)) then
      begin
        SetString(LRawVal, PAnsiChar(@FBuffer[LSegment.ValueStart]), LSegment.ValueLen);
        Result := string(LRawVal);
      end
      else
        Result := '';
      Exit;
    end;
  end;
  Result := '';
end;

function TEpollRawRequest.GetMethod: string; begin Result := FMethod; end;
function TEpollRawRequest.GetProtocolVersion: string;
begin
  {$IFDEF FPC}
  if FVersionCache = '' then
    FVersionCache := FVersionSpan.ToString(FBuffer);
  Result := FVersionCache;
  {$ELSE}
  Result := FVersion;
  {$ENDIF}
end;

function TEpollRawRequest.GetURL: string;
begin
  {$IFDEF FPC}
  if FPathCache = '' then
    FPathCache := FPathSpan.ToString(FBuffer);
  if FQueryCache = '' then
    FQueryCache := FQuerySpan.ToString(FBuffer);
  if FQueryCache <> '' then
    Result := FPathCache + '?' + FQueryCache
  else
    Result := FPathCache;
  {$ELSE}
  if FQuery <> '' then
    Result := FPath + '?' + FQuery
  else
    Result := FPath;
  {$ENDIF}
end;

function TEpollRawRequest.GetPathInfo: string;
begin
  {$IFDEF FPC}
  if FPathCache = '' then
    FPathCache := FPathSpan.ToString(FBuffer);
  Result := FPathCache;
  {$ELSE}
  Result := FPath;
  {$ENDIF}
end;

function TEpollRawRequest.GetQueryString: string;
begin
  {$IFDEF FPC}
  if FQueryCache = '' then
    FQueryCache := FQuerySpan.ToString(FBuffer);
  Result := FQueryCache;
  {$ELSE}
  Result := FQuery;
  {$ENDIF}
end;
function TEpollRawRequest.GetHost: string; begin Result := ResolveHeader('host'); end;
function TEpollRawRequest.GetRemoteAddr: string; begin Result := FClientIP; end;
// Retorna a porta na qual o servidor estÃ¡ ouvindo localmente de forma dinÃ¢mica
function TEpollRawRequest.GetServerPort: Integer; begin Result := THorseProviderEpoll.Port; end;
function TEpollRawRequest.GetContentType: string; begin Result := ResolveHeader('content-type'); end;

function TEpollRawRequest.GetContent: string;
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
function TEpollRawRequest.GetContentLength: Integer;
begin
  Result := FContentLength;
end;
{$ELSE}
  {$IF CompilerVersion >= 32.0}
  function TEpollRawRequest.GetContentLength: Int64;
  begin
    Result := FContentLength;
  end;
  {$ELSE}
  function TEpollRawRequest.GetContentLength: Integer;
  begin
    Result := FContentLength;
  end;
  {$ENDIF}
{$ENDIF}

function TEpollRawRequest.GetFieldByName(const AName: string): string;
begin
  Result := ResolveHeader(AName);
end;

procedure TEpollRawRequest.PopulateHeaders(ADest: TStrings);
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

procedure TEpollRawRequest.PopulateQueryFields(ADest: TStrings);
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

procedure TEpollRawRequest.PopulateContentFields(ADest: TStrings);
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

procedure TEpollRawRequest.PopulateCookieFields(ADest: TStrings);
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

function TEpollRawRequest.ReadBody(var Buffer; Count: Integer): Integer;
begin
  EnsureBodyStream;
  Result := FBodyStream.Read(Buffer, Count);
end;


{ TEpollRawResponse }

constructor TEpollRawResponse.Create(AContext: TEpollConnectionContext; AIsKeepAlive: Boolean);
begin
  inherited Create;
  FContext := AContext;
  FSocket := AContext.Socket;
  FHeadersSent := False;
  FStatusCode := 200;
  FReason := 'OK';
  FIsKeepAlive := AIsKeepAlive;
end;

destructor TEpollRawResponse.Destroy;
begin
  inherited;
end;

procedure TEpollRawResponse.SetCustomHeader(const AName, AValue: string);
begin
end;

procedure TEpollRawResponse.SendHeaders(AHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF}; const AContentLength: string; const AContentType: string);
var
  LHeaderStr: AnsiString;
  I: Integer;
  LHasContentType, LHasConnection, LHasContentLength: Boolean;
  {$IFNDEF FPC}
  LPair: TPair<string, string>;
  {$ENDIF}
begin
  if FHeadersSent then Exit;
  FHeadersSent := True;

  LHeaderStr := 'HTTP/1.1 ' + AnsiString(IntToStr(FStatusCode)) + ' ' + AnsiString(FReason) + #13#10;
  
  LHasContentType := False;
  LHasConnection := False;
  LHasContentLength := False;

  if AHeadersList <> nil then
  begin
    {$IFDEF FPC}
    for I := 0 to AHeadersList.Count - 1 do
    begin
      LHeaderStr := LHeaderStr + AnsiString(AHeadersList.Names[I]) + ': ' + AnsiString(AHeadersList.ValueFromIndex[I]) + #13#10;
      if SameText(AHeadersList.Names[I], 'Content-Type') then LHasContentType := True;
      if SameText(AHeadersList.Names[I], 'Connection') then LHasConnection := True;
      if SameText(AHeadersList.Names[I], 'Content-Length') then LHasContentLength := True;
    end;
    {$ELSE}
    for LPair in AHeadersList do
    begin
      LHeaderStr := LHeaderStr + AnsiString(LPair.Key) + ': ' + AnsiString(LPair.Value) + #13#10;
      if SameText(LPair.Key, 'Content-Type') then LHasContentType := True;
      if SameText(LPair.Key, 'Connection') then LHasConnection := True;
      if SameText(LPair.Key, 'Content-Length') then LHasContentLength := True;
    end;
    {$ENDIF}
  end;

  if not LHasContentType then
  begin
    { Fix A (streaming, re-derived onto merged 2026-07-17) — honour the
      Res.ContentType the handler set before SendStream. The stream writer passes
      FResponse.CSContentType here; without it a streamed response with no explicit
      Content-Type header defaulted to text/html (test 34). The non-stream path
      already reads CSContentType; this brings the streaming path in line. }
    if AContentType <> '' then
      LHeaderStr := LHeaderStr + 'Content-Type: ' + AnsiString(AContentType) + #13#10
    else
      LHeaderStr := LHeaderStr + 'Content-Type: text/html; charset=utf-8' + #13#10;
  end;

  if not LHasContentLength and (AContentLength <> '') then
    LHeaderStr := LHeaderStr + 'Content-Length: ' + AnsiString(AContentLength) + #13#10;

  if not LHasConnection then
  begin
    if FIsKeepAlive then
      LHeaderStr := LHeaderStr + 'Connection: keep-alive' + #13#10
    else
      LHeaderStr := LHeaderStr + 'Connection: close' + #13#10;
  end;

  LHeaderStr := LHeaderStr + #13#10;
  
  if Length(LHeaderStr) > 0 then
    WriteNonBlocking(@LHeaderStr[1], Length(LHeaderStr));
end;

function TEpollRawResponse.WriteNonBlocking(ABuffer: Pointer; ALength: Integer): Boolean;
var
  LIov: iovec;
begin
  if ALength <= 0 then
    Exit(True);
  LIov.iov_base := ABuffer;
  LIov.iov_len := ALength;
  Result := WriteNonBlockingV(1, @LIov);
end;

function TEpollRawResponse.WriteNonBlockingV(AIovCnt: Integer; AIov: Pointer): Boolean;
var
  LRes: NativeInt;
  LTotalBytes: NativeUInt;
  I: Integer;
  LErr: Integer;
  LWritten: NativeInt;
  LRemainderLen: Integer;
  LDestPos: Integer;
  LEvent: epoll_event;
  LIovBase: PByte;
  LIovs: piovec;
begin
  LIovs := piovec(AIov);
  Result := False;
  LTotalBytes := 0;
  for I := 0 to AIovCnt - 1 do
    Inc(LTotalBytes, LIovs[I].iov_len);

  if LTotalBytes = 0 then
    Exit(True);

  LRes := writev(FSocket, LIovs, AIovCnt);
  
  if LRes >= 0 then
  begin
    if NativeUInt(LRes) = LTotalBytes then
      Exit(True);
    
    LRemainderLen := LTotalBytes - LRes;
    SetLength(FContext.FWriteBuffer, LRemainderLen);
    LDestPos := 0;
    LWritten := LRes;

    for I := 0 to AIovCnt - 1 do
    begin
      if LWritten >= NativeInt(LIovs[I].iov_len) then
      begin
        Dec(LWritten, LIovs[I].iov_len);
      end
      else
      begin
        LIovBase := PByte(LIovs[I].iov_base) + LWritten;
        Move(LIovBase^, FContext.FWriteBuffer[LDestPos], LIovs[I].iov_len - LWritten);
        Inc(LDestPos, LIovs[I].iov_len - LWritten);
        LWritten := 0;
      end;
    end;

    FContext.FWriteOffset := 0;
    FContext.FWriteLen := LRemainderLen;

    FillChar(LEvent, SizeOf(LEvent), 0);
    LEvent.events := EPOLLOUT or EPOLLET or EPOLLONESHOT;
    LEvent.data.ptr := FContext;
    epoll_ctl(FContext.EpollFd, EPOLL_CTL_MOD, FSocket, @LEvent);
    Exit(True);
  end;

  {$IFDEF FPC}
  LErr := fpGetErrno;
  {$ELSE}
  LErr := errno;
  {$ENDIF}

  if (LErr = EAGAIN) or (LErr = EWOULDBLOCK) or (LErr = EINTR) then
  begin
    SetLength(FContext.FWriteBuffer, LTotalBytes);
    LDestPos := 0;
    for I := 0 to AIovCnt - 1 do
    begin
      if LIovs[I].iov_len > 0 then
      begin
        Move(LIovs[I].iov_base^, FContext.FWriteBuffer[LDestPos], LIovs[I].iov_len);
        Inc(LDestPos, LIovs[I].iov_len);
      end;
    end;

    FContext.FWriteOffset := 0;
    FContext.FWriteLen := LTotalBytes;

    FillChar(LEvent, SizeOf(LEvent), 0);
    LEvent.events := EPOLLOUT or EPOLLET or EPOLLONESHOT;
    LEvent.data.ptr := FContext;
    epoll_ctl(FContext.EpollFd, EPOLL_CTL_MOD, FSocket, @LEvent);
    Exit(True);
  end;

  Exit(False);
end;

procedure TEpollRawResponse.SendStreamResponse(AStream: TStream; AHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF});
var
  LUseChunked: Boolean;
  LChunkBuf: TBytes;
  LReadCount: Integer;
  LHexStr: string;
  LHexBytes: TBytes;
  LTermStr: string;
  LTermBytes: TBytes;
  LHasChunkedHeader: Boolean;
  LChunkIov: array[0..2] of iovec;
  {$IFDEF FPC}
  I: Integer;
  {$ENDIF}
  LFileHandle: THandle;
  LStreamSize: Int64;
begin
  LHasChunkedHeader := False;
  if AHeadersList <> nil then
  begin
    {$IFDEF FPC}
    for I := 0 to AHeadersList.Count - 1 do
      if SameText(AHeadersList.Names[I], 'Transfer-Encoding') and SameText(AHeadersList.ValueFromIndex[I], 'chunked') then
        LHasChunkedHeader := True;
    {$ELSE}
    LHasChunkedHeader := AHeadersList.ContainsKey('Transfer-Encoding') and SameText(AHeadersList.Items['Transfer-Encoding'], 'chunked');
    {$ENDIF}
  end;

  LStreamSize := AStream.Size - AStream.Position;

  if (not LHasChunkedHeader) and (AStream is TFileStream) then
  begin
    LFileHandle := TFileStream(AStream).Handle;
    if LStreamSize > 0 then
    begin
      SendHeaders(AHeadersList, IntToStr(LStreamSize));
      if sendfile(FSocket, LFileHandle, nil, LStreamSize) >= 0 then
        Exit;
    end;
  end;

  if (LStreamSize > 0) and (LStreamSize <= 10485760) then
  begin
    SendHeaders(AHeadersList, IntToStr(LStreamSize));
    if AStream is TCustomMemoryStream then
    begin
      WriteNonBlocking(TCustomMemoryStream(AStream).Memory, LStreamSize);
    end
    else
    begin
      SetLength(LChunkBuf, LStreamSize);
      AStream.ReadBuffer(LChunkBuf[0], LStreamSize);
      WriteNonBlocking(@LChunkBuf[0], LStreamSize);
    end;
    Exit;
  end;

  LUseChunked := LHasChunkedHeader or (AStream.Size >= 2097152) or (AStream.Size < 0);

  if LUseChunked then
  begin
    {$IFDEF FPC}
    if AHeadersList = nil then
      AHeadersList := TStringList.Create;
    AHeadersList.Add('Transfer-Encoding=chunked');
    {$ELSE}
    if AHeadersList = nil then
      AHeadersList := TDictionary<string, string>.Create;
    AHeadersList.AddOrSetValue('Transfer-Encoding', 'chunked');
    {$ENDIF}
  end;

  SendHeaders(AHeadersList);

  SetLength(LChunkBuf, 8192);
  while True do
  begin
    LReadCount := AStream.Read(LChunkBuf[0], Length(LChunkBuf));
    if LReadCount <= 0 then Break;

    if LUseChunked then
    begin
      LHexStr := Format('%x'#13#10, [LReadCount]);
      LHexBytes := TEncoding.ASCII.GetBytes(LHexStr);
      LChunkIov[0].iov_base := @LHexBytes[0];
      LChunkIov[0].iov_len := Length(LHexBytes);
      LChunkIov[1].iov_base := @LChunkBuf[0];
      LChunkIov[1].iov_len := LReadCount;
      LChunkIov[2].iov_base := PAnsiChar(#13#10);
      LChunkIov[2].iov_len := 2;
      if not WriteNonBlockingV(3, @LChunkIov[0]) then Break;
    end
    else
    begin
      if not WriteNonBlocking(@LChunkBuf[0], LReadCount) then Break;
    end;
  end;

  if LUseChunked then
  begin
    LTermStr := '0'#13#10#13#10;
    LTermBytes := TEncoding.ASCII.GetBytes(LTermStr);
    WriteNonBlocking(@LTermBytes[0], Length(LTermBytes));
    {$IFDEF FPC}
    if (AHeadersList <> nil) and (AHeadersList.Count = 1) and (AHeadersList.Names[0] = 'Transfer-Encoding') then
      AHeadersList.Free;
    {$ENDIF}
  end;
end;

procedure TEpollRawResponse.SendResponse(const ARes: THorseResponse);
var
  LBodyBytes: TBytes;
  LHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF};
  LContentType: string;
  LHeaderBytes: TBytes;
  LIov: array[0..1] of iovec;
  LHeaderStr: AnsiString;
  I: Integer;
  LHasContentType, LHasConnection: Boolean;
  { test 28 — adapter's own inherited CustomHeaders (RawWebResponse.SetCustomHeader) }
  LAdapterHeaders: TStrings;
  LAdapterIdx: Integer;
  { REPEATHDR-1 (test 10) — dup-preserving headers (Set-Cookie) }
  LRepeatHdrs: TStrings;
  LRepeatIdx: Integer;
  {$IFNDEF FPC}
  LPair: TPair<string, string>;
  {$ENDIF}
begin
  FStatusCode := ARes.Status;
  
  case FStatusCode of
    200: FReason := 'OK';
    201: FReason := 'Created';
    204: FReason := 'No Content';
    301: FReason := 'Moved Permanently';
    302: FReason := 'Found';
    400: FReason := 'Bad Request';
    401: FReason := 'Unauthorized';
    403: FReason := 'Forbidden';
    404: FReason := 'Not Found';
    500: FReason := 'Internal Server Error';
  else
    FReason := 'OK';
  end;

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

  LHeaderStr := 'HTTP/1.1 ' + AnsiString(IntToStr(FStatusCode)) + ' ' + AnsiString(FReason) + #13#10;
  
  LHasContentType := False;
  LHasConnection := False;

  LHeadersList := ARes.CustomHeaders;
  if LHeadersList <> nil then
  begin
    {$IFDEF FPC}
    for I := 0 to LHeadersList.Count - 1 do
    begin
      if not SameText(LHeadersList.Names[I], 'Set-Cookie') then
        LHeaderStr := LHeaderStr + AnsiString(LHeadersList.Names[I]) + ': ' + AnsiString(LHeadersList.ValueFromIndex[I]) + #13#10;
      if SameText(LHeadersList.Names[I], 'Content-Type') then LHasContentType := True;
      if SameText(LHeadersList.Names[I], 'Connection') then LHasConnection := True;
    end;
    {$ELSE}
    for LPair in LHeadersList do
    begin
      if not SameText(LPair.Key, 'Set-Cookie') then
        LHeaderStr := LHeaderStr + AnsiString(LPair.Key) + ': ' + AnsiString(LPair.Value) + #13#10;
      if SameText(LPair.Key, 'Content-Type') then LHasContentType := True;
      if SameText(LPair.Key, 'Connection') then LHasConnection := True;
    end;
    {$ENDIF}
  end;

  { test 28 (re-derived onto merged 2026-07-17) — headers set via
    Res.RawWebResponse.SetCustomHeader land on the adapter's OWN inherited
    CustomHeaders store, NOT on ARes.CustomHeaders (the shadow read above). On the
    adapter path FWebResponse is nil so the two stores are disjoint — emit the
    adapter's headers too or RawWebResponse.SetCustomHeader is dropped. Mirrors the
    IOCP REPEATHDR-1 union and the fork's validated Epoll. TStrings on both
    compilers (TWebResponse/TResponse.CustomHeaders), so no FPC/Delphi split. }
  if ARes.RawWebResponse <> nil then
    LAdapterHeaders := ARes.RawWebResponse.CustomHeaders
  else
    LAdapterHeaders := nil;
  if LAdapterHeaders <> nil then
    for LAdapterIdx := 0 to LAdapterHeaders.Count - 1 do
    begin
      if not SameText(LAdapterHeaders.Names[LAdapterIdx], 'Set-Cookie') then
        LHeaderStr := LHeaderStr + AnsiString(LAdapterHeaders.Names[LAdapterIdx]) + ': ' +
          AnsiString(LAdapterHeaders.ValueFromIndex[LAdapterIdx]) + #13#10;
      if SameText(LAdapterHeaders.Names[LAdapterIdx], 'Content-Type') then LHasContentType := True;
      if SameText(LAdapterHeaders.Names[LAdapterIdx], 'Connection') then LHasConnection := True;
    end;

  { REPEATHDR-1 (re-derived onto merged 2026-07-17, test 10) — emit duplicate-
    preserving headers (Set-Cookie) verbatim from ARes.RepeatHeaders. The shadow
    CustomHeaders dict collapses repeats to the last value (only user=tester
    survived; session=abc123 was lost), so Set-Cookie is skipped in the loops above
    and every occurrence is emitted here instead. RepeatHeaders stores Name=Value. }
  LRepeatHdrs := ARes.RepeatHeaders;
  if LRepeatHdrs <> nil then
    for LRepeatIdx := 0 to LRepeatHdrs.Count - 1 do
      LHeaderStr := LHeaderStr + AnsiString(LRepeatHdrs.Names[LRepeatIdx]) + ': ' +
        AnsiString(LRepeatHdrs.ValueFromIndex[LRepeatIdx]) + #13#10;

  if not LHasContentType then
  begin
    {$IFDEF FPC}
    LContentType := ARes.CSContentType;
    {$ELSE}
    if (ARes.RawWebResponse <> nil) and (ARes.RawWebResponse.ContentType <> '') then
      LContentType := ARes.RawWebResponse.ContentType
    else
      LContentType := ARes.CSContentType;
    {$ENDIF}
    if LContentType = '' then
      LContentType := 'text/html; charset=utf-8';
    LHeaderStr := LHeaderStr + 'Content-Type: ' + AnsiString(LContentType) + #13#10;
  end;

  LHeaderStr := LHeaderStr + 'Content-Length: ' + AnsiString(IntToStr(Length(LBodyBytes))) + #13#10;

  if not LHasConnection then
  begin
    if FIsKeepAlive then
      LHeaderStr := LHeaderStr + 'Connection: keep-alive' + #13#10
    else
      LHeaderStr := LHeaderStr + 'Connection: close' + #13#10;
  end;

  LHeaderStr := LHeaderStr + #13#10;
  
  SetLength(LHeaderBytes, Length(LHeaderStr));
  if Length(LHeaderStr) > 0 then
    Move(LHeaderStr[1], LHeaderBytes[0], Length(LHeaderStr));

  if not FHeadersSent then
  begin
    FHeadersSent := True;
    if Length(LBodyBytes) > 0 then
    begin
      LIov[0].iov_base := @LHeaderBytes[0];
      LIov[0].iov_len := Length(LHeaderBytes);
      LIov[1].iov_base := @LBodyBytes[0];
      LIov[1].iov_len := Length(LBodyBytes);
      WriteNonBlockingV(2, @LIov[0]);
    end
    else
    begin
      WriteNonBlocking(@LHeaderBytes[0], Length(LHeaderBytes));
    end;
  end
  else
  begin
    if Length(LBodyBytes) > 0 then
      WriteNonBlocking(@LBodyBytes[0], Length(LBodyBytes));
  end;
end;

{ THorseEpollWorker }

constructor THorseEpollWorker.Create(AListenSocket: Integer);
begin
  inherited Create(True);
  FListenSocket := AListenSocket;
  FRunning := False;
  FEpollFd := -1;
  FPipeFds[0] := -1;
  FPipeFds[1] := -1;
  FConnections := TList<TEpollConnectionContext>.Create;
  FConnectionsSync := TCriticalSection.Create;
  FListenContext := TEpollConnectionContext.Create(AListenSocket);
  FPipeContext := TEpollConnectionContext.Create(0);
  FreeOnTerminate := False;
end;

destructor THorseEpollWorker.Destroy;
begin
  TerminateWorker;
  while FConnections.Count > 0 do
    CloseConnection(FConnections[0]);
  FConnections.Free;
  FConnectionsSync.Free;
  FListenContext.Free;
  FPipeContext.Free;
  inherited;
end;

procedure THorseEpollWorker.TerminateWorker;
var
  LByte: Byte;
begin
  if not FRunning then Exit;
  FRunning := False;

  if FPipeFds[1] >= 0 then
  begin
    LByte := 1;
    {$IF DEFINED(FPC)}
    fpWrite(FPipeFds[1], LByte, 1);
    {$ELSE}
    __write(FPipeFds[1], @LByte, 1);
    {$ENDIF}
  end;

  Terminate;
  WaitFor;

  if FPipeFds[0] >= 0 then
  begin
    {$IF DEFINED(FPC)}fpClose(FPipeFds[0]);{$ELSE}__close(FPipeFds[0]);{$ENDIF}
    FPipeFds[0] := -1;
  end;
  if FPipeFds[1] >= 0 then
  begin
    {$IF DEFINED(FPC)}fpClose(FPipeFds[1]);{$ELSE}__close(FPipeFds[1]);{$ENDIF}
    FPipeFds[1] := -1;
  end;
  if FEpollFd >= 0 then
  begin
    {$IF DEFINED(FPC)}fpClose(FEpollFd);{$ELSE}__close(FEpollFd);{$ENDIF}
    FEpollFd := -1;
  end;
end;

procedure THorseEpollWorker.CloseConnection(AContext: TEpollConnectionContext);
begin
  if AContext = nil then Exit;
  {$IFDEF FPC}
  if InterlockedCompareExchange(AContext.FClosed, 1, 0) <> 0 then
  {$ELSE}
  if TInterlocked.CompareExchange(AContext.FClosed, 1, 0) <> 0 then
  {$ENDIF}
    Exit;

  FConnectionsSync.Enter;
  try
    FConnections.Remove(AContext);
  finally
    FConnectionsSync.Leave;
  end;
  epoll_ctl(FEpollFd, EPOLL_CTL_DEL, AContext.Socket, nil);
  {$IF DEFINED(FPC)}
  fpClose(AContext.Socket);
  {$ELSE}
  __close(AContext.Socket);
  {$ENDIF}
  AContext.Free;
end;

procedure THorseEpollWorker.ProcessClientRead(AContext: TEpollConnectionContext);
var
  LBytesRead: Integer;
  LRawReq: IHorseRawRequest;
  LRawRes: IHorseRawResponse;
  LRawResObj: TEpollRawResponse;
  LWebRequest: TInterfacedWebRequest;
  LWebResponse: TInterfacedWebResponse;
  {$IFDEF FPC}
  LReq: THorseRequest;
  LRes: THorseResponse;
  {$ENDIF}
  LEvent: epoll_event;
  LKeepAlive: Boolean;
  LConnHeader: string;
  LRequestComplete: Boolean;
  LExcessBytes: Integer;
  LIsChunked: Boolean;
  LSegment: THeaderSegment;
  LRawVal: AnsiString;
  LBodyReadBuf: TBytes;
  I: Integer;
  HasPendingWrite: Boolean;
  LMethod: string;
  {$IFDEF FPC}
  LPathSpan: TByteSpan;
  LQuerySpan: TByteSpan;
  LVersionSpan: TByteSpan;
  LStaticMatch: Boolean;
  LRadixRouter: THorseRadixRouter;
  LMethodType: TMethodType;
  LStaticCallbacks: TList<THorseCallback>;
  LFlow: TRadixFlow;
  {$ELSE}
  LPath: string;
  LQuery: string;
  LVersion: string;
  {$ENDIF}
  LHeaders: THeaderSegments;
  LBodyOffset: Integer;
  LContentLength: Int64;
  LWorker: THorseEpollWorker;
begin
  LWorker := Self;
  LRequestComplete := False;

  if AContext.BodyOffset = -1 then
  begin
    while True do
    begin
      {$IFDEF FPC}
      LBytesRead := fpRecv(
        AContext.Socket, 
        @AContext.Buffer[AContext.BytesReceived], 
        Length(AContext.Buffer) - AContext.BytesReceived, 
        0
      );
      {$ELSE}
      LBytesRead := recv(
        AContext.Socket, 
        AContext.Buffer[AContext.BytesReceived], 
        Length(AContext.Buffer) - AContext.BytesReceived, 
        0
      );
      {$ENDIF}

      if LBytesRead = 0 then
      begin
        CloseConnection(AContext);
        Exit;
      end;

      if LBytesRead < 0 then
      begin
        {$IFDEF FPC}
        if (fpGetErrno = 11) or (fpGetErrno = 11) then // EAGAIN/EWOULDBLOCK = 11
        {$ELSE}
        if (errno = EAGAIN) or (errno = EWOULDBLOCK) then
        {$ENDIF}
          Break
        else
        begin
          CloseConnection(AContext);
          Exit;
        end;
      end;

      AContext.BytesReceived := AContext.BytesReceived + LBytesRead;
      AContext.LastActive := GetTickCount64;

      if AContext.BytesReceived >= Length(AContext.Buffer) then
      begin
        if AContext.BytesReceived > 65536 then
        begin
          CloseConnection(AContext);
          Exit;
        end;
        SetLength(AContext.Buffer, Length(AContext.Buffer) * 2);
      end;
    end;

    if FastFindHeaderEndAndContentLength(AContext.Buffer, AContext.BytesReceived, LBodyOffset, LContentLength) then
    begin
      AContext.BodyOffset := LBodyOffset;
      AContext.ContentLength := LContentLength;
      LIsChunked := FastIsChunked(AContext.Buffer, AContext.BodyOffset);

      if LIsChunked then
      begin
        AContext.FChunked := True;
        LExcessBytes := AContext.BytesReceived - AContext.BodyOffset;
        if LExcessBytes > 0 then
          LRequestComplete := AContext.ProcessChunkedBytes(AContext.Buffer, AContext.BodyOffset, LExcessBytes)
        else
          LRequestComplete := False;
      end
      else
      begin
        AContext.FChunked := False;
        if AContext.ContentLength > 0 then
        begin
          LExcessBytes := AContext.BytesReceived - AContext.BodyOffset;
          if AContext.ContentLength >= 2097152 then
          begin
            if LExcessBytes > 0 then
              AContext.WriteToBodyStream(AContext.Buffer, AContext.BodyOffset, LExcessBytes);
          end
          else
          begin
            if Length(AContext.Buffer) < AContext.BodyOffset + AContext.ContentLength then
              SetLength(AContext.Buffer, AContext.BodyOffset + AContext.ContentLength);
          end;
          LRequestComplete := AContext.BytesReceived >= AContext.BodyOffset + AContext.ContentLength;
        end
        else
        begin
          LRequestComplete := True;
        end;
      end;
    end;
  end
  else
  begin
    if (not AContext.FChunked) and (AContext.ContentLength > 0) and (AContext.ContentLength < 2097152) then
    begin
      if Length(AContext.Buffer) < AContext.BodyOffset + AContext.ContentLength then
        SetLength(AContext.Buffer, AContext.BodyOffset + AContext.ContentLength);
        
      while True do
      begin
        {$IFDEF FPC}
        LBytesRead := fpRecv(
          AContext.Socket, 
          @AContext.Buffer[AContext.BytesReceived], 
          AContext.BodyOffset + AContext.ContentLength - AContext.BytesReceived, 
          0
        );
        {$ELSE}
        LBytesRead := recv(
          AContext.Socket, 
          AContext.Buffer[AContext.BytesReceived], 
          AContext.BodyOffset + AContext.ContentLength - AContext.BytesReceived, 
          0
        );
        {$ENDIF}

        if LBytesRead = 0 then
        begin
          CloseConnection(AContext);
          Exit;
        end;

        if LBytesRead < 0 then
        begin
          {$IFDEF FPC}
          if (fpGetErrno = 11) or (fpGetErrno = 11) then
          {$ELSE}
          if (errno = EAGAIN) or (errno = EWOULDBLOCK) then
          {$ENDIF}
            Break
          else
          begin
            CloseConnection(AContext);
            Exit;
          end;
        end;

        AContext.BytesReceived := AContext.BytesReceived + LBytesRead;
        AContext.LastActive := GetTickCount64;

        LRequestComplete := AContext.BytesReceived >= AContext.BodyOffset + AContext.ContentLength;
        if LRequestComplete then Break;
      end;
    end
    else
    begin
      SetLength(LBodyReadBuf, 8192);
      while True do
      begin
        {$IFDEF FPC}
        LBytesRead := fpRecv(AContext.Socket, PByte(LBodyReadBuf), 8192, 0);
        {$ELSE}
        LBytesRead := recv(AContext.Socket, PByte(LBodyReadBuf)^, 8192, 0);
        {$ENDIF}

        if LBytesRead = 0 then
        begin
          CloseConnection(AContext);
          Exit;
        end;

        if LBytesRead < 0 then
        begin
          {$IFDEF FPC}
          if (fpGetErrno = 11) or (fpGetErrno = 11) then
          {$ELSE}
          if (errno = EAGAIN) or (errno = EWOULDBLOCK) then
          {$ENDIF}
            Break
          else
          begin
            CloseConnection(AContext);
            Exit;
          end;
        end;

        AContext.LastActive := GetTickCount64;

        if AContext.FChunked then
        begin
          LRequestComplete := AContext.ProcessChunkedBytes(LBodyReadBuf, 0, LBytesRead);
          if LRequestComplete then Break;
        end
        else
        begin
          AContext.WriteToBodyStream(LBodyReadBuf, 0, LBytesRead);
          AContext.BytesReceived := AContext.BytesReceived + LBytesRead;
          LRequestComplete := AContext.BytesReceived >= AContext.BodyOffset + AContext.ContentLength;
          if LRequestComplete then Break;
        end;
      end;
    end;
  end;

  if LRequestComplete then
  begin
    if (AContext.FBodyStream = nil) and (AContext.FTempFileName <> '') then
      AContext.FBodyStream := THorseMemoryBufferPool.DefaultPool.AcquireStream;

    if AContext.FBodyStream <> nil then
      AContext.FBodyStream.Position := 0;

    AContext.FProcessing := True;

    {$IF NOT DEFINED(FPC)}
    // Delphi Linux: Executa rotas assincronamente no Task Parallel Library
    TTask.Run(
      procedure
      var
        LHorseReq: THorseRequest;
        LHorseRes: THorseResponse;
        LLocalEvent: epoll_event;
        LLocalEpollFd: Integer;
        LLocalContext: TEpollConnectionContext;
        LRawReq: IHorseRawRequest;
        LRawRes: IHorseRawResponse;
        LRawResObj: TEpollRawResponse;
        LWebRequest: TInterfacedWebRequest;
        LWebResponse: TInterfacedWebResponse;
        LKeepAlive: Boolean;
        LConnHeader: string;
        LMethod, LPath, LQuery, LVersion: string;
        LHeaders: THeaderSegments;
        LBodyOffset: Integer;
        LContentLength: Int64;
        HasPendingWrite: Boolean;
      begin
        LLocalEpollFd := LWorker.FEpollFd;
        LLocalContext := AContext;
        try
          if THorseHttpParser.TryParseRequest(
            LLocalContext.Buffer,
            LLocalContext.BytesReceived,
            LMethod,
            LPath,
            LQuery,
            LVersion,
            LHeaders,
            LBodyOffset,
            LContentLength
          ) then
          begin
            LLocalContext.Method := LMethod;
            LLocalContext.Path := LPath;
            LLocalContext.Query := LQuery;
            LLocalContext.Version := LVersion;
            LLocalContext.Headers := LHeaders;
            LLocalContext.BodyOffset := LBodyOffset;
            LLocalContext.ContentLength := LContentLength;
          end;

          LRawReq := TEpollRawRequest.Create(
            LLocalContext.Buffer,
            LLocalContext.Method,
            LLocalContext.Path,
            LLocalContext.Query,
            LLocalContext.Version,
            LLocalContext.Headers,
            LLocalContext.BodyOffset,
            LLocalContext.ContentLength,
            LLocalContext.FBodyStream,
            LLocalContext.FTempFileName,
            LLocalContext.ClientIP,
            LLocalContext.ClientPort
          );
          LLocalContext.Buffer := nil;
          LLocalContext.FBodyStream := nil;
          LLocalContext.FTempFileName := '';

          LConnHeader := LRawReq.GetFieldByName('connection');
          if SameText(LRawReq.GetProtocolVersion, 'HTTP/1.1') then
            LKeepAlive := not SameText(LConnHeader, 'close')
          else
            LKeepAlive := SameText(LConnHeader, 'keep-alive');

          LRawResObj := TEpollRawResponse.Create(LLocalContext, LKeepAlive);
          LRawRes := LRawResObj;
          LWebRequest := TInterfacedWebRequest.Create(LRawReq);
          LWebResponse := TInterfacedWebResponse.Create(LRawRes);

          try
            LHorseReq := THorseRequest.Create(LWebRequest);
            LHorseRes := THorseResponse.Create(nil);
            LHorseRes.SetCSRawWebResponse(LWebResponse);
            if LLocalContext <> nil then
            begin
              LHorseReq.Services.Add(THorseWebSocketUpgrader,
                THorseWebSocketSocketUpgrader.Create(
                  LLocalContext.Socket,
                  LHorseReq.WebSocketKey,
                  LLocalContext.ClientIP,
                  LLocalContext.ClientPort
                ), True);
            end;
            try
              THorseProviderEpoll.Execute(LHorseReq, LHorseRes);
            finally
              LRawResObj.SendResponse(LHorseRes);
              LHorseReq.Free;
              LHorseRes.Free;
            end;
          finally
            LWebRequest.Free;
          end;

          HasPendingWrite := False;
          if LLocalContext <> nil then
            HasPendingWrite := LLocalContext.FWriteLen > 0;

          if not HasPendingWrite then
          begin
            if LKeepAlive then
            begin
              LLocalContext.ClearRequest;
              SetLength(LLocalContext.Buffer, 8192);
              LLocalContext.FIsKeepAlive := True;
              LLocalContext.BytesReceived := 0;
              LLocalContext.FProcessing := False;

              LLocalEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
              LLocalEvent.data.ptr := LLocalContext;
              epoll_ctl(LLocalEpollFd, EPOLL_CTL_MOD, LLocalContext.Socket, @LLocalEvent);
            end
            else
            begin
              LWorker.CloseConnection(LLocalContext);
            end;
          end;
        except
          // Captura exceÃ§Ãµes para seguranÃ§a na thread
        end;
      end);
    {$ELSE}
    // Lazarus FPC: Despacha as rotas via GTaskPool de forma assÃ­ncrona
    {$IFNDEF HORSE_EPOLL_SYNCHRONOUS}
    if GTaskPool <> nil then
    begin
      GTaskPool.QueueTask(TEpollFPCTask.Create(
        AContext,
        True,
        FEpollFd,
        Self
      ));
    end
    else
    {$ENDIF}
    begin
      try
        if THorseHttpParser.TryParseRequest(
          AContext.Buffer,
          AContext.BytesReceived,
          LMethod,
          LPathSpan,
          LQuerySpan,
          LVersionSpan,
          LHeaders,
          LBodyOffset,
          LContentLength
        ) then
        begin
          AContext.Method := LMethod;
          AContext.PathSpan := LPathSpan;
          AContext.QuerySpan := LQuerySpan;
          AContext.VersionSpan := LVersionSpan;
          AContext.Headers := LHeaders;
          AContext.BodyOffset := LBodyOffset;
          AContext.ContentLength := LContentLength;
        end;

        // Fast Path de Roteamento Estático Zero-Allocation
        LStaticMatch := False;
        LRadixRouter := GActiveRadixRouter;

        if LRadixRouter <> nil then
        begin
          LMethodType := TMethodType.FromString(AContext.Method);
          if LRadixRouter.MatchStaticRoute(AContext.Buffer, AContext.PathSpan, LMethodType, LStaticCallbacks) then
          begin
            LStaticMatch := True;
            LRawReq := TEpollRawRequest.Create(
              AContext.Buffer,
              AContext.Method,
              AContext.PathSpan,
              AContext.QuerySpan,
              AContext.VersionSpan,
              AContext.Headers,
              AContext.BodyOffset,
              AContext.ContentLength,
              AContext.FBodyStream,
              AContext.FTempFileName,
              AContext.ClientIP,
              AContext.ClientPort
            );
            AContext.Buffer := nil;
            AContext.FBodyStream := nil;
            AContext.FTempFileName := '';

            LConnHeader := LRawReq.GetFieldByName('connection');
            if SameText(LRawReq.GetProtocolVersion, 'HTTP/1.1') then
              LKeepAlive := not SameText(LConnHeader, 'close')
            else
              LKeepAlive := SameText(LConnHeader, 'keep-alive');

            LRawResObj := TEpollRawResponse.Create(AContext, LKeepAlive);
            LRawRes := LRawResObj;
            LWebRequest := TInterfacedWebRequest.Create(LRawReq);
            LWebResponse := TInterfacedWebResponse.Create(LRawRes);

            try
              LReq := THorseRequest.Create(LWebRequest);
              LRes := THorseResponse.Create(nil);
              LRes.SetCSRawWebResponse(LWebResponse);
              try
                LFlow := TRadixFlow.Create(LStaticCallbacks, LReq, LRes);
                try
                  LFlow.Next;
                finally
                  LFlow.Free;
                end;
              finally
                LRawResObj.SendResponse(LRes);
                LReq.Free;
                LRes.Free;
              end;
            finally
              LWebRequest.Free;
            end;
          end;
        end;

        if not LStaticMatch then
        begin
          LRawReq := TEpollRawRequest.Create(
            AContext.Buffer,
            AContext.Method,
            AContext.PathSpan,
            AContext.QuerySpan,
            AContext.VersionSpan,
            AContext.Headers,
            AContext.BodyOffset,
            AContext.ContentLength,
            AContext.FBodyStream,
            AContext.FTempFileName,
            AContext.ClientIP,
            AContext.ClientPort
          );
          AContext.Buffer := nil;
          AContext.FBodyStream := nil;
          AContext.FTempFileName := '';

          LConnHeader := LRawReq.GetFieldByName('connection');
          if SameText(LRawReq.GetProtocolVersion, 'HTTP/1.1') then
            LKeepAlive := not SameText(LConnHeader, 'close')
          else
            LKeepAlive := SameText(LConnHeader, 'keep-alive');

          LRawResObj := TEpollRawResponse.Create(AContext, LKeepAlive);
          LRawRes := LRawResObj;
          LWebRequest := TInterfacedWebRequest.Create(LRawReq);
          LWebResponse := TInterfacedWebResponse.Create(LRawRes);

          try
            LReq := THorseRequest.Create(LWebRequest);
            LRes := THorseResponse.Create(nil);
            LRes.SetCSRawWebResponse(LWebResponse);
            if AContext <> nil then
            begin
              LReq.Services.Add(THorseWebSocketUpgrader,
                THorseWebSocketSocketUpgrader.Create(
                  AContext.Socket,
                  LReq.WebSocketKey,
                  AContext.ClientIP,
                  AContext.ClientPort
                ), True);
            end;
            try
              THorseProviderEpoll.Execute(LReq, LRes);
            finally
              LRawResObj.SendResponse(LRes);
              LReq.Free;
              LRes.Free;
            end;
          finally
            LWebRequest.Free;
          end;
        end;

        HasPendingWrite := False;
        if AContext <> nil then
          HasPendingWrite := AContext.FWriteLen > 0;

        if not HasPendingWrite then
        begin
          if LKeepAlive then
          begin
            AContext.ClearRequest;
            SetLength(AContext.Buffer, 8192);
            AContext.FIsKeepAlive := True;
            AContext.BytesReceived := 0;
            AContext.FProcessing := False;

            LEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
            LEvent.data.ptr := AContext;
            epoll_ctl(FEpollFd, EPOLL_CTL_MOD, AContext.Socket, @LEvent);
          end
          else
            CloseConnection(AContext);
        end;
      except
        // SeguranÃ§a no fallback sÃ­ncrono
      end;
    end;
    {$ENDIF}
  end
  else if AContext.BodyOffset <> -1 then
  begin
    LEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
    LEvent.data.ptr := AContext;
    epoll_ctl(FEpollFd, EPOLL_CTL_MOD, AContext.Socket, @LEvent);
  end
  else
  begin
    LEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
    LEvent.data.ptr := AContext;
    epoll_ctl(FEpollFd, EPOLL_CTL_MOD, AContext.Socket, @LEvent);
  end;
end;

procedure THorseEpollWorker.ProcessClientWrite(AContext: TEpollConnectionContext);
var
  LSent: NativeInt;
  LErr: Integer;
  LEvent: epoll_event;
begin
  if AContext = nil then Exit;

  {$IFDEF FPC}
  LSent := fpSend(AContext.Socket, PByte(AContext.FWriteBuffer) + AContext.FWriteOffset, AContext.FWriteLen, 0);
  {$ELSE}
  LSent := send(AContext.Socket, (PByte(AContext.FWriteBuffer) + AContext.FWriteOffset)^, AContext.FWriteLen, 0);
  {$ENDIF}

  if LSent > 0 then
  begin
    AContext.FWriteOffset := AContext.FWriteOffset + LSent;
    AContext.FWriteLen := AContext.FWriteLen - LSent;
    AContext.LastActive := GetTickCount64;

    if AContext.FWriteLen = 0 then
    begin
      AContext.FWriteBuffer := nil;
      if AContext.FIsKeepAlive then
      begin
        AContext.ClearRequest;
        AContext.BytesReceived := 0;
        AContext.FProcessing := False;

        LEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
        SetLength(AContext.Buffer, 8192);
        LEvent.data.ptr := AContext;
        epoll_ctl(FEpollFd, EPOLL_CTL_MOD, AContext.Socket, @LEvent);
      end
      else
      begin
        CloseConnection(AContext);
      end;
      Exit;
    end;
  end
  else if LSent < 0 then
  begin
    {$IFDEF FPC}
    LErr := fpGetErrno;
    {$ELSE}
    LErr := errno;
    {$ENDIF}

    if (LErr = EAGAIN) or (LErr = EWOULDBLOCK) or (LErr = EINTR) then
    begin
      // Continua monitorando para escrita
    end
    else
    begin
      CloseConnection(AContext);
      Exit;
    end;
  end
  else
  begin
    CloseConnection(AContext);
    Exit;
  end;

  LEvent.events := EPOLLOUT or EPOLLET or EPOLLONESHOT;
  LEvent.data.ptr := AContext;
  epoll_ctl(FEpollFd, EPOLL_CTL_MOD, AContext.Socket, @LEvent);
end;

procedure THorseEpollWorker.CheckTimeouts;
var
  LNow: Int64;
  I: Integer;
  LContext: TEpollConnectionContext;
  LExpired: TList<TEpollConnectionContext>;
begin
  LNow := GetTickCount64;
  LExpired := TList<TEpollConnectionContext>.Create;
  try
    FConnectionsSync.Enter;
    try
      for I := FConnections.Count - 1 downto 0 do
      begin
        LContext := FConnections[I];
        if LContext.FProcessing then
          Continue;

        if LContext.FIsKeepAlive and (LContext.BytesReceived = 0) then
        begin
          if LNow - LContext.LastActive > 60000 then
            LExpired.Add(LContext);
        end
        else
        begin
          if LNow - LContext.LastActive > 60000 then
            LExpired.Add(LContext);
        end;
      end;
    finally
      FConnectionsSync.Leave;
    end;

    for LContext in LExpired do
      CloseConnection(LContext);
  finally
    LExpired.Free;
  end;
end;

procedure THorseEpollWorker.Execute;
var
  {$IFDEF FPC}
  LAddr: TInetSockAddr;
  LAddrLen: TSockLen;
  {$ELSE}
  LAddr: sockaddr_in;
  LAddrLen: socklen_t;
  {$ENDIF}
  LEventCount: Integer;
  I: Integer;
  LEvents: array[0..255] of epoll_event;
  LEvent: epoll_event;
  LClientFd: Integer;
  LOptVal: Integer;
  LContext: TEpollConnectionContext;
  LLastTimeoutCheck: Int64;
  LCurrentTime: Int64;
begin
  {$IFDEF FPC}
  FEpollFd := epoll_create(1024);
  if FEpollFd < 0 then
  begin
    Writeln('Worker: epoll_create falhou com erro: ', fpGetErrno);
    Exit;
  end;
  {$ELSE}
  FEpollFd := epoll_create1(0);
  if FEpollFd < 0 then Exit;
  {$ENDIF}

  {$IFDEF FPC}
  Writeln('Worker: Criando pipe...');
  if fpPipe(FPipeFds) < 0 then
  begin
    Writeln('Worker: fpPipe falhou com erro: ', fpGetErrno);
    Exit;
  end;
  Writeln('Worker: Pipe criado. FPipeFds[0] = ', FPipeFds[0], ' FPipeFds[1] = ', FPipeFds[1]);
  fpFcntl(FPipeFds[0], F_SETFL, O_NONBLOCK);
  {$ELSE}
  if pipe(@FPipeFds[0]) < 0 then Exit;
  fcntl(FPipeFds[0], F_SETFL, O_NONBLOCK);
  {$ENDIF}

  FPipeContext.Socket := FPipeFds[0];
  LEvent.events := EPOLLIN;
  LEvent.data.ptr := FPipeContext;
  epoll_ctl(FEpollFd, EPOLL_CTL_ADD, FPipeFds[0], @LEvent);

  LEvent.events := EPOLLIN or EPOLLET;
  LEvent.data.ptr := FListenContext;
  epoll_ctl(FEpollFd, EPOLL_CTL_ADD, FListenSocket, @LEvent);

  FRunning := True;
  {$IFDEF FPC}
  Writeln('Worker: Thread iniciada. FListenSocket = ', FListenSocket, ' FEpollFd = ', FEpollFd);
  Flush(Output);
  {$ENDIF}
  LLastTimeoutCheck := GetTickCount64;

  try
    while not Terminated and FRunning do
    begin
      LEventCount := epoll_wait(FEpollFd, @LEvents[0], Length(LEvents), 1000);
      if LEventCount < 0 then
      begin
        {$IFDEF FPC}
        if fpGetErrno = 4 then Continue; // EINTR = 4
        Writeln('Worker: epoll_wait falhou com erro: ', fpGetErrno);
        Flush(Output);
        {$ELSE}
        if errno = EINTR then Continue;
        {$ENDIF}
        Break;
      end;

      for I := 0 to LEventCount - 1 do
      begin
        LEvent := LEvents[I];
        
        LContext := TEpollConnectionContext(LEvent.data.ptr);
        if LContext = FPipeContext then
        begin
          {$IFDEF FPC}
          Writeln('Worker: Sinal de parada recebido via pipe.');
          Flush(Output);
          {$ENDIF}
          Exit;
        end
        else if LContext = FListenContext then
        begin
          while True do
          begin
            LAddrLen := SizeOf(LAddr);
            {$IFDEF FPC}
            LClientFd := fpAccept(FListenSocket, psockaddr(@LAddr), @LAddrLen);
            {$ELSE}
            LClientFd := accept(FListenSocket, Psockaddr(@LAddr)^, LAddrLen);
            {$ENDIF}
            if LClientFd < 0 then
            begin
              {$IFDEF FPC}
              if (fpGetErrno = 11) or (fpGetErrno = 11) then // EAGAIN/EWOULDBLOCK = 11
              {$ELSE}
              if (errno = EAGAIN) or (errno = EWOULDBLOCK) then
              {$ENDIF}
                Break;
              Break;
            end;

            {$IFDEF FPC}
            fpFcntl(LClientFd, F_SETFL, O_NONBLOCK);
            {$ELSE}
            fcntl(LClientFd, F_SETFL, O_NONBLOCK);
            {$ENDIF}

            LOptVal := 1;
            {$IFDEF FPC}
            fpSetsockopt(LClientFd, IPPROTO_TCP, TCP_NODELAY, @LOptVal, SizeOf(LOptVal));
            {$ELSE}
            setsockopt(LClientFd, IPPROTO_TCP, TCP_NODELAY, LOptVal, SizeOf(LOptVal));
            {$ENDIF}

            LContext := TEpollConnectionContext.Create(LClientFd);
            LContext.EpollFd := FEpollFd;
            {$IFDEF FPC}
            LContext.ClientIP := NetAddrToStr(LAddr.sin_addr);
            LContext.ClientPort := ntohs(LAddr.sin_port);
            {$ELSE}
            LContext.ClientIP := string(AnsiString(inet_ntoa(LAddr.sin_addr)));
            LContext.ClientPort := ntohs(LAddr.sin_port);
            {$ENDIF}
            FConnectionsSync.Enter;
            try
              FConnections.Add(LContext);
            finally
              FConnectionsSync.Leave;
            end;

            LEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
            LEvent.data.ptr := LContext;
            epoll_ctl(FEpollFd, EPOLL_CTL_ADD, LClientFd, @LEvent);
          end;
        end
        else
        begin
          LContext := TEpollConnectionContext(LEvent.data.ptr);
          try
            if (LEvent.events and EPOLLOUT) <> 0 then
              ProcessClientWrite(LContext)
            else
              ProcessClientRead(LContext);
          except
            on E: Exception do
            begin
              {$IFDEF FPC}
              Writeln('Worker: Excecao ao processar leitura: ', E.ClassName, ' - ', E.Message, ' no endereco ', HexStr(ExceptAddr));
              Flush(Output);
              {$ENDIF}
              CloseConnection(LContext);
            end;
          end;
        end;
      end;

      LCurrentTime := GetTickCount64;
      if LCurrentTime - LLastTimeoutCheck > 1000 then
      begin
        CheckTimeouts;
        LLastTimeoutCheck := LCurrentTime;
      end;
    end;
  finally
    {$IFDEF FPC}Writeln('Worker: Thread encerrando.');{$ENDIF}
  end;
end;

{ THorseProviderEpoll }

class constructor THorseProviderEpoll.CreateClass;
var
  {$IFDEF FPC}
  LRLimit: TRLimit;
  {$ELSE}
  LRLimit: rlimit;
  {$ENDIF}
begin
  FPort := GetDefaultPort;
  FHost := GetDefaultHost;
  FListenSockets := TList<Integer>.Create;
  FWorkers := TObjectList<THorseEpollWorker>.Create(True);
  FRunning := False;

  // Eleva o limite mÃ¡ximo de descritores de arquivos abertos (ulimit -n) do processo para 65535
  LRLimit.rlim_cur := 65535;
  LRLimit.rlim_max := 65535;
  {$IFDEF FPC}
  fpSetrlimit(RLIMIT_NOFILE, @LRLimit);
  {$ELSE}
  setrlimit(RLIMIT_NOFILE, LRLimit);
  TThreadPool.Default.MaxWorkerThreads := 2048;
  TThreadPool.Default.MinWorkerThreads := TThread.ProcessorCount * 8;
  {$ENDIF}
end;

class destructor THorseProviderEpoll.DestroyClass;
begin
  InternalStopListen;
  FWorkers.Free;
  FListenSockets.Free;
end;

class function THorseProviderEpoll.GetDefaultHost: string;
begin
  Result := '0.0.0.0';
end;

class function THorseProviderEpoll.GetDefaultPort: Integer;
begin
  Result := 9095;
end;

class function THorseProviderEpoll.GetHost: string;
begin
  Result := FHost;
end;

class function THorseProviderEpoll.GetPort: Integer;
begin
  Result := FPort;
end;

class procedure THorseProviderEpoll.SetHost(const AValue: string);
begin
  FHost := AValue;
end;

class procedure THorseProviderEpoll.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

class function THorseProviderEpoll.IsRunning: Boolean;
begin
  Result := FRunning;
end;

class function THorseProviderEpoll.GetActivePort: Integer;
begin
  Result := FPort;
end;

class function THorseProviderEpoll.CreateListenSocket(const APort: Integer; const AHost: string): Integer;
var
  {$IFDEF FPC}
  LAddr: TInetSockAddr;
  {$ELSE}
  LAddr: sockaddr_in;
  {$ENDIF}
  LOptVal: Integer;
  LSocket: Integer;
const
  SO_REUSEPORT = 15;
begin
  {$IFDEF FPC}
  LSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  {$ELSE}
  LSocket := socket(AF_INET, SOCK_STREAM, 0);
  {$ENDIF}
  if LSocket < 0 then
    raise EOSError.Create('socket creation failed');

  LOptVal := 1;
  {$IFDEF FPC}
  fpSetsockopt(LSocket, SOL_SOCKET, SO_REUSEADDR, @LOptVal, SizeOf(LOptVal));
  fpSetsockopt(LSocket, SOL_SOCKET, SO_REUSEPORT, @LOptVal, SizeOf(LOptVal));
  fpFcntl(LSocket, F_SETFL, O_NONBLOCK);
  {$ELSE}
  setsockopt(LSocket, SOL_SOCKET, SO_REUSEADDR, LOptVal, SizeOf(LOptVal));
  setsockopt(LSocket, SOL_SOCKET, SO_REUSEPORT, LOptVal, SizeOf(LOptVal));
  fcntl(LSocket, F_SETFL, O_NONBLOCK);
  {$ENDIF}

  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  {$IFDEF FPC}
  LAddr.sin_port := htons(APort);
  if (AHost = '') or (AHost = '0.0.0.0') then
    LAddr.sin_addr := StrToNetAddr('0.0.0.0')
  else
    LAddr.sin_addr := StrToNetAddr(AHost);
  {$ELSE}
  LAddr.sin_port := htons(APort);
  if (AHost = '') or (AHost = '0.0.0.0') then
    LAddr.sin_addr.s_addr := INADDR_ANY
  else
    LAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(AHost)));
  {$ENDIF}

  {$IFDEF FPC}
  if fpBind(LSocket, psockaddr(@LAddr), SizeOf(LAddr)) < 0 then
  {$ELSE}
  if Posix.SysSocket.bind(LSocket, Psockaddr(@LAddr)^, SizeOf(LAddr)) < 0 then
  {$ENDIF}
  begin
    {$IFDEF FPC}fpClose(LSocket);{$ELSE}__close(LSocket);{$ENDIF}
    raise EOSError.Create('bind failed');
  end;

  {$IFDEF FPC}
  if fpListen(LSocket, 4096) < 0 then
  {$ELSE}
  if Posix.SysSocket.listen(LSocket, 4096) < 0 then
  {$ENDIF}
  begin
    {$IFDEF FPC}fpClose(LSocket);{$ELSE}__close(LSocket);{$ENDIF}
    raise EOSError.Create('listen failed');
  end;

  Result := LSocket;
end;

class procedure THorseProviderEpoll.InternalListen;
var
  I: Integer;
  LThreadCount: Integer;
  LWorker: THorseEpollWorker;
  LSocket: Integer;
begin
  TriggerBeforeListen;
  if FRunning then Exit;

  LThreadCount := TThread.ProcessorCount;
  if LThreadCount <= 0 then
    LThreadCount := 2;

  {$IFDEF FPC}
  {$IFNDEF HORSE_EPOLL_SYNCHRONOUS}
  if GTaskPool = nil then
    GTaskPool := TEpollFPCTaskPool.Create(LThreadCount * 8);
  {$ENDIF}
  {$ENDIF}

  try
    for I := 1 to LThreadCount do
    begin
      LSocket := CreateListenSocket(FPort, FHost);
      FListenSockets.Add(LSocket);
      LWorker := THorseEpollWorker.Create(LSocket);
      FWorkers.Add(LWorker);
      LWorker.Start;
    end;
    
    FRunning := True;
    DoOnListen;

    { [EPOLL-LISTEN-1] (re-derived onto merged 2026-07-17, upstream-PR candidate)
      Console apps expect THorse.Listen to BLOCK until StopListen — the contract
      every other provider honours (HttpSys/IOCP: `if IsConsole then while FRunning
      do Sleep`). Merged Epoll's InternalListen spawns worker threads and returns,
      so the test server printed its banner and exited immediately (workers alive
      but the main thread fell off the end → "Server stopped." at startup, nothing
      listening when a client connected). }
    if IsConsole then
      while FRunning do
        Sleep(100);
  except
    on E: Exception do
    begin
      InternalStopListen;
      raise;
    end;
  end;
end;

class procedure THorseProviderEpoll.InternalStopListen;
var
  I: Integer;
begin
  TriggerBeforeStop;
  if not FRunning then Exit;

  FRunning := False;

  {$IFDEF FPC}
  if GTaskPool <> nil then
  begin
    GTaskPool.Free;
    GTaskPool := nil;
  end;
  {$ENDIF}

  for I := 0 to FWorkers.Count - 1 do
    FWorkers[I].TerminateWorker;

  FWorkers.Clear;

  for I := 0 to FListenSockets.Count - 1 do
  begin
    {$IF DEFINED(FPC)}
    fpClose(FListenSockets[I]);
    {$ELSE}
    __close(FListenSockets[I]);
    {$ENDIF}
  end;
  
  FListenSockets.Clear;
  DoOnStopListen;
end;

class procedure THorseProviderEpoll.Listen;
begin
  InternalListen;
end;

class procedure THorseProviderEpoll.Listen(const APort: Integer; const ACallbackListen: TProc; const ACallbackStopListen: TProc);
begin
  SetPort(APort);
  OnListen := ACallbackListen;
  OnStopListen := ACallbackStopListen;
  InternalListen;
end;

class procedure THorseProviderEpoll.Listen(const APort: Integer; const AHost: string; const ACallbackListen: TProc; const ACallbackStopListen: TProc);
begin
  SetPort(APort);
  SetHost(AHost);
  OnListen := ACallbackListen;
  OnStopListen := ACallbackStopListen;
  InternalListen;
end;

class procedure THorseProviderEpoll.Listen(const AHost: string; const ACallbackListen: TProc; const ACallbackStopListen: TProc);
begin
  SetHost(AHost);
  OnListen := ACallbackListen;
  OnStopListen := ACallbackStopListen;
  InternalListen;
end;

class procedure THorseProviderEpoll.Listen(const ACallbackListen: TProc; const ACallbackStopListen: TProc);
begin
  OnListen := ACallbackListen;
  OnStopListen := ACallbackStopListen;
  InternalListen;
end;

class procedure THorseProviderEpoll.ListenWithConfig(const APort: Integer; const AConfig: THorseCrossSocketConfig);
begin
  SetPort(APort);
  InternalListen;
end;

class procedure THorseProviderEpoll.StopListen;
begin
  InternalStopListen;
end;

{ TEpollStreamWriter }

type
  TEpollStreamWriter = class(THorseStreamWriterBase)
  private
    FRawRes: TEpollRawResponse;
  protected
    procedure SendRawHeaders; override;
    procedure WriteRawBytes(const ABytes: TBytes); override;
  public
    constructor Create(const AResponse: THorseResponse); override;
    function IsConnected: Boolean; override;
  end;

constructor TEpollStreamWriter.Create(const AResponse: THorseResponse);
var
  LRawWebResponse: TObject;
begin
  inherited Create(AResponse);
  LRawWebResponse := AResponse.RawWebResponse;
  if Assigned(LRawWebResponse) and (LRawWebResponse is TInterfacedWebResponse) then
    FRawRes := TEpollRawResponse(TInterfacedWebResponse(LRawWebResponse).RawRes);
end;

procedure TEpollStreamWriter.SendRawHeaders;
var
  LHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF};
begin
  if not Assigned(FRawRes) then Exit;

  LHeadersList := FResponse.CustomHeaders;
  FRawRes.FStatusCode := FResponse.Status;

  case FRawRes.FStatusCode of
    200: FRawRes.FReason := 'OK';
    201: FRawRes.FReason := 'Created';
    204: FRawRes.FReason := 'No Content';
    301: FRawRes.FReason := 'Moved Permanently';
    302: FRawRes.FReason := 'Found';
    400: FRawRes.FReason := 'Bad Request';
    401: FRawRes.FReason := 'Unauthorized';
    403: FRawRes.FReason := 'Forbidden';
    404: FRawRes.FReason := 'Not Found';
    500: FRawRes.FReason := 'Internal Server Error';
  else
    FRawRes.FReason := 'OK';
  end;

  FRawRes.SendHeaders(LHeadersList, '', FResponse.CSContentType);
  FRawRes.FHeadersSent := True;
end;

procedure TEpollStreamWriter.WriteRawBytes(const ABytes: TBytes);
begin
  if Length(ABytes) = 0 then Exit;
  if Assigned(FRawRes) then
    FRawRes.WriteNonBlocking(@ABytes[0], Length(ABytes));
end;

function TEpollStreamWriter.IsConnected: Boolean;
begin
  Result := Assigned(FRawRes) and (FRawRes.FSocket >= 0);
end;

function EpollStreamWriterFactory(const AResponse: THorseResponse): IHorseStreamWriter;
begin
  Result := TEpollStreamWriter.Create(AResponse);
end;

initialization
  THorseResponse.RegisterStreamWriterFactory(EpollStreamWriterFactory);

{$ENDIF}

end.
