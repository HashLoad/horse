unit Horse.Provider.Epoll;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

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
  Horse.Commons;

type
  { Estrutura que representa os segmentos de cabeçalhos indexados durante o
    parsing preguiçoso para evitar alocações desnecessárias na heap. }
  THeaderSegment = record
    KeyStart: Integer;
    KeyLen: Integer;
    ValueStart: Integer;
    ValueLen: Integer;
  end;

  { Parser HTTP incremental e extremamente rápido que opera diretamente sobre
    buffers de bytes, realizando Lazy Parsing nos cabeçalhos. }
  THorseHttpParser = class
  private
    class function FindByte(const ABuffer: TBytes; AStart, AEnd: Integer; AByte: Byte): Integer; static; inline;
    class function FindCRLF(const ABuffer: TBytes; AStart, AEnd: Integer): Integer; static; inline;
    class function CompareBytesCI(const ABuffer: TBytes; AStart, ALen: Integer; const AStr: string): Boolean; static; inline;
  public
    class function TryParseRequest(
      const ABuffer: TBytes; 
      ALength: Integer;
      out AMethod: string;
      out APath: string;
      out AQuery: string;
      out AVersion: string;
      out AHeaders: TDictionary<string, THeaderSegment>;
      out ABodyOffset: Integer;
      out AContentLength: Int64
    ): Boolean; static;
  end;

  { Adaptador de requisição que implementa IHorseRawRequest e resolve os
    cabeçalhos sob demanda (Lazy Loading). }
  TEpollRawRequest = class(TInterfacedObject, IHorseRawRequest)
  private
    FBuffer: TBytes;
    FMethod: string;
    FPath: string;
    FQuery: string;
    FVersion: string;
    FBodyOffset: Integer;
    FContentLength: Int64;
    FHeaders: TDictionary<string, THeaderSegment>;
    FBodyStream: TStream;
    FTempFileName: string;
    FResolvedHeaders: TDictionary<string, string>;
    procedure EnsureBodyStream;
    function ResolveHeader(const AName: string): string;
  public
    constructor Create(
      const ABuffer: TBytes;
      const AMethod, APath, AQuery, AVersion: string;
      AHeaders: TDictionary<string, THeaderSegment>;
      ABodyOffset: Integer;
      AContentLength: Int64;
      ABodyStream: TStream;
      const ATempFileName: string
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

  { Adaptador de resposta que envia status, cabeçalhos e corpo diretamente
    através do socket do cliente Linux. }
  TEpollRawResponse = class(TInterfacedObject, IHorseRawResponse)
  private
    FSocket: Integer;
    FHeadersSent: Boolean;
    FStatusCode: Integer;
    FReason: string;
    FHeaders: TDictionary<string, string>;
    FIsKeepAlive: Boolean;
    function PrepareHeaders: TBytes;
    procedure SendHeaders;
    procedure SendStreamResponse(AStream: TStream; AHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF});
  public
    constructor Create(ASocket: Integer; AIsKeepAlive: Boolean = True);
    destructor Destroy; override;

    procedure SetCustomHeader(const AName, AValue: string);
    procedure SendResponse(const ARes: THorseResponse);
  end;

  { Unit de conexão cliente básica associada ao epoll }
  TEpollConnectionContext = class
  public
    Socket: Integer;
    Buffer: TBytes;
    BytesReceived: Integer;
    Method: string;
    Path: string;
    Query: string;
    Version: string;
    Headers: TDictionary<string, THeaderSegment>;
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
    constructor Create(ASocket: Integer);
    destructor Destroy; override;
    procedure ClearRequest;
    procedure WriteToBodyStream(const ABuffer: TBytes; AOffset, ALength: Integer);
    function ProcessChunkedBytes(const ABuffer: TBytes; AOffset, ALength: Integer): Boolean;
  end;

  { Pool de buffers estático e thread-safe }
  TBufferPool = class
  private
    const BUFFER_SIZE = 8192;
  public
    class constructor Create;
    class destructor Destroy;
    class function Acquire: TBytes;
    class procedure Release(var ABuffer: TBytes);
  end;

  { Thread worker que escuta seu próprio descritor de epoll (SO_REUSEPORT) }
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
    procedure ProcessClientRead(AContext: TEpollConnectionContext);
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
  public
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0'; const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallbackListen: TProc; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: TProc; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure ListenWithConfig(const APort: Integer; const AConfig: THorseCrossSocketConfig); override;
    class procedure StopListen; override;
    class function IsRunning: Boolean;

    class constructor CreateClass;
    class destructor DestroyClass;
  end;

  THorseProvider = class(THorseProviderEpoll);

{$ENDIF}

implementation

{$IFDEF LINUX}

threadvar
  FLocalPool: TQueue<TBytes>;

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

{$IF NOT DEFINED(FPC)}
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

type
  iovec = record
    iov_base: Pointer;
    iov_len: NativeUInt;
  end;
  piovec = ^iovec;

function writev(fd: Integer; iov: piovec; iovcnt: Integer): NativeInt; cdecl; external libc name 'writev';
function sendfile(out_fd: Integer; in_fd: Integer; offset: PInt64; count: NativeUInt): NativeInt; cdecl; external libc name 'sendfile';

function GetTickCount64: Int64;
var
  LTime: TEpollTimeSpec;
begin
  if clock_gettime(1, LTime) = 0 then
    Result := (LTime.tv_sec * 1000) + (LTime.tv_nsec div 1000000)
  else
    Result := 0;
end;
{$IFEND}

{ TBufferPool }

class constructor TBufferPool.Create;
begin
end;

class destructor TBufferPool.Destroy;
begin
end;

class function TBufferPool.Acquire: TBytes;
begin
  if FLocalPool = nil then
    FLocalPool := TQueue<TBytes>.Create;

  if FLocalPool.Count > 0 then
    Result := FLocalPool.Dequeue
  else
    SetLength(Result, BUFFER_SIZE);
end;

class procedure TBufferPool.Release(var ABuffer: TBytes);
begin
  if ABuffer = nil then Exit;
  if FLocalPool = nil then
    FLocalPool := TQueue<TBytes>.Create;

  if FLocalPool.Count < 32 then
    FLocalPool.Enqueue(ABuffer)
  else
    ABuffer := nil;
end;

{ TEpollConnectionContext }

constructor TEpollConnectionContext.Create(ASocket: Integer);
begin
  inherited Create;
  Socket := ASocket;
  Buffer := TBufferPool.Acquire;
  BytesReceived := 0;
  Headers := TDictionary<string, THeaderSegment>.Create;
  FBodyStream := nil;
  FTempFileName := '';
  FChunked := False;
  FChunkState := 0;
  FChunkSize := 0;
  FChunkRemaining := 0;
  SetLength(FChunkLineBytes, 256);
  FChunkLineLen := 0;
  FIsKeepAlive := False;
  LastActive := GetTickCount64;
  ClearRequest;
end;

destructor TEpollConnectionContext.Destroy;
begin
  Headers.Free;
  TBufferPool.Release(Buffer);
  if Assigned(FBodyStream) then
    FBodyStream.Free;
  if FTempFileName <> '' then
    DeleteFile(FTempFileName);
  inherited;
end;

procedure TEpollConnectionContext.ClearRequest;
begin
  Method := '';
  Path := '';
  Query := '';
  Version := '';
  Headers.Clear;
  BodyOffset := -1;
  ContentLength := 0;
  FBodyStream := nil;
  FTempFileName := '';
  FChunked := False;
  FChunkState := 0;
  FChunkSize := 0;
  FChunkRemaining := 0;
  FChunkLineLen := 0;
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
      FBodyStream := TMemoryStream.Create;
    end;
  end;

  if (FBodyStream is TMemoryStream) and (FBodyStream.Size + ALength >= 2097152) then
  begin
    LTempPath := '/tmp/';
    LTempFile := LTempPath + 'horse_spool_' + IntToStr(GetTickCount64) + '_' + IntToStr(Socket) + '.tmp';
    FTempFileName := LTempFile;
    LFileStream := TFileStream.Create(LTempFile, fmCreate or fmOpenWrite);
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

class function THorseHttpParser.TryParseRequest(
  const ABuffer: TBytes; 
  ALength: Integer;
  out AMethod: string;
  out APath: string;
  out AQuery: string;
  out AVersion: string;
  out AHeaders: TDictionary<string, THeaderSegment>;
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
  Key: string;
  Segment: THeaderSegment;
  RawLine: AnsiString;
begin
  AMethod := '';
  APath := '';
  AQuery := '';
  AVersion := '';
  ABodyOffset := -1;
  AContentLength := 0;
  AHeaders := nil;

  if ALength < 4 then
    Exit(False);

  // 1. Localiza o fim dos cabeçalhos (\r\n\r\n)
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

  AHeaders := TDictionary<string, THeaderSegment>.Create;

  // 2. Processa a linha inicial (Request Line)
  LineEnd := FindCRLF(ABuffer, 0, HeaderEnd);
  if LineEnd = -1 then Exit(False);

  Space1 := FindByte(ABuffer, 0, LineEnd, 32); // Espaço
  if Space1 = -1 then Exit(False);

  Space2 := FindByte(ABuffer, Space1 + 1, LineEnd, 32);
  if Space2 = -1 then Exit(False);

  // Extração rápida de Método, Path, Query e Versão
  SetString(RawLine, PAnsiChar(@ABuffer[0]), LineEnd);
  AMethod := string(Copy(RawLine, 1, Space1));
  
  QueryStart := FindByte(ABuffer, Space1 + 1, Space2, 63); // '?'
  if QueryStart <> -1 then
  begin
    APath := string(Copy(RawLine, Space1 + 2, QueryStart - (Space1 + 1)));
    AQuery := string(Copy(RawLine, QueryStart + 1, Space2 - QueryStart));
  end
  else
  begin
    APath := string(Copy(RawLine, Space1 + 2, Space2 - (Space1 + 1)));
    AQuery := '';
  end;

  AVersion := string(Copy(RawLine, Space2 + 2, LineEnd - (Space2 + 1)));

  // 3. Processa os cabeçalhos linha a linha indexando os offsets
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
        
        // Remove espaços do valor (Trim rápido dos bytes)
        I := Colon + 1;
        while (I < LineEnd) and (ABuffer[I] = 32) do Inc(I);
        Segment.ValueStart := I;
        Segment.ValueLen := LineEnd - I;

        // Comparações de bytes direta (Zero-Allocation) para as chaves conhecidas
        if CompareBytesCI(ABuffer, Segment.KeyStart, Segment.KeyLen, 'content-length') then
        begin
          if Segment.ValueLen > 0 then
          begin
            SetString(RawLine, PAnsiChar(@ABuffer[Segment.ValueStart]), Segment.ValueLen);
            AContentLength := StrToInt64Def(string(RawLine), 0);
          end;
          Key := 'content-length';
        end
        else if CompareBytesCI(ABuffer, Segment.KeyStart, Segment.KeyLen, 'transfer-encoding') then
          Key := 'transfer-encoding'
        else if CompareBytesCI(ABuffer, Segment.KeyStart, Segment.KeyLen, 'connection') then
          Key := 'connection'
        else if CompareBytesCI(ABuffer, Segment.KeyStart, Segment.KeyLen, 'host') then
          Key := 'host'
        else if CompareBytesCI(ABuffer, Segment.KeyStart, Segment.KeyLen, 'content-type') then
          Key := 'content-type'
        else
        begin
          SetString(RawLine, PAnsiChar(@ABuffer[Segment.KeyStart]), Segment.KeyLen);
          Key := LowerCase(Trim(string(RawLine)));
        end;

        AHeaders.AddOrSetValue(Key, Segment);
      end;
    end;

    LineStart := LineEnd + 2;
  end;

  ABodyOffset := HeaderEnd + 4;
  Result := True;
end;

{ TEpollRawRequest }

constructor TEpollRawRequest.Create(
  const ABuffer: TBytes;
  const AMethod, APath, AQuery, AVersion: string;
  AHeaders: TDictionary<string, THeaderSegment>;
  ABodyOffset: Integer;
  AContentLength: Int64;
  ABodyStream: TStream;
  const ATempFileName: string
);
begin
  inherited Create;
  FBuffer := ABuffer;
  FMethod := AMethod;
  FPath := APath;
  FQuery := AQuery;
  FVersion := AVersion;
  FHeaders := AHeaders;
  FBodyOffset := ABodyOffset;
  FContentLength := AContentLength;
  FBodyStream := ABodyStream;
  FTempFileName := ATempFileName;
  FResolvedHeaders := TDictionary<string, string>.Create;
end;

destructor TEpollRawRequest.Destroy;
begin
  FResolvedHeaders.Free;
  if Assigned(FBodyStream) then
    FBodyStream.Free;
  if FTempFileName <> '' then
    DeleteFile(FTempFileName);
  inherited;
end;

procedure TEpollRawRequest.EnsureBodyStream;
begin
  // FBodyStream já é instanciado e gerenciado externamente pelo Worker/Context
end;

function TEpollRawRequest.ResolveHeader(const AName: string): string;
var
  LSegment: THeaderSegment;
  LRawVal: AnsiString;
  LLowerName: string;
begin
  LLowerName := LowerCase(AName);
  if FResolvedHeaders.TryGetValue(LLowerName, Result) then
    Exit;

  if FHeaders.TryGetValue(LLowerName, LSegment) then
  begin
    if (LSegment.ValueLen > 0) and (LSegment.ValueStart >= 0) and (LSegment.ValueStart + LSegment.ValueLen <= Length(FBuffer)) then
    begin
      SetString(LRawVal, PAnsiChar(@FBuffer[LSegment.ValueStart]), LSegment.ValueLen);
      Result := Trim(string(LRawVal));
    end
    else
      Result := '';
    FResolvedHeaders.Add(LLowerName, Result);
  end
  else
    Result := '';
end;

function TEpollRawRequest.GetMethod: string; begin Result := FMethod; end;
function TEpollRawRequest.GetProtocolVersion: string; begin Result := FVersion; end;
function TEpollRawRequest.GetURL: string;
begin
  if FQuery <> '' then
    Result := FPath + '?' + FQuery
  else
    Result := FPath;
end;
function TEpollRawRequest.GetPathInfo: string; begin Result := FPath; end;
function TEpollRawRequest.GetQueryString: string; begin Result := FQuery; end;
function TEpollRawRequest.GetHost: string; begin Result := ResolveHeader('host'); end;
function TEpollRawRequest.GetRemoteAddr: string; begin Result := '127.0.0.1'; end;
function TEpollRawRequest.GetServerPort: Integer; begin Result := 9095; end;
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

{$IF DEFINED(FPC)}
function TEpollRawRequest.GetContentLength: Integer;
begin
  Result := FContentLength;
end;
{$ELSEIF CompilerVersion >= 32.0}
function TEpollRawRequest.GetContentLength: Int64;
begin
  Result := FContentLength;
end;
{$ELSE}
function TEpollRawRequest.GetContentLength: Integer;
begin
  Result := FContentLength;
end;
{$IFEND}

function TEpollRawRequest.GetFieldByName(const AName: string): string;
begin
  Result := ResolveHeader(AName);
end;

procedure TEpollRawRequest.PopulateQueryFields(ADest: TStrings);
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

procedure TEpollRawRequest.PopulateContentFields(ADest: TStrings);
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

procedure TEpollRawRequest.PopulateCookieFields(ADest: TStrings);
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

function TEpollRawRequest.ReadBody(var Buffer; Count: Integer): Integer;
begin
  EnsureBodyStream;
  Result := FBodyStream.Read(Buffer, Count);
end;


{ TEpollRawResponse }

constructor TEpollRawResponse.Create(ASocket: Integer; AIsKeepAlive: Boolean);
begin
  inherited Create;
  FSocket := ASocket;
  FHeadersSent := False;
  FStatusCode := 200;
  FReason := 'OK';
  FHeaders := TDictionary<string, string>.Create;
  FIsKeepAlive := AIsKeepAlive;
end;

destructor TEpollRawResponse.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure TEpollRawResponse.SetCustomHeader(const AName, AValue: string);
begin
end;

function TEpollRawResponse.PrepareHeaders: TBytes;
var
  LHeaderStr: string;
  LPair: TPair<string, string>;
begin
  LHeaderStr := Format('HTTP/1.1 %d %s'#13#10, [FStatusCode, FReason]);
  
  if not FHeaders.ContainsKey('Content-Type') then
    FHeaders.Add('Content-Type', 'text/html; charset=utf-8');

  if not FHeaders.ContainsKey('Connection') then
  begin
    if FIsKeepAlive then
      FHeaders.Add('Connection', 'keep-alive')
    else
      FHeaders.Add('Connection', 'close');
  end;

  for LPair in FHeaders do
    LHeaderStr := LHeaderStr + Format('%s: %s'#13#10, [LPair.Key, LPair.Value]);

  LHeaderStr := LHeaderStr + #13#10;
  Result := TEncoding.UTF8.GetBytes(LHeaderStr);
end;

procedure TEpollRawResponse.SendHeaders;
var
  LHeaderBytes: TBytes;
begin
  if FHeadersSent then Exit;
  LHeaderBytes := PrepareHeaders;
  {$IFDEF FPC}
  fpSend(FSocket, @LHeaderBytes[0], Length(LHeaderBytes), 0);
  {$ELSE}
  send(FSocket, LHeaderBytes[0], Length(LHeaderBytes), 0);
  {$ENDIF}
  FHeadersSent := True;
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
  {$IFDEF FPC}
  I: Integer;
  {$ENDIF}
  {$IF NOT DEFINED(FPC)}
  LFileHandle: THandle;
  LCount: Int64;
  {$ENDIF}
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

  LUseChunked := LHasChunkedHeader or (AStream.Size >= 2097152) or (AStream.Size < 0);

  if LUseChunked then
  begin
    FHeaders.AddOrSetValue('Transfer-Encoding', 'chunked');
    FHeaders.Remove('Content-Length');
  end;

  {$IF NOT DEFINED(FPC)}
  // Otimização Zero-Copy com sendfile do Linux para TFileStream
  if (not LUseChunked) and (AStream is TFileStream) then
  begin
    LFileHandle := TFileStream(AStream).Handle;
    LCount := AStream.Size - AStream.Position;
    if LCount > 0 then
    begin
      FHeaders.AddOrSetValue('Content-Length', IntToStr(LCount));
      SendHeaders;
      if sendfile(FSocket, LFileHandle, nil, LCount) >= 0 then
        Exit; // Sucesso com Zero-Copy
      // Se falhar (por exemplo, socket não aceitar ou erro), cai no fallback de cópia normal
    end;
  end;
  {$ENDIF}

  SendHeaders;

  SetLength(LChunkBuf, 8192);
  AStream.Position := 0;
  while True do
  begin
    LReadCount := AStream.Read(LChunkBuf[0], Length(LChunkBuf));
    if LReadCount <= 0 then Break;

    if LUseChunked then
    begin
      LHexStr := Format('%x'#13#10, [LReadCount]);
      LHexBytes := TEncoding.ASCII.GetBytes(LHexStr);
      {$IFDEF FPC}
      if (fpSend(FSocket, @LHexBytes[0], Length(LHexBytes), 0) < 0) or
         (fpSend(FSocket, @LChunkBuf[0], LReadCount, 0) < 0) or
         (fpSend(FSocket, PAnsiChar(#13#10), 2, 0) < 0) then Break;
      {$ELSE}
      if (send(FSocket, LHexBytes[0], Length(LHexBytes), 0) < 0) or
         (send(FSocket, LChunkBuf[0], LReadCount, 0) < 0) or
         (send(FSocket, PAnsiChar(#13#10)^, 2, 0) < 0) then Break;
      {$ENDIF}
    end
    else
    begin
      {$IFDEF FPC}
      if fpSend(FSocket, @LChunkBuf[0], LReadCount, 0) < 0 then Break;
      {$ELSE}
      if send(FSocket, LChunkBuf[0], LReadCount, 0) < 0 then Break;
      {$ENDIF}
    end;
  end;

  if LUseChunked then
  begin
    LTermStr := '0'#13#10#13#10;
    LTermBytes := TEncoding.ASCII.GetBytes(LTermStr);
    {$IFDEF FPC}
    fpSend(FSocket, @LTermBytes[0], Length(LTermBytes), 0);
    {$ELSE}
    send(FSocket, LTermBytes[0], Length(LTermBytes), 0);
    {$ENDIF}
  end;
end;

procedure TEpollRawResponse.SendResponse(const ARes: THorseResponse);
var
  LBodyBytes: TBytes;
  LPair: TPair<string, string>;
  LHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF};
  LContentType: string;
  {$IFDEF FPC}
  I: Integer;
  {$ENDIF}
  {$IF NOT DEFINED(FPC)}
  LHeaderBytes: TBytes;
  LIov: array[0..1] of iovec;
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

  FHeaders.AddOrSetValue('Content-Type', LContentType);

  LHeadersList := ARes.CustomHeaders;
  if LHeadersList <> nil then
  begin
    {$IFDEF FPC}
    for I := 0 to LHeadersList.Count - 1 do
      FHeaders.AddOrSetValue(LHeadersList.Names[I], LHeadersList.ValueFromIndex[I]);
    {$ELSE}
    for LPair in LHeadersList do
      FHeaders.AddOrSetValue(LPair.Key, LPair.Value);
    {$ENDIF}
  end;

  if ARes.ContentStream <> nil then
  begin
    SendStreamResponse(ARes.ContentStream, ARes.CustomHeaders);
    Exit;
  end;

  if Length(LBodyBytes) = 0 then
  begin
    if ARes.BodyText <> '' then
      LBodyBytes := TEncoding.UTF8.GetBytes(ARes.BodyText)
    {$IFNDEF FPC}
    else if (ARes.RawWebResponse <> nil) and (ARes.RawWebResponse.Content <> '') then
      LBodyBytes := TEncoding.UTF8.GetBytes(ARes.RawWebResponse.Content)
    {$ENDIF};
  end;

  FHeaders.AddOrSetValue('Content-Length', IntToStr(Length(LBodyBytes)));

  {$IF NOT DEFINED(FPC)}
  // Delphi Linux com writev (Single System Call para Header + Body)
  if not FHeadersSent then
  begin
    LHeaderBytes := PrepareHeaders;
    FHeadersSent := True;
    if Length(LBodyBytes) > 0 then
    begin
      LIov[0].iov_base := @LHeaderBytes[0];
      LIov[0].iov_len := Length(LHeaderBytes);
      LIov[1].iov_base := @LBodyBytes[0];
      LIov[1].iov_len := Length(LBodyBytes);
      writev(FSocket, @LIov[0], 2);
    end
    else
    begin
      send(FSocket, LHeaderBytes[0], Length(LHeaderBytes), 0);
    end;
  end
  else
  begin
    if Length(LBodyBytes) > 0 then
      send(FSocket, LBodyBytes[0], Length(LBodyBytes), 0);
  end;
  {$ELSE}
  // Lazarus FPC fallback síncrono padrão
  SendHeaders;
  if Length(LBodyBytes) > 0 then
    fpSend(FSocket, @LBodyBytes[0], Length(LBodyBytes), 0);
  {$ENDIF}
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
  FreeOnTerminate := False;
end;

destructor THorseEpollWorker.Destroy;
begin
  TerminateWorker;
  while FConnections.Count > 0 do
    CloseConnection(FConnections[0]);
  FConnections.Free;
  FConnectionsSync.Free;
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
  LWebRequest: TInterfacedWebRequest;
  LWebResponse: TInterfacedWebResponse;
  LReq: THorseRequest;
  LRes: THorseResponse;
  LEvent: epoll_event;
  LKeepAlive: Boolean;
  LConnHeader: string;
  LRequestComplete: Boolean;
  LExcessBytes: Integer;
  LIsChunked: Boolean;
  LSegment: THeaderSegment;
  LRawVal: AnsiString;
  LBodyReadBuf: TBytes;
begin
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

    if THorseHttpParser.TryParseRequest(
      AContext.Buffer,
      AContext.BytesReceived,
      AContext.Method,
      AContext.Path,
      AContext.Query,
      AContext.Version,
      AContext.Headers,
      AContext.BodyOffset,
      AContext.ContentLength
    ) then
    begin
      LIsChunked := False;
      if AContext.Headers.TryGetValue('transfer-encoding', LSegment) then
      begin
        if LSegment.ValueLen > 0 then
        begin
          SetString(LRawVal, PAnsiChar(@AContext.Buffer[LSegment.ValueStart]), LSegment.ValueLen);
          LIsChunked := SameText(Trim(string(LRawVal)), 'chunked');
        end;
      end;

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
          if LExcessBytes > 0 then
            AContext.WriteToBodyStream(AContext.Buffer, AContext.BodyOffset, LExcessBytes);
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
    SetLength(LBodyReadBuf, 8192);
    while True do
    begin
      {$IFDEF FPC}
      LBytesRead := fpRecv(AContext.Socket, @LBodyReadBuf[0], 8192, 0);
      {$ELSE}
      LBytesRead := recv(AContext.Socket, LBodyReadBuf[0], 8192, 0);
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

  if LRequestComplete then
  begin
    if AContext.FBodyStream = nil then
      AContext.FBodyStream := TMemoryStream.Create;

    AContext.FBodyStream.Position := 0;

    LRawReq := TEpollRawRequest.Create(
      AContext.Buffer,
      AContext.Method,
      AContext.Path,
      AContext.Query,
      AContext.Version,
      AContext.Headers,
      AContext.BodyOffset,
      AContext.ContentLength,
      AContext.FBodyStream,
      AContext.FTempFileName
    );
    AContext.FBodyStream := nil;
    AContext.FTempFileName := '';

    LConnHeader := LRawReq.GetFieldByName('connection');
    if SameText(LRawReq.GetProtocolVersion, 'HTTP/1.1') then
      LKeepAlive := not SameText(LConnHeader, 'close')
    else
      LKeepAlive := SameText(LConnHeader, 'keep-alive');

    LRawRes := TEpollRawResponse.Create(AContext.Socket, LKeepAlive);
    LWebRequest := TInterfacedWebRequest.Create(LRawReq);
    
    {$IFDEF FPC}
    LWebResponse := nil;
    {$ELSE}
    LWebResponse := TInterfacedWebResponse.Create(LRawRes);
    {$ENDIF}

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
        LLocalRawRes: IHorseRawResponse;
        LLocalKeepAlive: Boolean;
      begin
        LLocalEpollFd := FEpollFd;
        LLocalContext := AContext;
        LLocalRawRes := LRawRes;
        LLocalKeepAlive := LKeepAlive;
        try
          LHorseReq := THorseRequest.Create(LWebRequest);
          LHorseRes := THorseResponse.Create(nil);
          LHorseRes.SetCSRawWebResponse(LWebResponse);
          try
            THorseProviderEpoll.Execute(LHorseReq, LHorseRes);
          finally
            TEpollRawResponse(LLocalRawRes).SendResponse(LHorseRes);
            LHorseReq.Free;
            LHorseRes.Free;
          end;
        finally
          LWebRequest.Free;
          
          if LLocalKeepAlive then
          begin
            LLocalContext.ClearRequest;
            LLocalContext.FIsKeepAlive := True;
            LLocalContext.BytesReceived := 0;

            LLocalEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
            LLocalEvent.data.ptr := LLocalContext;
            epoll_ctl(LLocalEpollFd, EPOLL_CTL_MOD, LLocalContext.Socket, @LLocalEvent);
          end
          else
          begin
            CloseConnection(LLocalContext);
          end;
        end;
      end);
    {$ELSE}
    // FPC fallback síncrono padrão
    try
      LReq := THorseRequest.Create(LWebRequest);
      LRes := THorseResponse.Create(nil);
      try
        THorseProviderEpoll.Execute(LReq, LRes);
      finally
        TEpollRawResponse(LRawRes).SendResponse(LRes);
        LReq.Free;
        LRes.Free;
      end;
    finally
      LWebRequest.Free;
    end;

    if LKeepAlive then
    begin
      AContext.ClearRequest;
      AContext.FIsKeepAlive := True;
      AContext.BytesReceived := 0;

      LEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
      LEvent.data.ptr := AContext;
      epoll_ctl(FEpollFd, EPOLL_CTL_MOD, AContext.Socket, @LEvent);
    end
    else
      CloseConnection(AContext);
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
        if LContext.FIsKeepAlive and (LContext.BytesReceived = 0) then
        begin
          if LNow - LContext.LastActive > 60000 then
            LExpired.Add(LContext);
        end
        else
        begin
          if LNow - LContext.LastActive > 5000 then
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
  LEvents: array[0..63] of epoll_event;
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

  LEvent.events := EPOLLIN;
  LEvent.data.fd := FPipeFds[0];
  epoll_ctl(FEpollFd, EPOLL_CTL_ADD, FPipeFds[0], @LEvent);

  LEvent.events := EPOLLIN or EPOLLET;
  LEvent.data.fd := FListenSocket;
  epoll_ctl(FEpollFd, EPOLL_CTL_ADD, FListenSocket, @LEvent);

  FRunning := True;
  {$IFDEF FPC}
  Writeln('Worker: Thread iniciada. FListenSocket = ', FListenSocket, ' FEpollFd = ', FEpollFd);
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
        {$ELSE}
        if errno = EINTR then Continue;
        {$ENDIF}
        Break;
      end;

      for I := 0 to LEventCount - 1 do
      begin
        LEvent := LEvents[I];
        
        {$IFDEF FPC}
        // No FPC, a uniao epoll_data compartilha o mesmo espaço. Para o pipe e listen, comparamos com fd.
        if LEvent.data.fd = FPipeFds[0] then
        begin
          Writeln('Worker: Sinal de parada recebido via pipe.');
          Exit;
        end
        {$ELSE}
        if LEvent.data.fd = FPipeFds[0] then
          Exit
        {$ENDIF}
        else if LEvent.data.fd = FListenSocket then
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
            Writeln('Worker: Conexao aceita no socket = ', LClientFd);
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
            ProcessClientRead(LContext);
          except
            on E: Exception do
            begin
              {$IFDEF FPC}Writeln('Worker: Excecao ao processar leitura: ', E.ClassName, ' - ', E.Message);{$ENDIF}
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
    if FLocalPool <> nil then
    begin
      while FLocalPool.Count > 0 do
        FLocalPool.Dequeue;
      FLocalPool.Free;
      FLocalPool := nil;
    end;
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

  // Eleva o limite máximo de descritores de arquivos abertos (ulimit -n) do processo para 65535
  LRLimit.rlim_cur := 65535;
  LRLimit.rlim_max := 65535;
  {$IFDEF FPC}
  fpSetrlimit(RLIMIT_NOFILE, @LRLimit);
  {$ELSE}
  setrlimit(RLIMIT_NOFILE, LRLimit);
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

class procedure THorseProviderEpoll.InternalListen;
var
  {$IFDEF FPC}
  LAddr: TInetSockAddr;
  {$ELSE}
  LAddr: sockaddr_in;
  {$ENDIF}
  I: Integer;
  LThreadCount: Integer;
  LWorker: THorseEpollWorker;
  LOptVal: Integer;
  LSocket: Integer;
begin
  if FRunning then Exit;

  LThreadCount := TThread.ProcessorCount;
  if LThreadCount <= 0 then
    LThreadCount := 2;

  try
    for I := 1 to LThreadCount do
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
      fpSetsockopt(LSocket, IPPROTO_TCP, TCP_DEFER_ACCEPT, @LOptVal, SizeOf(LOptVal));
      {$ELSE}
      setsockopt(LSocket, SOL_SOCKET, SO_REUSEADDR, LOptVal, SizeOf(LOptVal));
      setsockopt(LSocket, SOL_SOCKET, SO_REUSEPORT, LOptVal, SizeOf(LOptVal));
      setsockopt(LSocket, IPPROTO_TCP, TCP_DEFER_ACCEPT, LOptVal, SizeOf(LOptVal));
      {$ENDIF}

      {$IFDEF FPC}
      fpFcntl(LSocket, F_SETFL, O_NONBLOCK);
      {$ELSE}
      fcntl(LSocket, F_SETFL, O_NONBLOCK);
      {$ENDIF}

      FillChar(LAddr, SizeOf(LAddr), 0);
      LAddr.sin_family := AF_INET;
      {$IFDEF FPC}
      LAddr.sin_port := htons(FPort);
      if (FHost = '') or (FHost = '0.0.0.0') then
        LAddr.sin_addr := StrToNetAddr('0.0.0.0')
      else
        LAddr.sin_addr := StrToNetAddr(FHost);
      {$ELSE}
      LAddr.sin_port := htons(FPort);
      if (FHost = '') or (FHost = '0.0.0.0') then
        LAddr.sin_addr.s_addr := INADDR_ANY
      else
        LAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(FHost)));
      {$ENDIF}

      {$IFDEF FPC}
      if fpBind(LSocket, psockaddr(@LAddr), SizeOf(LAddr)) < 0 then
      {$ELSE}
      if Posix.SysSocket.bind(LSocket, Psockaddr(@LAddr)^, SizeOf(LAddr)) < 0 then
      {$ENDIF}
        raise EOSError.Create('bind failed');

      {$IFDEF FPC}
      if fpListen(LSocket, 128) < 0 then
      {$ELSE}
      if Posix.SysSocket.listen(LSocket, 128) < 0 then
      {$ENDIF}
        raise EOSError.Create('listen failed');

      FListenSockets.Add(LSocket);

      LWorker := THorseEpollWorker.Create(LSocket);
      FWorkers.Add(LWorker);
      LWorker.Start;
    end;
    
    FRunning := True;
    DoOnListen;
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
  if not FRunning then Exit;

  FRunning := False;

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

{$ENDIF}

end.
