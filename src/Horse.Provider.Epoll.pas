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
    FBodyStream: TMemoryStream;
    FResolvedHeaders: TDictionary<string, string>;
    procedure EnsureBodyStream;
    function ResolveHeader(const AName: string): string;
  public
    constructor Create(
      const ABuffer: TBytes;
      const AMethod, APath, AQuery, AVersion: string;
      AHeaders: TDictionary<string, THeaderSegment>;
      ABodyOffset: Integer;
      AContentLength: Int64
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
    procedure SendHeaders;
  public
    constructor Create(ASocket: Integer);
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
    constructor Create(ASocket: Integer);
    destructor Destroy; override;
    procedure ClearRequest;
  end;

  { Pool de buffers estático e thread-safe }
  TBufferPool = class
  private
    class var FPool: TQueue<TBytes>;
    class var FSync: TCriticalSection;
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
    procedure ProcessClientRead(AContext: TEpollConnectionContext);
    procedure CloseConnection(AContext: TEpollConnectionContext);
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
  function epoll_create1(flags: Integer): Integer; cdecl; external libc name 'epoll_create1';
  function epoll_ctl(epfd: Integer; op: Integer; fd: Integer; event: pepoll_event): Integer; cdecl; external libc name 'epoll_ctl';
  function epoll_wait(epfd: Integer; events: pepoll_event; maxevents: Integer; timeout: Integer): Integer; cdecl; external libc name 'epoll_wait';
  function pipe(filedes: PInteger): Integer; cdecl; external libc name 'pipe';
{$IFEND}

{ TBufferPool }

class constructor TBufferPool.Create;
begin
  FPool := TQueue<TBytes>.Create;
  FSync := TCriticalSection.Create;
end;

class destructor TBufferPool.Destroy;
begin
  FSync.Enter;
  try
    while FPool.Count > 0 do
      FPool.Dequeue;
    FPool.Free;
  finally
    FSync.Leave;
    FSync.Free;
  end;
end;

class function TBufferPool.Acquire: TBytes;
begin
  FSync.Enter;
  try
    if FPool.Count > 0 then
      Result := FPool.Dequeue
    else
      SetLength(Result, BUFFER_SIZE);
  finally
    FSync.Leave;
  end;
end;

class procedure TBufferPool.Release(var ABuffer: TBytes);
begin
  if ABuffer = nil then Exit;
  FSync.Enter;
  try
    if FPool.Count < 512 then // Limite de cache no pool
      FPool.Enqueue(ABuffer)
    else
      ABuffer := nil;
  finally
    FSync.Leave;
  end;
end;

{ TEpollConnectionContext }

constructor TEpollConnectionContext.Create(ASocket: Integer);
begin
  inherited Create;
  Socket := ASocket;
  Buffer := TBufferPool.Acquire;
  BytesReceived := 0;
  Headers := TDictionary<string, THeaderSegment>.Create;
  ClearRequest;
end;

destructor TEpollConnectionContext.Destroy;
begin
  Headers.Free;
  TBufferPool.Release(Buffer);
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
  UrlEnd: Integer;
  QueryStart: Integer;
  Colon: Integer;
  Key: string;
  ValStr: string;
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
  
  UrlEnd := Space2;
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
        SetString(RawLine, PAnsiChar(@ABuffer[LineStart]), Colon - LineStart);
        Key := LowerCase(Trim(string(RawLine)));
        
        Segment.KeyStart := LineStart;
        Segment.KeyLen := Colon - LineStart;
        Segment.ValueStart := Colon + 1;
        Segment.ValueLen := LineEnd - (Colon + 1);
        
        AHeaders.AddOrSetValue(Key, Segment);

        if Key = 'content-length' then
        begin
          SetString(RawLine, PAnsiChar(@ABuffer[Segment.ValueStart]), Segment.ValueLen);
          ValStr := Trim(string(RawLine));
          AContentLength := StrToInt64Def(ValStr, 0);
        end;
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
  AContentLength: Int64
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
  FBodyStream := nil;
  FResolvedHeaders := TDictionary<string, string>.Create;
end;

destructor TEpollRawRequest.Destroy;
begin
  FHeaders.Free;
  FResolvedHeaders.Free;
  if Assigned(FBodyStream) then
    FBodyStream.Free;
  inherited;
end;

procedure TEpollRawRequest.EnsureBodyStream;
begin
  if FBodyStream <> nil then Exit;
  FBodyStream := TMemoryStream.Create;
  if (FContentLength > 0) and (Length(FBuffer) >= FBodyOffset + FContentLength) then
    FBodyStream.WriteBuffer(FBuffer[FBodyOffset], FContentLength);
  FBodyStream.Position := 0;
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
    if LSegment.ValueLen > 0 then
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

constructor TEpollRawResponse.Create(ASocket: Integer);
begin
  inherited Create;
  FSocket := ASocket;
  FHeadersSent := False;
  FStatusCode := 200;
  FReason := 'OK';
  FHeaders := TDictionary<string, string>.Create;
end;

destructor TEpollRawResponse.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure TEpollRawResponse.SetCustomHeader(const AName, AValue: string);
begin
end;

procedure TEpollRawResponse.SendHeaders;
var
  LHeaderStr: string;
  LHeaderBytes: TBytes;
  LPair: TPair<string, string>;
begin
  if FHeadersSent then Exit;

  LHeaderStr := Format('HTTP/1.1 %d %s'#13#10, [FStatusCode, FReason]);
  
  if not FHeaders.ContainsKey('Content-Type') then
    FHeaders.Add('Content-Type', 'text/html; charset=utf-8');

  for LPair in FHeaders do
    LHeaderStr := LHeaderStr + Format('%s: %s'#13#10, [LPair.Key, LPair.Value]);

  LHeaderStr := LHeaderStr + #13#10;
  LHeaderBytes := TEncoding.UTF8.GetBytes(LHeaderStr);

  {$IFDEF FPC}
  fpSend(FSocket, @LHeaderBytes[0], Length(LHeaderBytes), 0);
  {$ELSE}
  send(FSocket, LHeaderBytes[0], Length(LHeaderBytes), 0);
  {$ENDIF}
  FHeadersSent := True;
end;

procedure TEpollRawResponse.SendResponse(const ARes: THorseResponse);
var
  LBodyBytes: TBytes;
  LPair: TPair<string, string>;
  LHeadersList: {$IFDEF FPC}TStrings{$ELSE}TDictionary<string, string>{$ENDIF};
  LContentType: string;
  I: Integer;
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

  if (ARes.RawWebResponse <> nil) and (ARes.RawWebResponse.ContentType <> '') then
    LContentType := ARes.RawWebResponse.ContentType
  else
    LContentType := ARes.CSContentType;

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
    SetLength(LBodyBytes, ARes.ContentStream.Size);
    ARes.ContentStream.Position := 0;
    if ARes.ContentStream.Size > 0 then
      ARes.ContentStream.ReadBuffer(LBodyBytes[0], ARes.ContentStream.Size);
  end;

  if Length(LBodyBytes) = 0 then
  begin
    if ARes.BodyText <> '' then
      LBodyBytes := TEncoding.UTF8.GetBytes(ARes.BodyText)
    else if (ARes.RawWebResponse <> nil) and (ARes.RawWebResponse.Content <> '') then
      LBodyBytes := TEncoding.UTF8.GetBytes(ARes.RawWebResponse.Content);
  end;

  FHeaders.AddOrSetValue('Content-Length', IntToStr(Length(LBodyBytes)));

  SendHeaders;
  if Length(LBodyBytes) > 0 then
  begin
    {$IFDEF FPC}
    fpSend(FSocket, @LBodyBytes[0], Length(LBodyBytes), 0);
    {$ELSE}
    send(FSocket, LBodyBytes[0], Length(LBodyBytes), 0);
    {$ENDIF}
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
  FreeOnTerminate := False;
end;

destructor THorseEpollWorker.Destroy;
begin
  TerminateWorker;
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
  LRawRes: TEpollRawResponse;
  LWebRequest: TInterfacedWebRequest;
  LWebResponse: TInterfacedWebResponse;
  LReq: THorseRequest;
  LRes: THorseResponse;
  LEvent: epoll_event;
  LKeepAlive: Boolean;
  LConnHeader: string;
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

    if AContext.BytesReceived >= Length(AContext.Buffer) then
      SetLength(AContext.Buffer, Length(AContext.Buffer) * 2);
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
    if AContext.BytesReceived >= AContext.BodyOffset + AContext.ContentLength then
    begin
      LRawReq := TEpollRawRequest.Create(
        AContext.Buffer,
        AContext.Method,
        AContext.Path,
        AContext.Query,
        AContext.Version,
        AContext.Headers,
        AContext.BodyOffset,
        AContext.ContentLength
      );
      LRawRes := TEpollRawResponse.Create(AContext.Socket);
      LWebRequest := TInterfacedWebRequest.Create(LRawReq);
      
      {$IFDEF FPC}
      LWebResponse := nil;
      {$ELSE}
      LWebResponse := TInterfacedWebResponse.Create(LRawRes);
      {$ENDIF}
      try
        LReq := THorseRequest.Create(LWebRequest);
        {$IFDEF FPC}
        LRes := THorseResponse.Create(nil);
        {$ELSE}
        LRes := THorseResponse.Create(LWebResponse);
        {$ENDIF}
        try
          THorseProviderEpoll.Execute(LReq, LRes);
        finally
          LRawRes.SendResponse(LRes);
          LReq.Free;
          LRes.Free;
        end;
      finally
        LWebRequest.Free;
        if LWebResponse <> nil then
          LWebResponse.Free;
        LRawRes.Free;
      end;

      LConnHeader := LRawReq.GetFieldByName('connection');
      LKeepAlive := not SameText(LConnHeader, 'close');

      if LKeepAlive then
      begin
        AContext.ClearRequest;
        AContext.BytesReceived := 0;

        LEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
        LEvent.data.ptr := AContext;
        epoll_ctl(FEpollFd, EPOLL_CTL_MOD, AContext.Socket, @LEvent);
      end
      else
        CloseConnection(AContext);
    end
    else
    begin
      LEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
      LEvent.data.ptr := AContext;
      epoll_ctl(FEpollFd, EPOLL_CTL_MOD, AContext.Socket, @LEvent);
    end;
  end
  else
  begin
    LEvent.events := EPOLLIN or EPOLLET or EPOLLONESHOT;
    LEvent.data.ptr := AContext;
    epoll_ctl(FEpollFd, EPOLL_CTL_MOD, AContext.Socket, @LEvent);
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
    end;
  finally
    {$IFDEF FPC}Writeln('Worker: Thread encerrando.');{$ENDIF}
  end;
end;

{ THorseProviderEpoll }

class constructor THorseProviderEpoll.CreateClass;
begin
  FPort := GetDefaultPort;
  FHost := GetDefaultHost;
  FListenSockets := TList<Integer>.Create;
  FWorkers := TObjectList<THorseEpollWorker>.Create(True);
  FRunning := False;
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
