unit Horse.Response;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Classes,
  Generics.Collections,
  fpHTTP,
  HTTPDefs,
{$ELSE}
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
{$IF CompilerVersion > 32.0}
  Web.ReqMulti,
{$ENDIF}
{$ENDIF}
{ ===========================================================================
  PATCH-RES-1 — added System.Generics.Collections (Delphi only)
  Reason: FCustomHeaders is TList<TPair<string,string>> on Delphi.
  FPC path uses TStringList (Classes) which is already imported above.
  =========================================================================== }
{$IF NOT DEFINED(FPC)}
  System.Generics.Collections,
{$ENDIF}
{ =========================================================================== }
  Horse.Commons,
  Horse.Core.Cookie,
  Horse.Core.WebSocket;

type
  THorseResponse = class;

  IHorseStreamWriter = interface
    ['{33BB5995-1E80-4D53-9F96-2ACDE71CD04F}']
    procedure Write(const AText: string); overload;
    procedure Write(const ABytes: TBytes); overload;
    procedure Flush;
    procedure Close;
    function IsConnected: Boolean;
  end;

  THorseStreamProc = procedure(const AWriter: IHorseStreamWriter) of object;
  THorseStreamAnonProc = {$IF NOT DEFINED(FPC)}reference to {$ENDIF} procedure(const AWriter: IHorseStreamWriter);
  THorseStreamWriterFactory = {$IF NOT DEFINED(FPC)}reference to {$ENDIF}function(const AResponse: THorseResponse): IHorseStreamWriter;

  THorseStreamWriterBase = class(TInterfacedObject, IHorseStreamWriter)
  private
    FHeadersSent: Boolean;
    FUseChunked: Boolean;
    procedure SendHeaders;
  protected
    FResponse: THorseResponse;
    procedure SendRawHeaders; virtual; abstract;
    procedure WriteRawBytes(const ABytes: TBytes); virtual; abstract;
  public
    constructor Create(const AResponse: THorseResponse); virtual;
    procedure Write(const AText: string); overload;
    procedure Write(const ABytes: TBytes); overload;
    procedure Flush; virtual;
    procedure Close; virtual;
    function IsConnected: Boolean; virtual; abstract;
  end;

  THorseWebBrokerStreamWriter = class(THorseStreamWriterBase)
  protected
    procedure SendRawHeaders; override;
    procedure WriteRawBytes(const ABytes: TBytes); override;
  public
    function IsConnected: Boolean; override;
  end;

  THorseResponse = class
  private
    FWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
    FRequest: TObject;
    FAborted: Boolean;
    FContent: TObject;
    FIsStreaming: Boolean;
    FStreamMethod: THorseStreamProc;
    FStreamCallback: THorseStreamAnonProc;
    class var FStreamWriterFactory: THorseStreamWriterFactory;
  private
{ ===========================================================================
  PATCH-RES-1 — added FCustomHeaders field
  Reason: CrossSocket has no TWebResponse. TResponseBridge.CopyHeaders
  iterates this list directly to write headers to ICrossHttpResponse.
  AddHeader writes to both FWebResponse.SetCustomHeader (Indy path) and
  this map (CrossSocket path) so all existing middleware that calls
  Res.AddHeader continues to work on both providers without any change.

  Delphi: TDictionary<string,string> — O(1) lookup; last value wins for
  duplicate keys (AddOrSetValue overwrites).
  FPC: TStringList — same key=value string storage used on the Lazarus path.
  =========================================================================== }
    FCustomHeaders: {$IF NOT DEFINED(FPC)}TDictionary<string, string>{$ELSE}TStringList{$ENDIF};
{ REPEATHDR-1 (2026-07-17) — ordered, duplicate-preserving side-store for
  headers that MUST NOT be folded (currently Set-Cookie only; RFC 6265 §3). The
  primary FCustomHeaders store dedups by name (TDictionary on Delphi / Values[]
  on FPC), which silently collapses a second Set-Cookie onto the first. The
  adapter providers (IOCP now; Epoll/HttpSys when re-derived) emit this list
  directly, bypassing dedup, so every Set-Cookie reaches the wire. TStringList
  on both compilers; lazily allocated; nil until the first repeatable header. }
    FRepeatHeaders: TStringList;
{ =========================================================================== }
{ ===========================================================================
  PATCH-COOKIE-1 — typed Set-Cookie list (RFC 6265).
  A key-value header map cannot hold multiple Set-Cookie headers, so cookies
  added via AddCookie/Cookie are kept here (owned) and each response bridge
  emits one Set-Cookie line per entry.  Lazy-created; nil when no cookie set.
  =========================================================================== }
    FCookies: TObjectList<THorseCookie>;
{ =========================================================================== }
{ ===========================================================================
  PATCH-RES-4 — CrossSocket shadow fields
  Reason: On the CrossSocket path FWebResponse is nil (no TWebResponse exists).
  Every public method that previously wrote to FWebResponse now checks for nil
  and falls through to these fields instead. The bridge reads them via the
  read-only properties BodyText, ContentStream, and CSContentType.

  FCSStatusCode  — integer HTTP status (default 200)
  FCSBody        — string body set by Send(string) or Send<T>
  FCSContentType — Content-Type set by ContentType(string) or SendFile
  FCSContentStream — stream body set by SendFile/Download/Render
  =========================================================================== }
    FCSStatusCode:    Integer;
    FCSBody:          string;
    FCSBodyBytes:     TBytes;
    FCSContentType:   string;
    FCSContentStream: TStream;   // see FCSOwnsContentStream
{ PATCH-SENDFILE-1 — SendFile/Download on the shadow (CrossSocket/mORMot) path
  COPY the source into a response-owned stream so the caller may free their own
  stream immediately; the provider flushes AFTER the handler returns.  When
  FCSOwnsContentStream is True, Clear/Destroy free FCSContentStream. }
    FCSOwnsContentStream: Boolean;
{ =========================================================================== }
{ ===========================================================================
  PATCH-RES-6 — owned RawWebResponse adapter for CrossSocket path
  Mirrors PATCH-REQ-8: when FWebResponse is nil (CrossSocket), middleware that
  calls Res.RawWebResponse.SetCustomHeader(...) — e.g. Horse.CORS — would
  crash with an AV. This field holds a TCrossSocketWebResponse adapter so
  RawWebResponse returns a non-nil value.

  Owned by THorseResponse: Clear frees it; Destroy frees it.
  Nil on the Indy path (FWebResponse is the authoritative source there).

  See: Horse.Provider.CrossSocket.WebResponseAdapter.pas
  =========================================================================== }
    FCSRawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
{ =========================================================================== }
{ ===========================================================================
  PATCH-RES-7 — lazy-allocation helper for FCustomHeaders.
  Was eagerly created in the constructor; now created only when AddHeader
  is first called. Eager allocation paid an unconditional cost on every
  Indy/ISAPI/CGI request that never read or wrote a custom header.
  =========================================================================== }
    procedure EnsureCustomHeaders;
{ REPEATHDR-1 — lazy-allocation helper for FRepeatHeaders (mirrors EnsureCustomHeaders). }
    procedure EnsureRepeatHeaders;
{ =========================================================================== }
  public
    class procedure RegisterStreamWriterFactory(const AFactory: THorseStreamWriterFactory);
    function SendStream(const ACallback: THorseStreamProc): THorseResponse; overload;
    function SendStream(const ACallback: THorseStreamAnonProc): THorseResponse; overload;
    function Send(const AContent: string): THorseResponse; overload; virtual;
    function Send(const AContent: TBytes): THorseResponse; overload; virtual;
    function Send<T{$IF NOT DEFINED(FPC)}: class{$ENDIF}>(AContent: T): THorseResponse; overload;
    function RedirectTo(const ALocation: string): THorseResponse; overload; virtual;
    function RedirectTo(const ALocation: string; const AStatus: THTTPStatus): THorseResponse; overload; virtual;
    function Status(const AStatus: Integer): THorseResponse; overload; virtual;
    function Status(const AStatus: THTTPStatus): THorseResponse; overload; virtual;
    function SendFile(const AFileStream: TStream; const AFileName: string = ''; const AContentType: string = ''): THorseResponse; overload; virtual;
    function SendFile(const AFileName: string; const AContentType: string = ''): THorseResponse; overload; virtual;
    function Download(const AFileStream: TStream; const AFileName: string; const AContentType: string = ''): THorseResponse; overload; virtual;
    function Download(const AFileName: string; const AContentType: string = ''): THorseResponse; overload; virtual;
    function Render(const AFileStream: TStream; const AFileName: string): THorseResponse; overload; virtual;
    function Render(const AFileName: string): THorseResponse; overload; virtual;
    function Status: Integer; overload; virtual;
    function AddHeader(const AName, AValue: string): THorseResponse; virtual;
    function RemoveHeader(const AName: string): THorseResponse; virtual;
{ PATCH-COOKIE-1 — typed Set-Cookie API (RFC 6265). AddCookie takes ownership of
  ACookie; Cookie(name,value) creates one, adds it, and returns it for fluent
  attribute setting. Each provider bridge emits one Set-Cookie line per entry. }
    function AddCookie(const ACookie: THorseCookie): THorseResponse;
    function Cookie(const AName, AValue: string): THorseCookie;
{ PATCH-COOKIE-1 (Indy) — called by THorseRouterTree.Execute after the pipeline.
  Maps the typed cookie list onto the WebBroker TWebResponse.Cookies so Indy
  emits one Set-Cookie line per cookie. No-op when FWebResponse is nil
  (CrossSocket/mORMot read FCookies directly in their bridges). }
    procedure FlushCookiesToWebResponse;
{ end PATCH-COOKIE-1 }
    function Content: TObject; overload; virtual;
    function Content(const AContent: TObject): THorseResponse; overload; virtual;
    function ContentType(const AContentType: string): THorseResponse; virtual;
    function RawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF}; virtual;
    function UpgradeToWebSocket(const AOnConnect: TOnWebSocketConnect): IHorseWebSocketConnection;
    constructor Create(const AWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF});
{ ===========================================================================
  PATCH-RES-2 — added Clear procedure
  Reason: THorseContext.Reset recycles pooled objects between requests.
  Resets FContent and clears FCustomHeaders in place (dictionary object
  reused — avoids heap churn on the request hot path).
  FWebResponse is set to nil — belongs to the previous Indy context.
  =========================================================================== }
    procedure Clear;
{ =========================================================================== }
{ ===========================================================================
  PATCH-RES-6 — setter for the owned RawWebResponse adapter. Called once per
  request by the CrossSocket provider after pool acquire. Replaces any prior
  adapter instance (defence in depth — Clear normally nils it first).
  =========================================================================== }
    procedure SetCSRawWebResponse(const ARawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF});
{ =========================================================================== }
{ ===========================================================================
  PATCH-RES-3 — added CustomHeaders read-only property
  Reason: TResponseBridge.CopyHeaders reads this property to iterate and
  forward response headers to ICrossHttpResponse. Read-only — the bridge
  iterates only; all writes go through AddHeader as before.
  =========================================================================== }
    property CustomHeaders: {$IF NOT DEFINED(FPC)}TDictionary<string, string>{$ELSE}TStringList{$ENDIF} read FCustomHeaders;
{ REPEATHDR-1 — read-only ordered store of duplicate-preserving headers
  (Set-Cookie). nil until the first such header. Adapter providers emit these
  directly, bypassing the FCustomHeaders dedup. Name=Value separated. }
    property RepeatHeaders: TStringList read FRepeatHeaders;
{ =========================================================================== }
{ ===========================================================================
  PATCH-COOKIE-1 — read-only cookie list for the response bridges. nil until
  the first AddCookie/Cookie call.
  =========================================================================== }
    property Cookies: TObjectList<THorseCookie> read FCookies;
{ =========================================================================== }
{ ===========================================================================
  PATCH-RES-4 — read-only properties for the CrossSocket bridge
  TResponseBridge.Flush reads these to write the response body and
  Content-Type to ICrossHttpResponse.  All three are populated only when
  FWebResponse is nil (CrossSocket path); on the Indy path they are empty.
  =========================================================================== }
    property BodyText:       string  read FCSBody;
    property ContentStream:  TStream read FCSContentStream;
    property CSContentType:  string  read FCSContentType;
    property BodyBytes:      TBytes  read FCSBodyBytes;
{ =========================================================================== }
    function Abort: THorseResponse; virtual;
    property Aborted: Boolean read FAborted;
    property Request: TObject read FRequest write FRequest;
    property IsStreaming: Boolean read FIsStreaming;
    property StreamMethod: THorseStreamProc read FStreamMethod;
    property StreamCallback: THorseStreamAnonProc read FStreamCallback;
    destructor Destroy; override;
  end;

implementation

uses
  Horse.Core.Files,
  Horse.Mime,
  Horse.Request,
  Horse.Core,
  Horse.Exception.Interrupted,
  Horse.Core.MemoryBufferPool
  {$IF DEFINED(FPC)}
  , fphttpserver
  , ssockets
  , sockets
    {$IFDEF UNIX}
  , BaseUnix
    {$ENDIF}
    {$IFDEF MSWINDOWS}
  , WinSock2
    {$ENDIF}
  {$ELSE}
  , IdHTTPWebBrokerBridge, IdCustomHTTPServer
  {$ENDIF};

{$IF DEFINED(FPC)}
type
  TFPHTTPConnectionResponseAccess = class(TFPHTTPConnectionResponse)
  public
    procedure WriteToSocket(const ABytes: TBytes);
    function ClientConnected: Boolean;
  end;

procedure TFPHTTPConnectionResponseAccess.WriteToSocket(
  const ABytes: TBytes
);
begin
  if Length(ABytes) = 0 then
    Exit;

  if (Connection = nil) or (Connection.Socket = nil) then
    raise EWriteError.Create('Conexão HTTP encerrada durante o streaming.');

  Connection.Socket.WriteBuffer(
    ABytes[0],
    Length(ABytes)
  );
end;

function TFPHTTPConnectionResponseAccess.ClientConnected: Boolean;
var
  LSocket: TSocketStream;
  LSocketError: LongInt;
  LSocketErrorLen: TSockLen;
  LReadSet: TFDSet;
  LTimeout: TTimeVal;
  LSelectResult: LongInt;
  LRecvResult: LongInt;
  LByte: Byte;
begin
  Result := False;

  if (Connection = nil) or (Connection.Socket = nil) then
    Exit;

  LSocket := Connection.Socket;
  if LSocket.Handle < 0 then
    Exit;

  { SO_ERROR catches pending/reset socket errors without reading application
    data. A graceful peer close normally leaves SO_ERROR = 0, so a zero-timeout
    select + MSG_PEEK is also required below. }
  LSocketError := 0;
  LSocketErrorLen := SizeOf(LSocketError);
  if fpGetSockOpt(
       LSocket.Handle,
       SOL_SOCKET,
       SO_ERROR,
       @LSocketError,
       @LSocketErrorLen
     ) <> 0 then
    Exit;

  if LSocketError <> 0 then
    Exit;

  LTimeout.tv_sec := 0;
  LTimeout.tv_usec := 0;

  {$IFDEF UNIX}
  fpFD_Zero(LReadSet);
  fpFD_Set(LSocket.Handle, LReadSet);
  LSelectResult := fpSelect(
    LSocket.Handle + 1,
    @LReadSet,
    nil,
    nil,
    @LTimeout
  );
  {$ELSE}
  FD_Zero(LReadSet);
  FD_Set(LSocket.Handle, LReadSet);
  LSelectResult := WinSock2.select(
    LSocket.Handle + 1,
    @LReadSet,
    nil,
    nil,
    @LTimeout
  );
  {$ENDIF}

  if LSelectResult < 0 then
    Exit;

  { No pending read event means no FIN/reset was observed and the connection is
    still considered alive. }
  if LSelectResult = 0 then
    Exit(True);

  { A readable TCP socket can mean either data waiting or an orderly close.
    MSG_PEEK distinguishes them without consuming request bytes. }
  repeat
    LRecvResult := fpRecv(LSocket.Handle, @LByte, 1, MSG_PEEK);
    if LRecvResult >= 0 then
      Break;
    LSocketError := SocketError;
  {$IFDEF UNIX}
  until LSocketError <> ESysEINTR;
  {$ELSE}
  until True;
  {$ENDIF}

  if LRecvResult > 0 then
    Exit(True);

  if LRecvResult = 0 then
    Exit(False);

  { The state may change between select and recv. Would-block still means the
    socket is valid; every other error is treated as disconnected. }
  {$IFDEF UNIX}
  Result := (LSocketError = ESysEWOULDBLOCK) or
            (LSocketError = ESysEAGAIN);
  {$ELSE}
  Result := LSocketError = WSAEWOULDBLOCK;
  {$ENDIF}
end;
{$ELSE}
type
  TWebRequestFriend = class(TWebRequest);
  TIdHTTPAppResponseFriend = class(TIdHTTPAppResponse);
{$ENDIF}

function THorseResponse.AddHeader(const AName, AValue: string): THorseResponse;
begin
{ PATCH-RES-4 — nil-guard: skip FWebResponse on CrossSocket path.
  FIX-HEADER-DUP — CustomHeaders.Add APPENDS; SetCustomHeader REPLACES, so a
  second Set-Cookie (etc.) would overwrite the first (observed: fphttpserver
  test 10 kept only the last cookie). Append preserves duplicate headers on
  both fpWeb (TResponse) and Indy WebBroker (TWebResponse) — their wire paths
  emit each CustomHeaders entry. }
  if Assigned(FWebResponse) then
    FWebResponse.CustomHeaders.Add(AName + '=' + AValue);
{ end PATCH-RES-4 }
{ ===========================================================================
  PATCH-RES-1 — also populate FCustomHeaders so CrossSocket bridge can read it.
  PATCH-RES-7 — allocate the headers store on first use (was eager in Create).
  Delphi: TDictionary.AddOrSetValue  FPC: TStringList.Values[name] := value
  =========================================================================== }
  EnsureCustomHeaders;
{$IF NOT DEFINED(FPC)}
  FCustomHeaders.AddOrSetValue(AName, AValue);
{$ELSE}
  FCustomHeaders.Values[AName] := AValue;
{$ENDIF}
{ =========================================================================== }
{ REPEATHDR-1 — Set-Cookie is the one response header RFC 6265 §3 forbids
  folding. The dedup store above collapses a repeated Set-Cookie; also append it
  verbatim to the ordered side-store so adapter providers can emit every one.
  The dedup entry above is intentionally KEPT so readers that only consult
  FCustomHeaders (merged Epoll/HttpSys, CrossSocket bridge) still emit one
  Set-Cookie unchanged — no regression — until they adopt RepeatHeaders too. }
  if SameText(AName, 'Set-Cookie') then
  begin
    EnsureRepeatHeaders;
    FRepeatHeaders.Add(AName + '=' + AValue);
  end;
  Result := Self;
end;

{ ===========================================================================
  PATCH-RES-7 — EnsureCustomHeaders implementation
  =========================================================================== }
procedure THorseResponse.EnsureCustomHeaders;
begin
  if FCustomHeaders <> nil then Exit;
{$IF NOT DEFINED(FPC)}
  FCustomHeaders := TDictionary<string, string>.Create;
{$ELSE}
  FCustomHeaders := TStringList.Create;
{$ENDIF}
end;
{ =========================================================================== }

{ ===========================================================================
  REPEATHDR-1 — EnsureRepeatHeaders implementation
  =========================================================================== }
procedure THorseResponse.EnsureRepeatHeaders;
begin
  if FRepeatHeaders <> nil then Exit;
  FRepeatHeaders := TStringList.Create;
  FRepeatHeaders.NameValueSeparator := '=';
end;
{ =========================================================================== }

function THorseResponse.Content(const AContent: TObject): THorseResponse;
begin
  Result := Self;
  FContent := AContent;
end;

function THorseResponse.Content: TObject;
begin
  Result := FContent;
end;

function THorseResponse.ContentType(const AContentType: string): THorseResponse;
begin
{ PATCH-RES-4 — nil-guard }
  if not Assigned(FWebResponse) then
  begin
    FCSContentType := AContentType;
    Exit(Self);
  end;
{ end PATCH-RES-4 }
  FWebResponse.ContentType := AContentType;
  Result := Self;
end;

constructor THorseResponse.Create(const AWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF});
begin
  FWebResponse := AWebResponse;
{ PATCH-RES-4 — initialise FCSStatusCode to 200 (HTTP OK) }
  FCSStatusCode := 200;
{ end PATCH-RES-4 }
  if Assigned(FWebResponse) then
  begin
{$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := THTTPStatus.Ok.ToInteger;
{$IF DEFINED(FPC)}
    FWebResponse.FreeContentStream := True;
{$ENDIF}
  end;
{ ===========================================================================
  PATCH-RES-7 — FCustomHeaders is no longer eagerly allocated here.
  AddHeader calls EnsureCustomHeaders on first use. Indy/ISAPI/CGI requests
  that never call AddHeader pay nothing.
  =========================================================================== }
end;

{ ===========================================================================
  PATCH-RES-2 — Clear implementation
  =========================================================================== }
procedure THorseResponse.Clear;
begin
  FAborted := False;
  FWebResponse := nil;
{ STREAM-RESET-1 — a pool-recycled response must NOT inherit the previous
  request's streaming state. SendStream sets FIsStreaming := True; if it survives
  into the next request on a reused pooled context, the CrossSocket provider skips
  that request's Flush (it thinks the stream already sent the response) and the
  request never responds — the /ping after a stream times out. Reset all three
  per-request streaming fields here (FStreamWriterFactory is a class var — global,
  not per-request — and is intentionally left alone). }
  FIsStreaming := False;
  FStreamMethod := nil;
  FStreamCallback := nil;

  if Assigned(FContent) then
    FreeAndNil(FContent);

  if Assigned(FCustomHeaders) then
    FCustomHeaders.Clear;
{ REPEATHDR-1 — wipe the duplicate-preserving store on pool recycle. }
  if Assigned(FRepeatHeaders) then
    FRepeatHeaders.Clear;
{ PATCH-RES-4 — wipe CrossSocket shadow fields }
  FCSBody          := '';
  FCSBodyBytes     := nil;
  FCSContentType   := '';
{ PATCH-SENDFILE-1 — free the owned copy (SendFile/Download); else just nil it. }
  if FCSOwnsContentStream and Assigned(FCSContentStream) then
    FreeAndNil(FCSContentStream)
  else
    FCSContentStream := nil;
  FCSOwnsContentStream := False;
  FCSStatusCode    := 200;
{ end PATCH-RES-4 }
{ PATCH-RES-6 — free the per-request TWebResponse adapter (owned).
  Nil on the Indy path (never assigned there); owned on CrossSocket path. }
  if Assigned(FCSRawWebResponse) then
    FreeAndNil(FCSRawWebResponse);
{ end PATCH-RES-6 }
{ PATCH-COOKIE-1 — drop the owned cookies on pool recycle. }
  if Assigned(FCookies) then
    FCookies.Clear;
{ end PATCH-COOKIE-1 }
end;
{ =========================================================================== }

function THorseResponse.Abort: THorseResponse;
begin
  Result := Self;
  FAborted := True;
end;

{ ===========================================================================
  PATCH-RES-6 — SetCSRawWebResponse implementation
  =========================================================================== }
procedure THorseResponse.SetCSRawWebResponse(
  const ARawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF});
begin
  if Assigned(FCSRawWebResponse) and (FCSRawWebResponse <> ARawWebResponse) then
    FreeAndNil(FCSRawWebResponse);
  FCSRawWebResponse := ARawWebResponse;
end;

destructor THorseResponse.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
{ ===========================================================================
  PATCH-RES-1 — free FCustomHeaders
  =========================================================================== }
  if Assigned(FCustomHeaders) then
    FCustomHeaders.Free;
{ REPEATHDR-1 — free the duplicate-preserving store. }
  if Assigned(FRepeatHeaders) then
    FRepeatHeaders.Free;
{ =========================================================================== }
{ PATCH-RES-6 — free the owned TWebResponse adapter if Clear was not called
  before Destroy (e.g. pool shutdown path). }
  if Assigned(FCSRawWebResponse) then
    FCSRawWebResponse.Free;
{ end PATCH-RES-6 }
{ PATCH-SENDFILE-1 — free the owned SendFile/Download copy if still held. }
  if FCSOwnsContentStream and Assigned(FCSContentStream) then
    FreeAndNil(FCSContentStream);
{ PATCH-COOKIE-1 — free the owned cookie list. }
  if Assigned(FCookies) then
    FreeAndNil(FCookies);
  inherited;
end;

function THorseResponse.RawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
begin
{ PATCH-RES-6 — return the CrossSocket adapter when FWebResponse is nil,
  so middleware calling Res.RawWebResponse.SetCustomHeader (e.g. Horse.CORS)
  works on the CrossSocket path without an AV. }
  if Assigned(FWebResponse) then
    Exit(FWebResponse);
  Result := FCSRawWebResponse;
{ end PATCH-RES-6 }
end;

function THorseResponse.Send(const AContent: TBytes): THorseResponse;
var
  LContent: TBytes;
  LStream: TStream;
begin
  LContent := AContent;
  THorseCore.ExecuteOnSend(THorseRequest(FRequest), Self, LContent);
  if not Assigned(FWebResponse) then
  begin
    FCSBodyBytes := LContent;
    Exit(Self);
  end;
  if Length(LContent) = 0 then
  begin
    FWebResponse.ContentStream := THorseMemoryBufferPool.DefaultPool.AcquireStream;
    FWebResponse.ContentLength := 0;
    Exit(Self);
  end;
  LStream := THorseMemoryBufferPool.DefaultPool.AcquireStream;
  LStream.WriteBuffer(LContent[0], Length(LContent));
  LStream.Position := 0;
  FWebResponse.ContentStream := LStream;
  Result := Self;
end;

function THorseResponse.Send(const AContent: string): THorseResponse;
var
  LContent: string;
begin
  LContent := AContent;
  THorseCore.ExecuteOnSend(THorseRequest(FRequest), Self, LContent);
{ PATCH-RES-4 — nil-guard }
  if not Assigned(FWebResponse) then
  begin
    FCSBody := LContent;
    Exit(Self);
  end;
{ end PATCH-RES-4 }
{$IF NOT DEFINED(FPC)}
{ PATCH-RES-5 — Indy empty-body fix
  When ContentText = '' and ContentStream = nil, TIdHTTPResponseInfo.WriteContent
  substitutes a default HTML body (<HTML><BODY><B>200 OK</B></BODY></HTML>).
  Assigning an empty TMemoryStream forces Indy into the stream path: it sends
  0 bytes and no HTML is generated.  FreeContentStream defaults to True so Indy
  owns and frees the stream. }
  if LContent = '' then
  begin
    FWebResponse.ContentStream := THorseMemoryBufferPool.DefaultPool.AcquireStream;
    FWebResponse.ContentLength := 0;
    Exit(Self);
  end;
{ end PATCH-RES-5 }
  FWebResponse.Content := LContent;
{$ELSE}
{ FPC-NEWLINE-1 — fpWeb's TResponse.Content is backed by FContents: TStrings;
  the fphttpserver wire path emits FContents.Text, which appends a trailing
  LineEnding to EVERY body ('pong' arrives as 'pong' plus #10; a 65536-byte
  body arrives as 65537) and trimming afterwards is impossible (TStrings cannot
  tell a genuine trailing newline from its own). A ContentStream is sent
  verbatim and takes precedence over Contents; FreeContentStream makes TResponse
  own it. An empty body still gets an empty stream so Content-Length is 0 with
  no spurious line ending. Scope: fpWeb-native providers only (FCSRawWebResponse
  = nil); adapter-backed providers (Epoll/IOCP/HttpSys) keep the Content path
  their validated bridges read. }
  if FCSRawWebResponse = nil then
  begin
    FWebResponse.FreeContentStream := True;
    FWebResponse.ContentStream := TStringStream.Create(LContent);
    FWebResponse.ContentLength := FWebResponse.ContentStream.Size;
  end
  else
    FWebResponse.Content := LContent;
{$ENDIF}
  Result := Self;
end;

function THorseResponse.Send<T>(AContent: T): THorseResponse;
begin
  FContent := AContent;
  Result := Self;
end;

function THorseResponse.RedirectTo(const ALocation: string): THorseResponse;
begin
{ PATCH-RES-4 — nil-guard: on CrossSocket path FWebResponse is nil;
  AddHeader already dual-writes to FCustomHeaders so Location is captured.
  Status delegates to FCSStatusCode when FWebResponse is nil. }
  if Assigned(FWebResponse) then
    FWebResponse.SetCustomHeader('Location', ALocation)
  else
    AddHeader('Location', ALocation);
{ end PATCH-RES-4 }
  Result := Status(THTTPStatus.SeeOther);
end;

function THorseResponse.RedirectTo(const ALocation: string; const AStatus: THTTPStatus): THorseResponse;
begin
{ PATCH-RES-4 — nil-guard }
  if Assigned(FWebResponse) then
    FWebResponse.SetCustomHeader('Location', ALocation)
  else
    AddHeader('Location', ALocation);
{ end PATCH-RES-4 }
  Result := Status(AStatus);
end;

function THorseResponse.RemoveHeader(const AName: string): THorseResponse;
var
  I: Integer;
begin
{ PATCH-RES-4 — nil-guard: skip FWebResponse access on CrossSocket path }
  if Assigned(FWebResponse) then
  begin
    I := FWebResponse.CustomHeaders.IndexOfName(AName);
    if I <> -1 then
      FWebResponse.CustomHeaders.Delete(I);
  end;
{ end PATCH-RES-4 }
{ ===========================================================================
  PATCH-RES-1 — also remove from FCustomHeaders
  PATCH-RES-7 — FCustomHeaders is allocated lazily; nothing to remove if it
  was never created (no AddHeader call ever ran).
  Delphi: TDictionary.Remove  FPC: TStringList delete by IndexOfName
  =========================================================================== }
  if FCustomHeaders <> nil then
  begin
{$IF NOT DEFINED(FPC)}
    FCustomHeaders.Remove(AName);
{$ELSE}
    I := FCustomHeaders.IndexOfName(AName);
    if I >= 0 then
      FCustomHeaders.Delete(I);
{$ENDIF}
  end;
{ =========================================================================== }
  Result := Self;
end;

{ ===========================================================================
  PATCH-COOKIE-1 — typed Set-Cookie API implementation
  =========================================================================== }
function THorseResponse.AddCookie(const ACookie: THorseCookie): THorseResponse;
begin
  Result := Self;
  if ACookie = nil then
    Exit;
  if FCookies = nil then
    FCookies := TObjectList<THorseCookie>.Create(True {AOwnsObjects});
  FCookies.Add(ACookie);
end;

function THorseResponse.Cookie(const AName, AValue: string): THorseCookie;
begin
  Result := THorseCookie.Create(AName, AValue);
  AddCookie(Result);
end;

procedure THorseResponse.FlushCookiesToWebResponse;
{ Indy path only. On FPC the fphttpserver TResponse.Cookies mapping is a
  follow-up (CrossSocket is the FPC transport); kept a no-op so FPC builds are
  unaffected. CrossSocket/mORMot never reach the body (FWebResponse is nil). }
var
  LCookie:    THorseCookie;
{$IFNDEF FPC}
  LState:     THorseCookieState;
  LWebCookie: TCookie;
{$ENDIF}
begin
  if (FWebResponse = nil) or (FCookies = nil) then
    Exit;

{$IFDEF FPC}
  { fpWeb supports duplicate custom headers and already serializes entries in
    name=value form. Writing the cookie's complete RFC 6265 value preserves
    Max-Age and emits one independent Set-Cookie line per cookie. }
  for LCookie in FCookies do
    FWebResponse.CustomHeaders.Add(
      'Set-Cookie=' + LCookie.ToHeaderValue
    );
{$ELSE}
  for LCookie in FCookies do
  begin
    LState := LCookie.State;
    LWebCookie := FWebResponse.Cookies.Add;
    LWebCookie.Name  := LState.Name;
    LWebCookie.Value := LState.Value;
    if LState.Domain <> '' then
      LWebCookie.Domain := LState.Domain;
    if LState.Path <> '' then
      LWebCookie.Path := LState.Path;
    if LState.HasExpires then
      LWebCookie.Expires := LState.Expires;   // TCookie has no Max-Age — use .Expires() on Indy
    LWebCookie.Secure := LState.Secure;
{$IF CompilerVersion >= 31}   // Delphi 10.1 Berlin+ — TCookie.HttpOnly
    LWebCookie.HttpOnly := LState.HttpOnly;
{$IFEND}
{$IF CompilerVersion >= 34}   // Delphi 10.4 Sydney+ — TCookie.SameSite (string)
    case LState.SameSite of
      ssStrict: LWebCookie.SameSite := 'Strict';
      ssLax:    LWebCookie.SameSite := 'Lax';
      ssNone:   LWebCookie.SameSite := 'None';
    end;
{$IFEND}
  end;
{$ENDIF}
end;
{ end PATCH-COOKIE-1 }

function THorseResponse.Status(const AStatus: THTTPStatus): THorseResponse;
begin
{ PATCH-RES-4 — nil-guard }
  if not Assigned(FWebResponse) then
  begin
    FCSStatusCode := AStatus.ToInteger;
    Exit(Self);
  end;
{ end PATCH-RES-4 }
{$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := AStatus.ToInteger;
  Result := Self;
end;

function THorseResponse.SendFile(const AFileStream: TStream; const AFileName: string; const AContentType: string): THorseResponse;
var
  LFileName:    string;
  LContentType: string;
begin
  Result := Self;
  AFileStream.Position := 0;
  LFileName    := ExtractFileName(AFileName);
  LContentType := AContentType;
  if LContentType = EmptyStr then
    LContentType := Horse.Mime.THorseMimeTypes.GetFileType(LFileName);

{ PATCH-RES-4 / PATCH-SENDFILE-1 — nil-guard: on the CrossSocket/mORMot path,
  COPY the source into a response-owned stream.  The response is flushed AFTER
  the handler returns, so a non-owning reference would dangle the moment the
  caller frees AFileStream (the common `try SendFile finally FreeAndNil` idiom);
  copying decouples us from the caller's stream lifetime. }
  if not Assigned(FWebResponse) then
  begin
    if FCSOwnsContentStream and Assigned(FCSContentStream) then
      FreeAndNil(FCSContentStream);
    FCSContentStream := TMemoryStream.Create;
    AFileStream.Position := 0;
    if AFileStream.Size > 0 then
      TMemoryStream(FCSContentStream).CopyFrom(AFileStream, AFileStream.Size);
    FCSContentStream.Position := 0;
    FCSOwnsContentStream := True;
    FCSContentType   := LContentType;
    AddHeader('Content-Disposition', Format('inline; filename="%s"', [LFileName]));
    Exit;
  end;
{ end PATCH-RES-4 / PATCH-SENDFILE-1 }

  FWebResponse.FreeContentStream := False;
  FWebResponse.ContentLength := AFileStream.Size;
  FWebResponse.ContentStream := AFileStream;
  FWebResponse.SetCustomHeader('Content-Disposition', Format('inline; filename="%s"', [LFileName]));
  FWebResponse.ContentType := LContentType;
  if (LContentType = EmptyStr) then
    FWebResponse.ContentType := Horse.Mime.THorseMimeTypes.GetFileType(LFileName);

{$IF DEFINED(FPC)}
  FWebResponse.SendContent;
{$ELSE}
  FWebResponse.SendResponse;
{$ENDIF}
end;

function THorseResponse.SendFile(const AFileName: string; const AContentType: string): THorseResponse;
var
  LFile: THorseCoreFile;
  LContentType: string;
begin
  Result := Self;

  LFile := THorseCoreFile.Create(AFileName);
  LFile.FreeContentStream := True;
  try
    LContentType := AContentType;
    if (AContentType = EmptyStr) then
      LContentType := LFile.ContentType;
    SendFile(LFile.ContentStream, LFile.Name, LContentType);
  finally
    LFile.Free;
  end;
end;

function THorseResponse.Download(const AFileStream: TStream; const AFileName: string; const AContentType: string): THorseResponse;
var
  LFileName:    string;
  LContentType: string;
begin
  Result := Self;
  AFileStream.Position := 0;
  LFileName    := ExtractFileName(AFileName);
  LContentType := AContentType;
  if LContentType = EmptyStr then
    LContentType := Horse.Mime.THorseMimeTypes.GetFileType(LFileName);

{ PATCH-RES-4 / PATCH-SENDFILE-1 — nil-guard: copy into a response-owned stream
  so the caller may free AFileStream immediately (flush happens post-handler). }
  if not Assigned(FWebResponse) then
  begin
    if FCSOwnsContentStream and Assigned(FCSContentStream) then
      FreeAndNil(FCSContentStream);
    FCSContentStream := TMemoryStream.Create;
    AFileStream.Position := 0;
    if AFileStream.Size > 0 then
      TMemoryStream(FCSContentStream).CopyFrom(AFileStream, AFileStream.Size);
    FCSContentStream.Position := 0;
    FCSOwnsContentStream := True;
    FCSContentType   := LContentType;
    AddHeader('Content-Disposition', Format('attachment; filename="%s"', [LFileName]));
    Exit;
  end;
{ end PATCH-RES-4 / PATCH-SENDFILE-1 }

  FWebResponse.FreeContentStream := False;
  FWebResponse.ContentLength := AFileStream.Size;
  FWebResponse.ContentStream := AFileStream;
  FWebResponse.SetCustomHeader('Content-Disposition', Format('attachment; filename="%s"', [LFileName]));
  FWebResponse.ContentType := LContentType;
  if (LContentType = EmptyStr) then
    FWebResponse.ContentType := Horse.Mime.THorseMimeTypes.GetFileType(LFileName);

{$IF DEFINED(FPC)}
  FWebResponse.SendContent;
{$ELSE}
  FWebResponse.SendResponse;
{$ENDIF}
end;

function THorseResponse.Download(const AFileName: string; const AContentType: string): THorseResponse;
var
  LFile: THorseCoreFile;
  LContentType: string;
begin
  Result := Self;

  LFile := THorseCoreFile.Create(AFileName);
  LFile.FreeContentStream := True;
  try
    LContentType := AContentType;
    if (AContentType = EmptyStr) then
      LContentType := LFile.ContentType;
    Download(LFile.ContentStream, LFile.Name, LContentType);
  finally
    LFile.Free;
  end;
end;

function THorseResponse.Render(const AFileStream: TStream;
  const AFileName: string): THorseResponse;
begin
  Result := Self;
  SendFile(AFileStream, AFileName, Horse.Commons.TMimeTypes.TextHTML.ToString);
end;

function THorseResponse.Render(const AFileName: string): THorseResponse;
begin
  Result := Self;
  SendFile(AFileName, Horse.Commons.TMimeTypes.TextHTML.ToString);
end;

function THorseResponse.Status: Integer;
begin
{ PATCH-RES-4 — nil-guard }
  if not Assigned(FWebResponse) then
    Exit(FCSStatusCode);
{ end PATCH-RES-4 }
  Result := {$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF};
end;

function THorseResponse.Status(const AStatus: Integer): THorseResponse;
begin
{ PATCH-RES-4 — nil-guard }
  if not Assigned(FWebResponse) then
  begin
    FCSStatusCode := AStatus;
    Exit(Self);
  end;
{ end PATCH-RES-4 }
{$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := AStatus;
  Result := Self;
end;

function THorseResponse.UpgradeToWebSocket(const AOnConnect: TOnWebSocketConnect): IHorseWebSocketConnection;
var
  LUpgraderObj: TObject;
  LUpgrader: THorseWebSocketUpgrader;
  LConnection: IHorseWebSocketConnection;
  LRequest: THorseRequest;
begin
  LRequest := THorseRequest(FRequest);
  if not LRequest.IsWebSocket then
  begin
    Status(400).Send('Request is not a WebSocket upgrade request.');
    raise EHorseCallbackInterrupted.Create('Request is not a WebSocket upgrade request.');
  end;

  LUpgraderObj := LRequest.Services.Resolve(THorseWebSocketUpgrader);
  if not Assigned(LUpgraderObj) then
  begin
    Status(501).Send('WebSockets are not supported by the active provider.');
    raise EHorseCallbackInterrupted.Create('WebSockets are not supported by the active provider.');
  end;

  LUpgrader := THorseWebSocketUpgrader(LUpgraderObj);
  LConnection := LUpgrader.Upgrade(LRequest.PathInfo, 30);
  
  if Assigned(AOnConnect) then
  begin
    try
      AOnConnect(LConnection);
    except
      on E: Exception do
      begin
        LConnection.TriggerError(E);
        LConnection.Close(1011, 'Internal Error');
        raise;
      end;
    end;
  end;
  
  Result := LConnection;
end;

{ THorseStreamWriterBase }

constructor THorseStreamWriterBase.Create(const AResponse: THorseResponse);
var
  LProtocol: string;
  LContentType: string;
begin
  inherited Create;
  FResponse := AResponse;
  FHeadersSent := False;

  LContentType := FResponse.CSContentType;
  if FResponse.RawWebResponse <> nil then
    LContentType := FResponse.RawWebResponse.ContentType;

  LProtocol := '';
  if FResponse.Request <> nil then
  begin
    if THorseRequest(FResponse.Request).RawWebRequest <> nil then
      LProtocol := THorseRequest(FResponse.Request).RawWebRequest.ProtocolVersion;
  end;

  FUseChunked := (not SameText(LProtocol, 'HTTP/2')) and
                 (not SameText(LContentType, 'text/event-stream'));
end;

procedure THorseStreamWriterBase.SendHeaders;
begin
  if FHeadersSent then Exit;
  if FUseChunked then
    FResponse.AddHeader('Transfer-Encoding', 'chunked');
  SendRawHeaders;
  FHeadersSent := True;
end;

procedure THorseStreamWriterBase.Write(const AText: string);
begin
  Write(TEncoding.UTF8.GetBytes(AText));
end;

procedure THorseStreamWriterBase.Write(const ABytes: TBytes);
var
  LChunkHeader: string;
  LChunkBytes: TBytes;
begin
  SendHeaders;
  if Length(ABytes) = 0 then Exit;

  if FUseChunked then
  begin
    LChunkHeader := Format('%x'#13#10, [Length(ABytes)]);
    LChunkBytes := TEncoding.UTF8.GetBytes(LChunkHeader);

    WriteRawBytes(LChunkBytes);
    WriteRawBytes(ABytes);
    WriteRawBytes(TEncoding.UTF8.GetBytes(#13#10));
  end
  else
  begin
    WriteRawBytes(ABytes);
  end;

  Flush;
end;

procedure THorseStreamWriterBase.Flush;
begin
end;

procedure THorseStreamWriterBase.Close;
begin
  if FHeadersSent and FUseChunked then
  begin
    WriteRawBytes(TEncoding.UTF8.GetBytes('0'#13#10#13#10));
    Flush;
  end;
end;

{ THorseResponse }

class procedure THorseResponse.RegisterStreamWriterFactory(const AFactory: THorseStreamWriterFactory);
begin
  FStreamWriterFactory := AFactory;
end;

function THorseResponse.SendStream(const ACallback: THorseStreamProc): THorseResponse;
var
  LWriter: IHorseStreamWriter;
begin
  Result := Self;
  FIsStreaming := True;
  FStreamMethod := ACallback;

  if not Assigned(FStreamWriterFactory) then
    raise Exception.Create('Nenhum provedor de streaming registrado.');

  LWriter := FStreamWriterFactory(Self);
  try
    ACallback(LWriter);
  finally
    LWriter.Close;
  end;
end;

function THorseResponse.SendStream(const ACallback: THorseStreamAnonProc): THorseResponse;
var
  LWriter: IHorseStreamWriter;
begin
  Result := Self;
  FIsStreaming := True;
  FStreamCallback := ACallback;

  if not Assigned(FStreamWriterFactory) then
    raise Exception.Create('Nenhum provedor de streaming registrado.');

  LWriter := FStreamWriterFactory(Self);
  try
    ACallback(LWriter);
  finally
    LWriter.Close;
  end;
end;

{ THorseWebBrokerStreamWriter }

procedure THorseWebBrokerStreamWriter.SendRawHeaders;
begin
  if FResponse.RawWebResponse <> nil then
  begin
    if FResponse.RawWebResponse.ContentType = '' then
      FResponse.RawWebResponse.ContentType := 'text/plain';

    {$IF NOT DEFINED(FPC)}
    if FResponse.RawWebResponse is TIdHTTPAppResponse then
    begin
      TIdHTTPAppResponseFriend(FResponse.RawWebResponse).FResponseInfo.ResponseNo := FResponse.Status;
      TIdHTTPAppResponseFriend(FResponse.RawWebResponse).FResponseInfo.ResponseText := 'OK';
      TIdHTTPAppResponseFriend(FResponse.RawWebResponse).FResponseInfo.ContentType := FResponse.RawWebResponse.ContentType;
      
      // Evita o HTML default de 39 bytes do Indy no final do request
      TIdHTTPAppResponseFriend(FResponse.RawWebResponse).FResponseInfo.ContentStream := TMemoryStream.Create;
      TIdHTTPAppResponseFriend(FResponse.RawWebResponse).FResponseInfo.FreeContentStream := True;
      TIdHTTPAppResponseFriend(FResponse.RawWebResponse).FResponseInfo.ContentLength := 0;

      TIdHTTPAppResponseFriend(FResponse.RawWebResponse).MoveCookiesAndCustomHeaders;
      TIdHTTPAppResponseFriend(FResponse.RawWebResponse).FResponseInfo.WriteHeader;
      TIdHTTPAppResponseFriend(FResponse.RawWebResponse).FSent := True;
      Exit;
    end;
    {$ENDIF}

    {$IF DEFINED(FPC)}
    FResponse.RawWebResponse.SendHeaders;
    {$ELSE}
    FResponse.RawWebResponse.SendResponse;
    {$ENDIF}
  end;
end;

procedure THorseWebBrokerStreamWriter.WriteRawBytes(const ABytes: TBytes);
begin
  if Length(ABytes) = 0 then
    Exit;

  if FResponse.RawWebResponse = nil then
    Exit;

  if FResponse.RawWebResponse <> nil then
  begin
    {$IF DEFINED(FPC)}
    if FResponse.RawWebResponse is TFPHTTPConnectionResponse then
    begin
      TFPHTTPConnectionResponseAccess(
        FResponse.RawWebResponse
      ).WriteToSocket(ABytes);
    end
    else if FResponse.RawWebResponse.ContentStream <> nil then
    begin
      FResponse.RawWebResponse.ContentStream.Write(
        ABytes[0],
        Length(ABytes)
      );
    end;    
    {$ELSE}
    if (FResponse.Request <> nil) and (THorseRequest(FResponse.Request).RawWebRequest <> nil) then
      TWebRequestFriend(THorseRequest(FResponse.Request).RawWebRequest).WriteClient(ABytes[0], Length(ABytes));
    {$ENDIF}
  end;
end;

function THorseWebBrokerStreamWriter.IsConnected: Boolean;
begin
  Result := False;

  if FResponse.RawWebResponse = nil then
    Exit;

  {$IF DEFINED(FPC)}
  if FResponse.RawWebResponse is TFPHTTPConnectionResponse then
    Result :=  TFPHTTPConnectionResponseAccess(FResponse.RawWebResponse).ClientConnected
  {$ELSE}
    if FResponse.RawWebResponse is TIdHTTPAppResponse then
      if TIdHTTPAppResponseFriend(FResponse.RawWebResponse).FThread <> nil then
        Result := TIdHTTPAppResponseFriend(FResponse.RawWebResponse).FThread.Connection.Connected;
  {$ENDIF}
end;

function DefaultWebBrokerStreamWriterFactory(const AResponse: THorseResponse): IHorseStreamWriter;
begin
  Result := THorseWebBrokerStreamWriter.Create(AResponse);
end;

initialization
  THorseResponse.RegisterStreamWriterFactory(DefaultWebBrokerStreamWriterFactory);

end.
