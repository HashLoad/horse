unit Horse.Request;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  fpHTTP,
  HTTPDefs,
{$ELSE}
  System.SysUtils,
  Web.HTTPApp,
{$IF CompilerVersion > 32.0}
  Web.ReqMulti,
{$ENDIF}
{$ENDIF}
  Horse.Core.Param,
  Horse.Session,
  Horse.Commons;

type
  THorseRequest = class
  private
    FWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
    FHeaders: THorseCoreParam;
    FQuery: THorseCoreParam;
    FParams: THorseCoreParam;
    FContentFields: THorseCoreParam;
    FCookie: THorseCoreParam;
    FBody: TObject;
    FSession: TObject;
    FSessions: THorseSessions;
{ ===========================================================================
  PATCH-REQ-3 — CrossSocket shadow fields (populated by Populate, nil by default)
  =========================================================================== }
    FCSMethod:      string;
    FCSMethodType:  TMethodType;
    FCSPathInfo:    string;
    FCSContentType: string;
    FCSRemoteAddr:  string;
{ =========================================================================== }
    procedure InitializeQuery;
    procedure InitializeParams;
    procedure InitializeContentFields;
    procedure InitializeCookie;
    function IsMultipartForm: Boolean;
    function IsFormURLEncoded: Boolean;
    function CanLoadContentFields: Boolean;
  public
    function Body: string; overload; virtual;
    function Body<T: class>: T; overload;
    function Body(const ABody: TObject): THorseRequest; overload; virtual;
    function Session<T: class>: T; overload;
    function Session(const ASession: TObject): THorseRequest; overload; virtual;
    function Headers: THorseCoreParam; virtual;
    function Query: THorseCoreParam; virtual;
    function Params: THorseCoreParam; virtual;
    function Cookie: THorseCoreParam; virtual;
    function ContentFields: THorseCoreParam; virtual;
    function Sessions: THorseSessions; virtual;
    function MethodType: TMethodType; virtual;
    function ContentType: string; virtual;
    function Host: string; virtual;
    function PathInfo: string; virtual;
    function RawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}; virtual;
    constructor Create(const AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}); overload;
{ ===========================================================================
  PATCH-REQ-1 — added parameterless constructor overload
  Reason: THorseContextPool.WarmUp pre-allocates THorseRequest instances at
  application startup, before any HTTP request arrives and before any
  TWebRequest exists. The pool calls this overload; the original constructor
  is completely unchanged and continues to be used by the Indy provider.
  =========================================================================== }
    constructor Create; overload;
{ =========================================================================== }
{ ===========================================================================
  PATCH-REQ-2 — added Clear procedure
  Reason: THorseContext.Reset recycles pooled objects between requests
  without Free/Create overhead. Rules enforced:
    • FBody  — set to nil, NEVER freed (non-owning CrossSocket buffer ref)
    • FSession — set to nil (stale session = wrong-request auth)
    • FWebRequest — set to nil (belongs to previous Indy context)
    • param collections — cleared in place, objects reused
  =========================================================================== }
    procedure Clear;
{ =========================================================================== }
{ ===========================================================================
  PATCH-REQ-3 — added Populate procedure and RemoteAddr function
  Reason: The CrossSocket bridge must inject per-request values directly
  into THorseRequest without a live TWebRequest.  All fields below were
  previously read-only delegations to FWebRequest; they now have private
  shadow fields that are populated here and returned when FWebRequest is nil.

  Fields injected via Populate:
    FCSMethod      — method string ('GET','POST',…)
    FCSMethodType  — parsed TMethodType
    FCSPathInfo    — decoded path ('/api/users/1')
    FCSContentType — Content-Type header value
    FCSRemoteAddr  — real peer socket address

  RemoteAddr is a new public function (no equivalent existed before).
  MethodType, ContentType, PathInfo fall through to the CS fields when
  FWebRequest is nil (nil-guard added to each implementation).
  =========================================================================== }
    procedure Populate(
      const AMethod:      string;
      AMethodType:        TMethodType;
      const APath:        string;
      const AContentType: string;
      const ARemoteAddr:  string
    );
    function RemoteAddr: string; virtual;
{ ===========================================================================
  PATCH-REQ-4 — PopulateCookiesFromHeader
  Parses the raw "Cookie: name=value; name2=value2" header string into the
  FCookie param collection.  Called by the CrossSocket request bridge after
  Populate() so that Req.Cookie works on the CrossSocket path without any
  dependency on FWebRequest.
  =========================================================================== }
    procedure PopulateCookiesFromHeader(const ACookieHeader: string);
{ =========================================================================== }
    destructor Destroy; override;
  end;

implementation

uses      
{$IF DEFINED(FPC)}
  Classes,
{$ELSE}
  System.Classes,
{$ENDIF}
  Horse.Core.Param.Header;

{ ===========================================================================
  PATCH-REQ-5 — Body: string nil-guard
  On the CrossSocket path FWebRequest is nil.  FBody holds a non-owning
  reference to the CrossSocket receive buffer (a TStream).  Read and return
  its contents as a UTF-8 string.  If FBody is nil or not a stream, return
  an empty string so that callers that check Body <> '' still work.
  =========================================================================== }
function THorseRequest.Body: string;
{$IF NOT DEFINED(FPC)}
var
  LStream: TStream;
  LBytes:  TBytes;
{$ENDIF}
begin
  if not Assigned(FWebRequest) then
  begin
{$IF DEFINED(FPC)}
    Result := '';
{$ELSE}
    if Assigned(FBody) and (FBody is TStream) then
    begin
      LStream := TStream(FBody);
      LStream.Position := 0;
      SetLength(LBytes, LStream.Size);
      if LStream.Size > 0 then
        LStream.Read(LBytes[0], LStream.Size);
      Result := TEncoding.UTF8.GetString(LBytes);
    end
    else
      Result := '';
{$ENDIF}
    Exit;
  end;
  Result := FWebRequest.Content;
end;

function THorseRequest.Body(const ABody: TObject): THorseRequest;
begin
  Result := Self;
  if Assigned(FBody) then
    FBody.Free;
  FBody := ABody;
end;

function THorseRequest.Body<T>: T;
begin
  Result := T(FBody);
end;

function THorseRequest.CanLoadContentFields: Boolean;
begin
  Result := IsMultipartForm or IsFormURLEncoded;
end;

function THorseRequest.ContentFields: THorseCoreParam;
begin
  if not Assigned(FContentFields) then
    InitializeContentFields;
  Result := FContentFields;
end;

function THorseRequest.Cookie: THorseCoreParam;
begin
  if not Assigned(FCookie) then
    InitializeCookie;
  Result := FCookie;
end;

constructor THorseRequest.Create(const AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF});
begin
  FWebRequest := AWebRequest;
  FSessions := THorseSessions.Create;
end;

{ ===========================================================================
  PATCH-REQ-1 — parameterless constructor implementation
  =========================================================================== }
constructor THorseRequest.Create;
begin
  FWebRequest := nil;
  FSessions := THorseSessions.Create;
end;
{ =========================================================================== }

{ ===========================================================================
  PATCH-REQ-2 — Clear implementation
  THorseCoreParam owns FParams (a TDictionary<string,string>) which is
  exposed via the public Dictionary property. Calling Dictionary.Clear
  wipes all entries in-place without freeing the THorseCoreParam object
  itself, so the next request reuses the same objects with no heap churn.
  FContent is a lazy TStrings cache inside THorseCoreParam — it is freed
  and nil-ed here via FreeAndNil on the param object then recreated by the
  next InitializeXxx call. Because FContent is private to THorseCoreParam
  the cleanest way to reset it is to FreeAndNil the whole THorseCoreParam
  and let the lazy accessor rebuild it, which is what the destructor does.
  We therefore use Dictionary.Clear for the hot-path wipe and rely on the
  lazy InitializeXxx pattern (already used everywhere in this class) when
  a full reset including FContent is required.
  Strategy per field:
    FHeaders      — Dictionary.Clear  (header map reused, no FContent used)
    FQuery        — FreeAndNil + lazy rebuild via InitializeQuery
    FParams       — Dictionary.Clear  (route params repopulated by router)
    FContentFields— FreeAndNil + lazy rebuild via InitializeContentFields
    FCookie       — FreeAndNil + lazy rebuild via InitializeCookie
    FSessions     — FreeAndNil + Create (no lazy init method exists)
  =========================================================================== }
procedure THorseRequest.Clear;
begin
  FWebRequest := nil;
  // FBody: non-owning reference into CrossSocket's socket buffer.
  // Must be set to nil here. NEVER call FBody.Free — doing so corrupts
  // the live TCP connection. The pool Reset sets FBody := nil before
  // calling Clear, but we enforce the contract here as a safety net.
  FBody := nil;
  FSession := nil;
{ PATCH-REQ-3 — wipe shadow fields so next request starts clean }
  FCSMethod      := '';
  FCSMethodType  := mtAny;
  FCSPathInfo    := '';
  FCSContentType := '';
  FCSRemoteAddr  := '';
{ end PATCH-REQ-3 }
  if Assigned(FHeaders) then
    FHeaders.Dictionary.Clear;
  if Assigned(FQuery) then
    FreeAndNil(FQuery);
  if Assigned(FParams) then
    FParams.Dictionary.Clear;
  if Assigned(FContentFields) then
    FreeAndNil(FContentFields);
  if Assigned(FCookie) then
    FreeAndNil(FCookie);
  if Assigned(FSessions) then
    FreeAndNil(FSessions);
  FSessions := THorseSessions.Create;
end;
{ =========================================================================== }

destructor THorseRequest.Destroy;
begin
  if Assigned(FHeaders) then
    FreeAndNil(FHeaders);
  if Assigned(FQuery) then
    FreeAndNil(FQuery);
  if Assigned(FParams) then
    FreeAndNil(FParams);
  if Assigned(FContentFields) then
    FreeAndNil(FContentFields);
  if Assigned(FCookie) then
    FreeAndNil(FCookie);
  if Assigned(FBody) then
    FBody.Free;
  if Assigned(FSessions) then
    FSessions.Free;
  inherited;
end;

function THorseRequest.Headers: THorseCoreParam;
var
  LParam: THorseList;
begin
  if not Assigned(FHeaders) then
  begin
{ PATCH-REQ-3 — nil-guard: when FWebRequest is nil (CrossSocket path),
  Populate already called InitializeHeaders which set FHeaders.
  If somehow we arrive here with FWebRequest=nil and FHeaders=nil,
  create an empty param rather than crashing on GetHeaders(nil). }
    if not Assigned(FWebRequest) then
    begin
      FHeaders := THorseCoreParam.Create(THorseList.Create).Required(False);
      Exit(FHeaders);
    end;
{ end PATCH-REQ-3 }
    LParam := THorseCoreParamHeader.GetHeaders(FWebRequest);
    FHeaders := THorseCoreParam.Create(LParam).Required(False);
  end;
  Result := FHeaders;
end;

{ ===========================================================================
  PATCH-REQ-6 — Host nil-guard
  On the CrossSocket path FWebRequest is nil.  Return the Host header value
  from the already-populated FHeaders dictionary.  The request bridge
  validates Host ([SEC-17]) before populating FHeaders, so it is always
  present on valid CrossSocket requests.
  =========================================================================== }
function THorseRequest.Host: string;
begin
  if not Assigned(FWebRequest) then
  begin
    Result := '';
    if Assigned(FHeaders) then
      FHeaders.Dictionary.TryGetValue('Host', Result);
    Exit;
  end;
  Result := FWebRequest.Host;
end;

function THorseRequest.ContentType: string;
begin
{ PATCH-REQ-3 — nil-guard }
  if not Assigned(FWebRequest) then
    Exit(FCSContentType);
{ end PATCH-REQ-3 }
  Result := FWebRequest.ContentType;
end;

function THorseRequest.PathInfo: string;
var
  LPrefix: string;
begin
{ PATCH-REQ-3 — nil-guard }
  if not Assigned(FWebRequest) then
    Exit(FCSPathInfo);
{ end PATCH-REQ-3 }
  LPrefix := EmptyStr;
  if FWebRequest.PathInfo = EmptyStr then
    LPrefix := '/';
  Result := LPrefix + FWebRequest.PathInfo;
end;

procedure THorseRequest.InitializeContentFields;
{$IF NOT DEFINED(FPC)}
const
  CONTENT_DISPOSITION = 'Content-Disposition: form-data; name=';
{$ENDIF}
var
  I: Integer;
  LName: String;
  LValue: String;
begin
  FContentFields := THorseCoreParam.Create(THorseList.Create).Required(False);
{ PATCH-REQ-4 — nil-guard: on CrossSocket path FWebRequest is nil.
  Multipart / form-url-encoded body parsing is the responsibility of
  application-level middleware on the CrossSocket path (e.g. a middleware
  that reads Req.Body and parses it manually).  We simply return an empty
  param collection here rather than crashing on nil FWebRequest. }
  if not Assigned(FWebRequest) then
    Exit;
{ end PATCH-REQ-4 }
  if (not CanLoadContentFields) then
    Exit;

  for I := 0 to Pred(FWebRequest.Files.Count) do
    FContentFields.AddStream(FWebRequest.Files[I].FieldName, FWebRequest.Files[I].Stream);

  for I := 0 to Pred(FWebRequest.ContentFields.Count) do
  begin
    if IsMultipartForm then
    begin
{$IF DEFINED(FPC)}
      LName := FWebRequest.ContentFields.Names[I];
      LValue := FWebRequest.ContentFields.ValueFromIndex[I];
{$ELSE}
{$IF CompilerVersion <= 31.0}
      if FWebRequest.ContentFields[I].StartsWith(CONTENT_DISPOSITION) then
      begin
        LName := FWebRequest.ContentFields[I]
          .Replace(CONTENT_DISPOSITION, EmptyStr)
          .Replace('"', EmptyStr);
        LValue := FWebRequest.ContentFields[I + 1];
      end;
{$ELSE}
      LName := FWebRequest.ContentFields.Names[I];
      LValue := FWebRequest.ContentFields.ValueFromIndex[I];
{$ENDIF}
{$ENDIF}
    end
    else
    begin
      LName := FWebRequest.ContentFields.Names[I];
      LValue := FWebRequest.ContentFields.ValueFromIndex[I];
    end;

    if LName <> EmptyStr then
      FContentFields.Dictionary.AddOrSetValue(LName, LValue);

    LName := EmptyStr;
    LValue := EmptyStr;
  end;
end;

procedure THorseRequest.InitializeCookie;
const
  KEY = 0;
  VALUE = 1;
var
  LParam: TArray<string>;
  LItem: string;
begin
  FCookie := THorseCoreParam.Create(THorseList.Create).Required(False);
{ PATCH-REQ-4 — nil-guard: on CrossSocket path FWebRequest is nil.
  Cookie parsing from the raw header string is handled by
  THorseRequest.PopulateCookiesFromHeader, called by the CrossSocket bridge
  after Populate().  Nothing to do here on that path. }
  if not Assigned(FWebRequest) then
    Exit;
{ end PATCH-REQ-4 }
  for LItem in FWebRequest.CookieFields do
  begin
    LParam := LItem.Split(['=']);
    FCookie.Dictionary.AddOrSetValue(LParam[KEY], LParam[VALUE]);
  end;
end;

procedure THorseRequest.InitializeParams;
begin
  FParams := THorseCoreParam.Create(THorseList.Create).Required(True);
end;

{ ===========================================================================
  PATCH-REQ-7 — InitializeQuery nil-guard
  On the CrossSocket path FWebRequest is nil.  Query parameters are
  pre-populated by the CrossSocket request bridge directly via
  AHorseReq.Query.Dictionary.AddOrSetValue, which triggers this method
  as the lazy initialiser (FQuery is nil on first call).  We create the
  empty param collection and return early — the bridge then populates it.
  Accessing FWebRequest.QueryFields on the CrossSocket path would crash.
  =========================================================================== }
procedure THorseRequest.InitializeQuery;
var
  LItem, LKey, LValue: string;
  LEqualFirstPos: Integer;
begin
  FQuery := THorseCoreParam.Create(THorseList.Create).Required(False);
  if not Assigned(FWebRequest) then
    Exit;  // CrossSocket path: bridge populates query dict directly
  for LItem in FWebRequest.QueryFields do
  begin
    LEqualFirstPos := Pos('=', LItem);
    LKey := Copy(LItem, 1, LEqualFirstPos - 1);
    LValue := Copy(LItem, LEqualFirstPos + 1, Length(LItem));
    if not FQuery.Dictionary.ContainsKey(LKey) then
      FQuery.Dictionary.AddOrSetValue(LKey, LValue)
    else
      FQuery.Dictionary[LKey] := FQuery.Dictionary[LKey] +','+ LValue;
  end;
end;

function THorseRequest.IsFormURLEncoded: Boolean;
var
  LContentType, LFormUrlEncoded: string;
begin
  LContentType := FWebRequest.ContentType;
  LFormUrlEncoded := TMimeTypes.ApplicationXWWWFormURLEncoded.ToString;
{$IF DEFINED(FPC)}
  Result := StrLIComp(PChar(LContentType), PChar(LFormUrlEncoded), Length(LFormUrlEncoded)) = 0;
{$ELSE}
{$IF CompilerVersion <= 30}
  Result := LContentType = PChar(LFormUrlEncoded);
{$ELSE}
  Result := StrLIComp(PChar(LContentType), PChar(LFormUrlEncoded), Length(LFormUrlEncoded)) = 0;
{$IFEND}
{$ENDIF}
end;

function THorseRequest.IsMultipartForm: Boolean;
var
  LContentType, LFormData: string;
begin
  LContentType := FWebRequest.ContentType;
  LFormData := TMimeTypes.MultiPartFormData.ToString;
{$IF DEFINED(FPC)}
  Result := StrLIComp(PChar(LContentType), PChar(LFormData), Length(PChar(LFormData))) = 0;
{$ELSE}
{$IF CompilerVersion <= 30}
  Result := LContentType = PChar(LFormData);
{$ELSE}
  Result := StrLIComp(PChar(LContentType), PChar(LFormData), Length(PChar(LFormData))) = 0;
{$IFEND}
{$ENDIF}
end;

function THorseRequest.MethodType: TMethodType;
begin
{ PATCH-REQ-3 — nil-guard: return shadow field when FWebRequest is nil }
  if not Assigned(FWebRequest) then
    Exit(FCSMethodType);
{ end PATCH-REQ-3 }
  Result := {$IF DEFINED(FPC)}StringCommandToMethodType(FWebRequest.Method); {$ELSE}FWebRequest.MethodType; {$ENDIF}
end;

function THorseRequest.Params: THorseCoreParam;
begin
  if not Assigned(FParams) then
    InitializeParams;
  Result := FParams;
end;

function THorseRequest.Query: THorseCoreParam;
begin
  if not Assigned(FQuery) then
    InitializeQuery;
  Result := FQuery;
end;

function THorseRequest.RawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
begin
  Result := FWebRequest;
end;

function THorseRequest.Session(const ASession: TObject): THorseRequest;
begin
  Result := Self;
  FSession := ASession;
end;

function THorseRequest.Session<T>: T;
begin
  Result := T(FSession);
end;

function THorseRequest.Sessions: THorseSessions;
begin
  Result := FSessions;
end;

{ ===========================================================================
  PATCH-REQ-3 — Populate implementation
  Called once per request by the CrossSocket bridge AFTER the pool returns
  a context.  Sets the five shadow fields and pre-builds FHeaders so the
  lazy Headers() accessor never calls GetHeaders(nil).
  =========================================================================== }
procedure THorseRequest.Populate(
  const AMethod:      string;
  AMethodType:        TMethodType;
  const APath:        string;
  const AContentType: string;
  const ARemoteAddr:  string
);
begin
  FCSMethod      := AMethod;
  FCSMethodType  := AMethodType;
  FCSPathInfo    := APath;
  FCSContentType := AContentType;
  FCSRemoteAddr  := ARemoteAddr;

  // Pre-build FHeaders as an empty container so the lazy init in Headers()
  // never reaches THorseCoreParamHeader.GetHeaders(nil).
  // The bridge then populates it via FHeaders.Dictionary.AddOrSetValue.
  if not Assigned(FHeaders) then
    FHeaders := THorseCoreParam.Create(THorseList.Create).Required(False)
  else
    FHeaders.Dictionary.Clear;
end;

function THorseRequest.RemoteAddr: string;
begin
  Result := FCSRemoteAddr;
end;
{ =========================================================================== }

{ ===========================================================================
  PATCH-REQ-4 — PopulateCookiesFromHeader implementation
  Parses the RFC 6265 Cookie header value:
    "name=value; name2=value2; ..."
  Each pair is split on the first '=' so values that themselves contain '='
  (e.g. Base64) are preserved intact.
  Leading/trailing whitespace around names and values is trimmed.
  FCookie is pre-built by InitializeCookie (which returns early on the
  CrossSocket path), so we just call AddOrSetValue here.
  =========================================================================== }
procedure THorseRequest.PopulateCookiesFromHeader(const ACookieHeader: string);
var
  Pairs:    TArray<string>;
  Pair:     string;
  EqPos:    Integer;
  CName, CValue: string;
begin
  if ACookieHeader = '' then
    Exit;

  // Ensure FCookie is initialised (InitializeCookie is idempotent — it will
  // return immediately after creating an empty collection on CrossSocket path)
  if not Assigned(FCookie) then
    FCookie := THorseCoreParam.Create(THorseList.Create).Required(False);

  Pairs := ACookieHeader.Split([';']);
  for Pair in Pairs do
  begin
    EqPos := Pos('=', Pair);
    if EqPos < 2 then Continue;   // skip malformed / empty-name pairs
    CName  := Trim(Copy(Pair, 1, EqPos - 1));
    CValue := Trim(Copy(Pair, EqPos + 1, MaxInt));
    if CName = '' then Continue;
    FCookie.Dictionary.AddOrSetValue(CName, CValue);
  end;
end;
{ =========================================================================== }

end.
