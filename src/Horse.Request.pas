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
  Generics.Collections,
{$ELSE}
  System.SysUtils,
  Web.HTTPApp,
  System.Generics.Collections,
{$IF CompilerVersion > 32.0}
  Web.ReqMulti,
{$ENDIF}
{$ENDIF}
  Horse.Core.Param,
  Horse.Session,
  Horse.Commons,
  Horse.Core.Context;

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
    FOwnsBody: Boolean;
    FSession: TObject;
    FOwnsSession: Boolean;
    FSessions: THorseSessions;
    FArena: THorseArenaAllocator;
    FOwnsArena: Boolean;
    FServices: THorseRequestContext;
{ ===========================================================================
  PATCH-REQ-3  CrossSocket shadow fields (populated by Populate, nil by default)
  =========================================================================== }
    FCSMethod:      string;
    FCSMethodType:  TMethodType;
    FCSPathInfo:    string;
    FCSContentType: string;
    FCSRemoteAddr:  string;
{ PATCH-REQ-9  cached decoded body string for the CrossSocket path.
  Populated once at populate time by SetBodyString (called from
  TRequestBridge.MapBody); returned directly by Body: string so the stream
  is read and UTF-8-decoded exactly once per request, not on every call. }
    FBodyString:    string;
{ =========================================================================== }
{ ===========================================================================
  PATCH-REQ-8  Owned TWebRequest / TRequest adapter for middleware
  compatibility on the CrossSocket path.

  Problem solved:
    Existing middleware (e.g. Horse.CORS) reads Req.RawWebRequest.Method / .Host /
    .GetFieldByName(...). Before this patch, RawWebRequest returned the raw
    FWebRequest, which is nil on the CrossSocket path  every such call AV'd.

  Design:
    The CrossSocket provider's TRequestBridge.Populate constructs a concrete
    TCrossSocketWebRequest (subclass of TWebRequest on Delphi / TRequest on FPC)
    backed by ICrossHttpRequest, and hands it to SetCSRawWebRequest. THorseRequest
    OWNS the adapter: Clear and Destroy free it.

  Field stays nil on the Indy path  FWebRequest remains the authoritative source
  there (owned by the Indy provider, as before).

  See: Horse.Provider.CrossSocket.WebRequestAdapter.pas
  =========================================================================== }
    FCSRawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
{ =========================================================================== }
    FMatchedRoute: string;
    FState: TObjectDictionary<string, TObject>;
{ =========================================================================== }
    function GetArena: THorseArenaAllocator;
    procedure InitializeQuery;
    procedure InitializeParams;
    procedure InitializeContentFields;
    procedure InitializeCookie;
{ PATCH-COOKIE-2  RFC 6265 single-pair parser shared by InitializeCookie and
  PopulateCookiesFromHeader: split on the FIRST '=' only (values may contain
  '='), trim OWS, strip one layer of surrounding quotes. Adds to FCookie. }
    procedure AddCookiePair(const APair: string);
    function IsMultipartForm: Boolean;
    function IsFormURLEncoded: Boolean;
    function CanLoadContentFields: Boolean;
    function GetServices: THorseRequestContext;
  public
    function Body: string; overload; virtual;
    function Body<T: class>: T; overload;
    function Body(const ABody: TObject; AOwnsBody: Boolean = True): THorseRequest; overload; virtual;
    function Session<T: class>: T; overload;
    function Session(const ASession: TObject; AOwnsSession: Boolean = False): THorseRequest; overload; virtual;
    function Headers: THorseCoreParam; virtual;
    function Query: THorseCoreParam; virtual;
    function Params: THorseCoreParam; virtual;
    function Cookie: THorseCoreParam; virtual;
    function ContentFields: THorseCoreParam; virtual;
    function Sessions: THorseSessions; virtual;
    function MethodType: TMethodType; virtual;
{ ===========================================================================
  PATCH-REQ-10  Method: string accessor
  Convenience companion to MethodType. TMethodType collapses OPTIONS / TRACE /
  CONNECT into mtAny, so middleware that needs to discriminate by raw verb
  (e.g. OPTIONS preflight handling) cannot use the enum.

  Req.RawWebRequest.Method covers the same use case via the PATCH-REQ-8
  adapter, but this property avoids the indirection for the common case. The
  Indy path returns FWebRequest.Method (the existing TWebRequest/TRequest
  property); the CrossSocket path returns FCSMethod populated by Populate().
  =========================================================================== }
    function Method: string; virtual;
{ =========================================================================== }
    function ContentType: string; virtual;
    function Host: string; virtual;
    function IsWebSocket: Boolean; virtual;
    function WebSocketKey: string; virtual;
    property Arena: THorseArenaAllocator read GetArena write FArena;
    function PathInfo: string; virtual;
    function RawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}; virtual;
    constructor Create(const AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}); overload;
{ ===========================================================================
  PATCH-REQ-1  added parameterless constructor overload
  Reason: THorseContextPool.WarmUp pre-allocates THorseRequest instances at
  application startup, before any HTTP request arrives and before any
  TWebRequest exists. The pool calls this overload; the original constructor
  is completely unchanged and continues to be used by the Indy provider.
  =========================================================================== }
    constructor Create; overload;
{ =========================================================================== }
{ ===========================================================================
  PATCH-REQ-2  added Clear procedure
  Reason: THorseContext.Reset recycles pooled objects between requests
  without Free/Create overhead. Rules enforced:
     FBody        freed only when FOwnsBody=True (Jhonson-style owned objects);
                    set to nil when FOwnsBody=False (CrossSocket non-owning buffer ref)
     FBodyString  set to '' (cached decoded body; repopulated by MapBody)
     FSession     freed when FOwnsSession=True; set to nil otherwise
     FWebRequest  set to nil (belongs to previous Indy context)
     param collections  cleared in place, objects reused
  =========================================================================== }
    procedure Clear;
{ =========================================================================== }
{ ===========================================================================
  PATCH-REQ-3  added Populate procedure and RemoteAddr function
  Reason: The CrossSocket bridge must inject per-request values directly
  into THorseRequest without a live TWebRequest.  All fields below were
  previously read-only delegations to FWebRequest; they now have private
  shadow fields that are populated here and returned when FWebRequest is nil.

  Fields injected via Populate:
    FCSMethod       method string ('GET','POST',)
    FCSMethodType   parsed TMethodType
    FCSPathInfo     decoded path ('/api/users/1')
    FCSContentType  Content-Type header value
    FCSRemoteAddr   real peer socket address

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
    function GetPathSegments: TArray<THorseBufferSlice>;
{ ===========================================================================
  PATCH-REQ-5  RawPathInfo
  Returns the undecoded (percent-encoded) request path on the Delphi/Indy
  path  preserving the exact value THorseRouterTree.Execute previously read
  from ARequest.RawWebRequest.RawPathInfo.

  Three execution paths:
    CrossSocket  FWebRequest = nil - return FCSPathInfo (CrossSocket's own
                 URL parser supplies a decoded path; the router treats it
                 the same way it treated RawPathInfo on Indy).
    Delphi/Indy  return FWebRequest.RawPathInfo (undecoded, original behaviour).
    FPC/Indy     TRequest has no RawPathInfo - return FWebRequest.PathInfo
                 (unchanged from the pre-patch router behaviour on FPC).

  THorseRouterTree.Execute (PATCH-TREE-1) calls this instead of PathInfo so
  that percent-encoded URLs continue to route correctly on the Indy path.
  =========================================================================== }
    function RawPathInfo: string; virtual;
{ ===========================================================================
  PATCH-REQ-4  PopulateCookiesFromHeader
  Parses the raw "Cookie: name=value; name2=value2" header string into the
  FCookie param collection.  Called by the CrossSocket request bridge after
  Populate() so that Req.Cookie works on the CrossSocket path without any
  dependency on FWebRequest.
  =========================================================================== }
    procedure PopulateCookiesFromHeader(const ACookieHeader: string);
{ =========================================================================== }
{ ===========================================================================
  PATCH-REQ-8  setter for the owned RawWebRequest adapter. Called once per
  request by the CrossSocket bridge right after Populate / header / cookie
  population. Replaces any prior adapter instance (defence in depth 
  Clear normally nils it first).
  =========================================================================== }
    procedure SetCSRawWebRequest(const ARawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF});
{ =========================================================================== }
{ PATCH-REQ-9  called by TRequestBridge.MapBody to cache the decoded body. }
    procedure SetBodyString(const AValue: string);
{ =========================================================================== }
    property MatchedRoute: string read FMatchedRoute write FMatchedRoute;
    property State: TObjectDictionary<string, TObject> read FState;
    property Services: THorseRequestContext read GetServices;
    destructor Destroy; override;
  end;

implementation

uses      
{$IF DEFINED(FPC)}
  Classes,
  Generics.Defaults,
{$ELSE}
  System.Classes,
  System.Generics.Defaults,
{$ENDIF}
  Horse.Utils,
  Horse.Core.Param.Header;



{ ===========================================================================
  PATCH-REQ-5 / PATCH-REQ-9  Body: string
  On the CrossSocket path FWebRequest is nil.  PATCH-REQ-9 pre-populates
  FBodyString once at populate time (TRequestBridge.MapBody - SetBodyString)
  so this accessor is O(1) regardless of body size and is safe to call
  multiple times.  Binary/non-text bodies are not decoded here  callers
  that need the raw bytes should use Body<TStream>.
  =========================================================================== }
function THorseRequest.Body: string;
begin
  if not Assigned(FWebRequest) then
  begin
    Result := FBodyString;
    Exit;
  end;
  Result := FWebRequest.Content;
end;

{ PATCH-REQ-11  AOwnsBody controls whether Clear frees FBody on pool recycle.
  Default True preserves upstream ownership semantics (Jhonson, etc.).
  CrossSocket's MapBody passes False  the body is a non-owning reference
  into CrossSocket's receive buffer that must never be freed by Horse. }
function THorseRequest.Body(const ABody: TObject; AOwnsBody: Boolean = True): THorseRequest;
begin
  Result := Self;
  if FOwnsBody and Assigned(FBody) then
    FreeAndNil(FBody);
  FBody     := ABody;
  FOwnsBody := AOwnsBody;
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

function THorseRequest.GetServices: THorseRequestContext;
begin
  if not Assigned(FServices) then
    FServices := THorseRequestContext.Create;
  Result := FServices;
end;

constructor THorseRequest.Create(const AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF});
begin
  FWebRequest := AWebRequest;
  FState := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
end;

{ ===========================================================================
  PATCH-REQ-1  parameterless constructor implementation
  =========================================================================== }
constructor THorseRequest.Create;
begin
  FWebRequest := nil;
  FState := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
end;
{ =========================================================================== }

{ ===========================================================================
  PATCH-REQ-2  Clear implementation
  THorseCoreParam owns FParams (a TDictionary<string,string>) which is
  exposed via the public Dictionary property. Calling Dictionary.Clear
  wipes all entries in-place without freeing the THorseCoreParam object
  itself, so the next request reuses the same objects with no heap churn.
  FContent is a lazy TStrings cache inside THorseCoreParam  it is freed
  and nil-ed here via FreeAndNil on the param object then recreated by the
  next InitializeXxx call. Because FContent is private to THorseCoreParam
  the cleanest way to reset it is to FreeAndNil the whole THorseCoreParam
  and let the lazy accessor rebuild it, which is what the destructor does.
  We therefore use Dictionary.Clear for the hot-path wipe and rely on the
  lazy InitializeXxx pattern (already used everywhere in this class) when
  a full reset including FContent is required.
  Strategy per field:
    FHeaders       Dictionary.Clear  (header map reused, no FContent used)
    FQuery         FreeAndNil + lazy rebuild via InitializeQuery
    FParams        Dictionary.Clear  (route params repopulated by router)
    FContentFields FreeAndNil + lazy rebuild via InitializeContentFields
    FCookie        FreeAndNil + lazy rebuild via InitializeCookie
    FSessions      Clear in-place (PATCH-SES-1); no allocation on hot path
  =========================================================================== }
procedure THorseRequest.Clear;
begin
  FWebRequest := nil;
  { PATCH-REQ-11  respect body ownership.
    FOwnsBody=True  - owned object (e.g. Jhonson JSON)  free it.
    FOwnsBody=False - non-owning ref (CrossSocket buffer)  just nil. }
  if FOwnsBody and Assigned(FBody) then
    FreeAndNil(FBody)
  else
    FBody := nil;
  FOwnsBody := False;

  FBodyString := '';    { PATCH-REQ-9 }

  { PATCH-REQ-10 - libera sesso owned antes de reutilizar o request.
    No provider mORMot o THorseRequest  reaproveitado pelo pool; apenas
    zerar FSession deixa o objeto anterior sem dono e causa vazamento. }
  if FOwnsSession and Assigned(FSession) then
    FreeAndNil(FSession)
  else
    FSession := nil;
  FOwnsSession := False;

{ PATCH-REQ-3  wipe shadow fields so next request starts clean }
  FCSMethod      := '';
  FCSMethodType  := mtAny;
  FCSPathInfo    := '';
  FCSContentType := '';
  FCSRemoteAddr  := '';
{ end PATCH-REQ-3 }
{ PATCH-REQ-8  free the per-request TWebRequest adapter (owned).
  Nil on the Indy path (never assigned there); owned on CrossSocket path. }
  if Assigned(FCSRawWebRequest) then
    FreeAndNil(FCSRawWebRequest);
{ end PATCH-REQ-8 }
  if Assigned(FArena) then
    FArena.Reset;
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
{ PATCH-SES-1  reuse the existing THorseSessions object across pool recycles.
  THorseSessions.Clear calls TObjectDictionary.Clear which frees owned TSession
  values before emptying the map  no allocation on the hot path. }
  if Assigned(FSessions) then
    FSessions.Clear;
{ end PATCH-SES-1 }
  FMatchedRoute := '';
  if Assigned(FState) then
    FState.Clear;
  if Assigned(FServices) then
    FreeAndNil(FServices);
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
  if FOwnsBody and Assigned(FBody) then
    FBody.Free;
  if FOwnsSession and Assigned(FSession) then
    FreeAndNil(FSession);
  if Assigned(FSessions) then
    FSessions.Free;
{ PATCH-REQ-8 � free the owned TWebRequest adapter if Clear was not called
  before Destroy (e.g. pool shutdown path). Safe because FCSRawWebRequest is
  only ever populated on the CrossSocket path and is owned by THorseRequest. }
  if Assigned(FCSRawWebRequest) then
    FCSRawWebRequest.Free;
{ end PATCH-REQ-8 }
  if FOwnsArena and Assigned(FArena) then
    FreeAndNil(FArena);
  if Assigned(FServices) then
    FreeAndNil(FServices);
  if Assigned(FState) then
    FreeAndNil(FState);
  inherited;
end;

function THorseRequest.Headers: THorseCoreParam;
var
  LParam: THorseList;
begin
  if not Assigned(FHeaders) then
  begin
{ PATCH-REQ-3 � nil-guard: when FWebRequest is nil (CrossSocket path),
  Populate already called InitializeHeaders which set FHeaders.
  If somehow we arrive here with FWebRequest=nil and FHeaders=nil,
  create an empty param rather than crashing on GetHeaders(nil). }
    if not Assigned(FWebRequest) then
    begin
      FHeaders := THorseCoreParam.Create(THorseList.Create({$IFDEF FPC}THorseHeaderComparer.Create{$ELSE}TIStringComparer.Ordinal{$ENDIF})).Required(False);
      Exit(FHeaders);
    end;
{ end PATCH-REQ-3 }
    LParam := THorseCoreParamHeader.GetHeaders(FWebRequest);
    FHeaders := THorseCoreParam.Create(LParam).Required(False);
  end;
  Result := FHeaders;
end;

{ ===========================================================================
  PATCH-REQ-6 � Host nil-guard
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
{ PATCH-REQ-3 � nil-guard }
  if not Assigned(FWebRequest) then
    Exit(FCSContentType);
{ end PATCH-REQ-3 }
  Result := FWebRequest.ContentType;
end;

function THorseRequest.PathInfo: string;
var
  LPrefix: string;
begin
{ PATCH-REQ-3 � nil-guard }
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
{ PATCH-REQ-4 � nil-guard: on CrossSocket path FWebRequest is nil.
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
var
  LItem: string;
begin
  FCookie := THorseCoreParam.Create(THorseList.Create).Required(False);
{ PATCH-REQ-4 � nil-guard: on CrossSocket path FWebRequest is nil.
  Cookie parsing from the raw header string is handled by
  THorseRequest.PopulateCookiesFromHeader, called by the CrossSocket bridge
  after Populate().  Nothing to do here on that path. }
  if not Assigned(FWebRequest) then
    Exit;
{ end PATCH-REQ-4 }
{ PATCH-COOKIE-2 � each CookieFields entry is one "name=value"; parse with the
  shared RFC 6265 helper. Replaces the old Split(['=']) + [0]/[1] which
  truncated values containing '=' (base64/JWT) and could index out of bounds. }
  for LItem in FWebRequest.CookieFields do
    AddCookiePair(LItem);
end;

{ PATCH-COOKIE-2 � shared single-pair cookie parser. }
procedure THorseRequest.AddCookiePair(const APair: string);
var
  EqPos: Integer;
  CName, CValue: string;
begin
  if not Assigned(FCookie) then
    FCookie := THorseCoreParam.Create(THorseList.Create).Required(False);
  EqPos := Pos('=', APair);
  if EqPos < 2 then
    Exit;                                   // no/empty name � skip
  CName  := Trim(Copy(APair, 1, EqPos - 1));
  CValue := Trim(Copy(APair, EqPos + 1, MaxInt));
  if CName = '' then
    Exit;
  // strip one layer of surrounding DQUOTEs from a quoted value (RFC 6265 �4.1.1)
  if (Length(CValue) >= 2) and (CValue[1] = '"') and
     (CValue[Length(CValue)] = '"') then
    CValue := Copy(CValue, 2, Length(CValue) - 2);
  FCookie.Dictionary.AddOrSetValue(CName, CValue);
end;

procedure THorseRequest.InitializeParams;
begin
  FParams := THorseCoreParam.Create(THorseList.Create).Required(True);
end;

{ ===========================================================================
  PATCH-REQ-7 � InitializeQuery nil-guard
  On the CrossSocket path FWebRequest is nil.  Query parameters are
  pre-populated by the CrossSocket request bridge directly via
  AHorseReq.Query.Dictionary.AddOrSetValue, which triggers this method
  as the lazy initialiser (FQuery is nil on first call).  We create the
  empty param collection and return early � the bridge then populates it.
  Accessing FWebRequest.QueryFields on the CrossSocket path would crash.
  =========================================================================== }
procedure THorseRequest.InitializeQuery;
var
  LQuery: string;
  LStart, LLen, I, LEqPos: Integer;
  LKey, LValue: string;
begin
  FQuery := THorseCoreParam.Create(THorseList.Create).Required(False);
  if not Assigned(FWebRequest) then
    Exit;  // CrossSocket path: bridge populates query dict directly
  
  LQuery := FWebRequest.Query;
  if LQuery = '' then
    Exit;

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
      LKey := Copy(LQuery, LStart, LEqPos - LStart);
      LValue := Copy(LQuery, LEqPos + 1, I - LEqPos - 1);
      
      LKey := DecodeParam(LKey);
      LValue := DecodeParam(LValue);
        
      if not FQuery.Dictionary.ContainsKey(LKey) then
        FQuery.Dictionary.AddOrSetValue(LKey, LValue)
      else
        FQuery.Dictionary[LKey] := FQuery.Dictionary[LKey] + ',' + LValue;
    end
    else
    begin
      LKey := Copy(LQuery, LStart, I - LStart);
      LKey := DecodeParam(LKey);
      if not FQuery.Dictionary.ContainsKey(LKey) then
        FQuery.Dictionary.AddOrSetValue(LKey, '');
    end;

    LStart := I + 1;
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
{ PATCH-REQ-3 }
  if not Assigned(FWebRequest) then
    Exit(FCSMethodType);
  Result := TMethodType.FromString(FWebRequest.Method);
end;

{ PATCH-REQ-10  Method: string }
function THorseRequest.Method: string;
begin
  if not Assigned(FWebRequest) then
    Exit(FCSMethod);
  Result := FWebRequest.Method;
end;
{ end PATCH-REQ-10 }

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
{ PATCH-REQ-8 � return the CrossSocket-path adapter when the Indy-path
  FWebRequest is nil, so existing middleware that calls
    Req.RawWebRequest.Method / .Host / .GetFieldByName(...)
  works unchanged on the CrossSocket path. See
  Horse.Provider.CrossSocket.WebRequestAdapter. }
  if Assigned(FWebRequest) then
    Exit(FWebRequest);
  Result := FCSRawWebRequest;
{ end PATCH-REQ-8 }
end;

function THorseRequest.Session(const ASession: TObject; AOwnsSession: Boolean): THorseRequest;
begin
  Result := Self;

  { PATCH-REQ-10 - respeita a propriedade da sess�o anterior.
    Se a sess�o anterior n�o era owned, n�o deve ser liberada aqui; se era
    owned, precisa ser liberada antes de substituir a refer�ncia. }
  if FOwnsSession and Assigned(FSession) then
    FreeAndNil(FSession)
  else
    FSession := nil;

  FSession := ASession;
  FOwnsSession := AOwnsSession;
end;

function THorseRequest.Session<T>: T;
begin
  Result := T(FSession);
end;

function THorseRequest.Sessions: THorseSessions;
begin
  if not Assigned(FSessions) then
    FSessions := THorseSessions.Create;
  Result := FSessions;
end;

{ ===========================================================================
  PATCH-REQ-3 � Populate implementation
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
    FHeaders := THorseCoreParam.Create(THorseList.Create({$IFDEF FPC}THorseHeaderComparer.Create{$ELSE}TIStringComparer.Ordinal{$ENDIF})).Required(False)
  else
    FHeaders.Clear;
end;

function THorseRequest.RemoteAddr: string;
begin
  Result := FCSRemoteAddr;
end;

{ ===========================================================================
  PATCH-REQ-5 � RawPathInfo implementation
  =========================================================================== }
function THorseRequest.RawPathInfo: string;
begin
  if not Assigned(FWebRequest) then
    Exit(FCSPathInfo);
{$IF DEFINED(FPC)}
  Result := FWebRequest.PathInfo;
{$ELSE}
  Result := FWebRequest.RawPathInfo;
{$ENDIF}
end;

function THorseRequest.GetArena: THorseArenaAllocator;
begin
  if not Assigned(FArena) then
  begin
    FArena := THorseArenaAllocator.Create(65536);
    FOwnsArena := True;
  end;
  Result := FArena;
end;

function THorseRequest.GetPathSegments: TArray<THorseBufferSlice>;
var
  LPath: string;
  LPathLen: Integer;
  LByteCount: Integer;
  LBytes: TBytes;
  LTempBytes: TBytes;
  LSlice: THorseBufferSlice;
  LStartOffset: Integer;
  I, LLen: Integer;
  LStart, LCount: Integer;
begin
  LPath := RawPathInfo;
  LPathLen := Length(LPath);
  if LPathLen = 0 then
    Exit(nil);

  if FArena = nil then
  begin
    FArena := THorseArenaAllocator.Create(64 * 1024);
    FOwnsArena := True;
  end;

{$IF DEFINED(FPC)}
  LByteCount := Length(LPath);
  LSlice := FArena.Allocate(LByteCount);
  LBytes := LSlice.Buffer;
  LStartOffset := LSlice.Start;
  if LByteCount > 0 then
    Move(Pointer(LPath)^, LBytes[LStartOffset], LByteCount);
{$ELSE}
  LTempBytes := TEncoding.UTF8.GetBytes(LPath);
  LByteCount := Length(LTempBytes);
  LSlice := FArena.Allocate(LByteCount);
  LBytes := LSlice.Buffer;
  LStartOffset := LSlice.Start;
  if LByteCount > 0 then
    Move(LTempBytes[0], LBytes[LStartOffset], LByteCount);
{$ENDIF}

  SetLength(Result, 0);
  LStart := LStartOffset;
  LCount := 0;

  I := LStartOffset;
  while I < LStartOffset + LByteCount do
  begin
    if LBytes[I] = 47 then // '/'
    begin
      LLen := I - LStart;
      if (LLen > 0) or (LStart = LStartOffset) then
      begin
        Inc(LCount);
        SetLength(Result, LCount);
        Result[LCount - 1] := THorseBufferSlice.Create(LBytes, LStart, LLen);
      end;
      LStart := I + 1;
    end;
    Inc(I);
  end;

  LLen := (LStartOffset + LByteCount) - LStart;
  if LLen > 0 then
  begin
    Inc(LCount);
    SetLength(Result, LCount);
    Result[LCount - 1] := THorseBufferSlice.Create(LBytes, LStart, LLen);
  end;
end;
{ =========================================================================== }

{ ===========================================================================
  PATCH-REQ-4 � PopulateCookiesFromHeader implementation
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
  LStart, LLen, I: Integer;
  LPair: string;
begin
  if ACookieHeader = '' then
    Exit;

  if not Assigned(FCookie) then
    FCookie := THorseCoreParam.Create(THorseList.Create).Required(False);

  LStart := 1;
  LLen := Length(ACookieHeader);
  while LStart <= LLen do
  begin
    I := LStart;
    while (I <= LLen) and (ACookieHeader[I] <> ';') do
      Inc(I);

    LPair := Copy(ACookieHeader, LStart, I - LStart);
    if LPair <> '' then
      AddCookiePair(LPair);

    LStart := I + 1;
  end;
end;
{ =========================================================================== }

{ ===========================================================================
  PATCH-REQ-8 � SetCSRawWebRequest implementation
  Called once per request by TRequestBridge.Populate. Replaces any existing
  adapter (defensive; the normal path is Clear -> nil -> Populate -> set).
  =========================================================================== }
procedure THorseRequest.SetCSRawWebRequest(
  const ARawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF});
begin
  if Assigned(FCSRawWebRequest) and (FCSRawWebRequest <> ARawWebRequest) then
    FreeAndNil(FCSRawWebRequest);
  FCSRawWebRequest := ARawWebRequest;
  FWebRequest := ARawWebRequest;
end;
{ =========================================================================== }

{ ===========================================================================
  PATCH-REQ-9 � SetBodyString implementation
  Called once per request by TRequestBridge.MapBody after reading and
  UTF-8-decoding the binary receive buffer.  Body: string returns this
  cached value directly � the stream is never re-read by the accessor.
  =========================================================================== }
procedure THorseRequest.SetBodyString(const AValue: string);
begin
  FBodyString := AValue;
end;
{ =========================================================================== }

function THorseRequest.IsWebSocket: Boolean;
begin
  Result := Headers.ContainsKey('upgrade') and (Pos('websocket', LowerCase(Headers['upgrade'])) > 0);
end;

function THorseRequest.WebSocketKey: string;
begin
  Result := '';
  if Headers.ContainsKey('sec-websocket-key') then
    Result := Headers['sec-websocket-key'];
end;
{ =========================================================================== }

end.
