unit Horse.Provider.RawAdapters;

{
  Horse Provider Raw Adapters
  ===========================
  Generic TWebRequest / TWebResponse subclasses that delegate to
  IHorseRawRequest / IHorseRawResponse interfaces.

  Purpose: new providers implement the lightweight interfaces (~10 methods)
  from Horse.Provider.RawInterfaces and wrap them in these adapters to get
  full TWebRequest / TWebResponse compatibility for free.

  Delphi branch: subclasses TWebRequest / TWebResponse (Web.HTTPApp).
  FPC branch:    subclasses TRequest / TResponse (HTTPDefs).

  The adapters handle:
  - All abstract Get/Set variable method stubs
  - Eager population of QueryFields / ContentFields / CookieFields
  - GetFieldByName delegation
  - ReadClient / ReadString delegation
  - Write-side stubs (no-ops — responses go through THorseResponse)

  SetCustomHeader on TInterfacedWebResponse is inherited from TWebResponse
  and writes to the inherited CustomHeaders TStrings. The adapter optionally
  forwards to IHorseRawResponse if the provider wants real-time interception.
}

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Classes,
  fpHTTP,
  HTTPDefs,
{$ELSE}
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
{$ENDIF}
  Horse.Provider.RawInterfaces;

type
{$IF DEFINED(FPC)}

  { TInterfacedWebRequest — FPC TRequest subclass
    Populates inherited fields from IHorseRawRequest in constructor. }
  TInterfacedWebRequest = class(TRequest)
  private
    FRawReq: IHorseRawRequest;
  public
    constructor Create(const ARawReq: IHorseRawRequest); reintroduce;
    property RawReq: IHorseRawRequest read FRawReq;
  end;

  { TInterfacedWebResponse — FPC TResponse subclass }
  TInterfacedWebResponse = class(TResponse)
  private
    FRawRes: IHorseRawResponse;
  public
    constructor Create(const ARawRes: IHorseRawResponse); reintroduce;
    property RawRes: IHorseRawResponse read FRawRes;
  end;

{$ELSE}

  { TInterfacedWebRequest — Delphi TWebRequest subclass
    Delegates all abstract methods to IHorseRawRequest.
    Eagerly populates inherited QueryFields / ContentFields / CookieFields. }
  TInterfacedWebRequest = class(TWebRequest)
  private
    FRawReq: IHorseRawRequest;
  protected
    function  GetStringVariable(Index: Integer): string; override;
    function  GetDateVariable(Index: Integer): TDateTime; override;
{$IF CompilerVersion >= 32.0}  // Delphi 10.2 Tokyo+
    function  GetIntegerVariable(Index: Integer): Int64; override;
{$ELSE}
    function  GetIntegerVariable(Index: Integer): Integer; override;
{$IFEND}
    function  GetRawContent: TBytes; override;
  public
    constructor Create(const ARawReq: IHorseRawRequest); reintroduce;
    destructor  Destroy; override;
    function  GetFieldByName(const Name: string): string; override;
    function  ReadClient(var Buffer; Count: Integer): Integer; override;
    function  ReadString(Count: Integer): string; override;
    function  TranslateURI(const URI: string): string; override;
    function  WriteClient(var Buffer; Count: Integer): Integer; override;
    function  WriteString(const AString: string): Boolean; override;
    function  WriteHeaders(StatusCode: Integer; const ReasonString,
                           Headers: string): Boolean; override;
    property  RawReq: IHorseRawRequest read FRawReq;
  end;

  { TInterfacedWebResponse — Delphi TWebResponse subclass
    All abstract methods stubbed. SetCustomHeader is inherited and works
    out of the box via the inherited CustomHeaders TStrings. }
  TInterfacedWebResponse = class(TWebResponse)
  private
    FRawRes: IHorseRawResponse;
  protected
    function  GetStringVariable(Index: Integer): string; override;
    procedure SetStringVariable(Index: Integer; const Value: string); override;
    function  GetDateVariable(Index: Integer): TDateTime; override;
    procedure SetDateVariable(Index: Integer; const Value: TDateTime); override;
{$IF CompilerVersion >= 32.0}  // Delphi 10.2 Tokyo+
    function  GetIntegerVariable(Index: Integer): Int64; override;
    procedure SetIntegerVariable(Index: Integer; Value: Int64); override;
{$ELSE}
    function  GetIntegerVariable(Index: Integer): Integer; override;
    procedure SetIntegerVariable(Index: Integer; Value: Integer); override;
{$IFEND}
    function  GetContent: string; override;
    procedure SetContent(const Value: string); override;
    procedure SetContentStream(Value: TStream); override;
    function  GetStatusCode: Integer; override;
    procedure SetStatusCode(Value: Integer); override;
    function  GetLogMessage: string; override;
    procedure SetLogMessage(const Value: string); override;
  public
    constructor Create(const ARawRes: IHorseRawResponse); reintroduce;
    destructor  Destroy; override;
    procedure SendResponse; override;
    procedure SendRedirect(const URI: string); override;
    property  RawRes: IHorseRawResponse read FRawRes;
  end;

{$ENDIF}

implementation

{ --------------------------------------------------------------------------- }
{ String variable indices — same as Web.HTTPApp TWebRequest private constants  }
{ Stable across Delphi versions from XE onward.                               }
{ --------------------------------------------------------------------------- }
{$IF NOT DEFINED(FPC)}
const
  HRV_Method           = 0;
  HRV_ProtocolVersion  = 1;
  HRV_URL              = 2;
  HRV_Query            = 3;
  HRV_PathInfo         = 4;
  HRV_PathTranslated   = 5;
  HRV_CacheControl     = 6;
  HRV_Date             = 7;
  HRV_Accept           = 8;
  HRV_From             = 9;
  HRV_Host             = 10;
  HRV_IfModifiedSince  = 11;
  HRV_Referer          = 12;
  HRV_UserAgent        = 13;
  HRV_ContentEncoding  = 14;
  HRV_ContentType      = 15;
  HRV_ContentLength    = 16;
  HRV_ContentVersion   = 17;
  HRV_DerivedFrom      = 18;
  HRV_Expires          = 19;
  HRV_Title            = 20;
  HRV_RemoteAddr       = 21;
  HRV_RemoteHost       = 22;
  HRV_ScriptName       = 23;
  HRV_ServerPort       = 24;
  HRV_Content          = 25;
  HRV_Connection       = 26;
  HRV_Cookie           = 27;
  HRV_Authorization    = 28;
{$ENDIF}

{ =========================================================================== }
{ Delphi implementation                                                       }
{ =========================================================================== }
{$IF NOT DEFINED(FPC)}

{ --------------------------------------------------------------------------- }
{ TInterfacedWebRequest                                                       }
{ --------------------------------------------------------------------------- }

constructor TInterfacedWebRequest.Create(const ARawReq: IHorseRawRequest);
begin
  { Assign FRawReq BEFORE inherited Create — TWebRequest.Create calls
    GetStringVariable internally during initialisation. }
  FRawReq := ARawReq;
  inherited Create;

  { Populate the inherited TStrings (backed by private fields — no virtual
    getter to override). }
  if Assigned(FRawReq) then
  begin
    FRawReq.PopulateQueryFields(QueryFields);
    FRawReq.PopulateContentFields(ContentFields);
    FRawReq.PopulateCookieFields(CookieFields);
  end;
end;

destructor TInterfacedWebRequest.Destroy;
begin
  FRawReq := nil;
  inherited;
end;

function TInterfacedWebRequest.GetStringVariable(Index: Integer): string;
begin
  if FRawReq = nil then Exit('');
  case Index of
    HRV_Method:           Result := FRawReq.GetMethod;
    HRV_ProtocolVersion:  Result := FRawReq.GetProtocolVersion;
    HRV_URL:              Result := FRawReq.GetURL;
    HRV_Query:            Result := FRawReq.GetQueryString;
    HRV_PathInfo:         Result := FRawReq.GetPathInfo;
    HRV_PathTranslated:   Result := FRawReq.GetPathInfo;
    HRV_CacheControl:     Result := FRawReq.GetFieldByName('Cache-Control');
    HRV_Date:             Result := FRawReq.GetFieldByName('Date');
    HRV_Accept:           Result := FRawReq.GetFieldByName('Accept');
    HRV_From:             Result := FRawReq.GetFieldByName('From');
    HRV_Host:             Result := FRawReq.GetHost;
    HRV_IfModifiedSince:  Result := FRawReq.GetFieldByName('If-Modified-Since');
    HRV_Referer:          Result := FRawReq.GetFieldByName('Referer');
    HRV_UserAgent:        Result := FRawReq.GetFieldByName('User-Agent');
    HRV_ContentEncoding:  Result := FRawReq.GetFieldByName('Content-Encoding');
    HRV_ContentType:      Result := FRawReq.GetContentType;
    HRV_ContentLength:    Result := IntToStr(FRawReq.GetContentLength);
    HRV_ContentVersion:   Result := FRawReq.GetFieldByName('Content-Version');
    HRV_DerivedFrom:      Result := FRawReq.GetFieldByName('Derived-From');
    HRV_Expires:          Result := FRawReq.GetFieldByName('Expires');
    HRV_Title:            Result := FRawReq.GetFieldByName('Title');
    HRV_RemoteAddr:       Result := FRawReq.GetRemoteAddr;
    HRV_RemoteHost:       Result := FRawReq.GetRemoteAddr;
    HRV_ScriptName:       Result := '';
    HRV_ServerPort:       Result := IntToStr(FRawReq.GetServerPort);
    HRV_Content:          Result := FRawReq.GetContent;
    HRV_Connection:       Result := FRawReq.GetFieldByName('Connection');
    HRV_Cookie:           Result := FRawReq.GetFieldByName('Cookie');
    HRV_Authorization:    Result := FRawReq.GetFieldByName('Authorization');
  else
    Result := '';
  end;
end;

function TInterfacedWebRequest.GetDateVariable(Index: Integer): TDateTime;
begin
  Result := 0;
end;

{$IF CompilerVersion >= 32.0}
function TInterfacedWebRequest.GetIntegerVariable(Index: Integer): Int64;
{$ELSE}
function TInterfacedWebRequest.GetIntegerVariable(Index: Integer): Integer;
{$IFEND}
begin
  if FRawReq = nil then Exit(-1);
  case Index of
    HRV_ContentLength:  Result := FRawReq.GetContentLength;
    HRV_ServerPort:     Result := FRawReq.GetServerPort;
  else
    Result := -1;
  end;
end;

function TInterfacedWebRequest.GetRawContent: TBytes;
var
  LContent: string;
begin
  if Assigned(FRawReq) then
  begin
    LContent := FRawReq.GetContent;
    if LContent <> '' then
      Result := TEncoding.UTF8.GetBytes(LContent)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TInterfacedWebRequest.GetFieldByName(const Name: string): string;
begin
  if Assigned(FRawReq) then
    Result := FRawReq.GetFieldByName(Name)
  else
    Result := '';
end;

function TInterfacedWebRequest.ReadClient(var Buffer; Count: Integer): Integer;
begin
  if Assigned(FRawReq) then
    Result := FRawReq.ReadBody(Buffer, Count)
  else
    Result := 0;
end;

function TInterfacedWebRequest.ReadString(Count: Integer): string;
var
  LBytes: TBytes;
  LRead:  Integer;
begin
  if Count <= 0 then Exit('');
  SetLength(LBytes, Count);
  LRead := ReadClient(LBytes[0], Count);
  if LRead <= 0 then Exit('');
  SetLength(LBytes, LRead);
  Result := TEncoding.UTF8.GetString(LBytes);
end;

function TInterfacedWebRequest.TranslateURI(const URI: string): string;
begin
  Result := URI;
end;

function TInterfacedWebRequest.WriteClient(var Buffer; Count: Integer): Integer;
begin
  Result := 0;
end;

function TInterfacedWebRequest.WriteString(const AString: string): Boolean;
begin
  Result := False;
end;

function TInterfacedWebRequest.WriteHeaders(StatusCode: Integer;
  const ReasonString, Headers: string): Boolean;
begin
  Result := False;
end;

{ --------------------------------------------------------------------------- }
{ TInterfacedWebResponse                                                      }
{ --------------------------------------------------------------------------- }

constructor TInterfacedWebResponse.Create(const ARawRes: IHorseRawResponse);
begin
  FRawRes := ARawRes;
  inherited Create(nil);
end;

destructor TInterfacedWebResponse.Destroy;
begin
  FRawRes := nil;
  inherited;
end;

function TInterfacedWebResponse.GetStringVariable(Index: Integer): string;
begin
  Result := '';
end;

procedure TInterfacedWebResponse.SetStringVariable(Index: Integer; const Value: string);
begin
  { Stub }
end;

function TInterfacedWebResponse.GetDateVariable(Index: Integer): TDateTime;
begin
  Result := 0;
end;

procedure TInterfacedWebResponse.SetDateVariable(Index: Integer; const Value: TDateTime);
begin
  { Stub }
end;

{$IF CompilerVersion >= 32.0}
function TInterfacedWebResponse.GetIntegerVariable(Index: Integer): Int64;
{$ELSE}
function TInterfacedWebResponse.GetIntegerVariable(Index: Integer): Integer;
{$IFEND}
begin
  Result := 0;
end;

{$IF CompilerVersion >= 32.0}
procedure TInterfacedWebResponse.SetIntegerVariable(Index: Integer; Value: Int64);
{$ELSE}
procedure TInterfacedWebResponse.SetIntegerVariable(Index: Integer; Value: Integer);
{$IFEND}
begin
  { Stub }
end;

function TInterfacedWebResponse.GetContent: string;
begin
  Result := '';
end;

procedure TInterfacedWebResponse.SetContent(const Value: string);
begin
  { Stub — use THorseResponse.Send }
end;

procedure TInterfacedWebResponse.SetContentStream(Value: TStream);
begin
  { Stub — use THorseResponse.SendFile }
end;

function TInterfacedWebResponse.GetStatusCode: Integer;
begin
  Result := 200;
end;

procedure TInterfacedWebResponse.SetStatusCode(Value: Integer);
begin
  { Stub — use THorseResponse.Status }
end;

function TInterfacedWebResponse.GetLogMessage: string;
begin
  Result := '';
end;

procedure TInterfacedWebResponse.SetLogMessage(const Value: string);
begin
  { Stub }
end;

procedure TInterfacedWebResponse.SendResponse;
begin
  { No-op — response sending goes through TResponseBridge.Flush }
end;

procedure TInterfacedWebResponse.SendRedirect(const URI: string);
begin
  { No-op — use THorseResponse.RedirectTo }
end;

{$ELSE}

{ =========================================================================== }
{ FPC implementation                                                          }
{ =========================================================================== }

{ --------------------------------------------------------------------------- }
{ TInterfacedWebRequest — FPC                                                 }
{ --------------------------------------------------------------------------- }

constructor TInterfacedWebRequest.Create(const ARawReq: IHorseRawRequest);
begin
  FRawReq := ARawReq;
  inherited Create;

  if Assigned(FRawReq) then
  begin
    Method          := FRawReq.GetMethod;
    URL             := FRawReq.GetURL;
    URI             := FRawReq.GetURL;
    PathInfo        := FRawReq.GetPathInfo;
    Host            := FRawReq.GetHost;
    ContentType     := FRawReq.GetContentType;
    RemoteAddr      := FRawReq.GetRemoteAddr;
    RemoteHost      := FRawReq.GetRemoteAddr;
    ProtocolVersion := FRawReq.GetProtocolVersion;
    ServerPort      := IntToStr(FRawReq.GetServerPort);

    FRawReq.PopulateQueryFields(QueryFields);
    FRawReq.PopulateContentFields(ContentFields);
    FRawReq.PopulateCookieFields(CookieFields);
  end;
end;

{ --------------------------------------------------------------------------- }
{ TInterfacedWebResponse — FPC                                                }
{ --------------------------------------------------------------------------- }

constructor TInterfacedWebResponse.Create(const ARawRes: IHorseRawResponse);
begin
  FRawRes := ARawRes;
  inherited Create(nil);
  Code := 200;
  ContentType := '';
end;

{$ENDIF}

end.
