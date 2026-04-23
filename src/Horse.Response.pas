unit Horse.Response;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Classes,
  fpHTTP,
  HTTPDefs,
{$ELSE}
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
  Horse.Commons;

type
  THorseResponse = class
  private
    FWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
    FContent: TObject;
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
    FCSContentType:   string;
    FCSContentStream: TStream;   // non-owning — caller retains ownership
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
  public
    function Send(const AContent: string): THorseResponse; overload; virtual;
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
    function Content: TObject; overload; virtual;
    function Content(const AContent: TObject): THorseResponse; overload; virtual;
    function ContentType(const AContentType: string): THorseResponse; virtual;
    function RawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF}; virtual;
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
{ =========================================================================== }
    destructor Destroy; override;
  end;

implementation

uses        
{$IF DEFINED(FPC)}
  SysUtils,
{$ELSE}
  System.SysUtils,
{$ENDIF}
  Horse.Core.Files,
  Horse.Mime;

function THorseResponse.AddHeader(const AName, AValue: string): THorseResponse;
begin
{ PATCH-RES-4 — nil-guard: skip FWebResponse on CrossSocket path }
  if Assigned(FWebResponse) then
    FWebResponse.SetCustomHeader(AName, AValue);
{ end PATCH-RES-4 }
{ ===========================================================================
  PATCH-RES-1 — also populate FCustomHeaders so CrossSocket bridge can read it.
  Delphi: TDictionary.AddOrSetValue  FPC: TStringList.Values[name] := value
  =========================================================================== }
{$IF NOT DEFINED(FPC)}
  FCustomHeaders.AddOrSetValue(AName, AValue);
{$ELSE}
  FCustomHeaders.Values[AName] := AValue;
{$ENDIF}
{ =========================================================================== }
  Result := Self;
end;

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
  PATCH-RES-1 — initialise FCustomHeaders
  Delphi: TDictionary<string,string>  FPC: TStringList
  =========================================================================== }
{$IF NOT DEFINED(FPC)}
  FCustomHeaders := TDictionary<string, string>.Create;
{$ELSE}
  FCustomHeaders := TStringList.Create;
{$ENDIF}
{ =========================================================================== }
end;

{ ===========================================================================
  PATCH-RES-2 — Clear implementation
  =========================================================================== }
procedure THorseResponse.Clear;
begin
  FWebResponse := nil;
  FContent := nil;
  if Assigned(FCustomHeaders) then
    FCustomHeaders.Clear;
{ PATCH-RES-4 — wipe CrossSocket shadow fields }
  FCSBody          := '';
  FCSContentType   := '';
  FCSContentStream := nil;   // non-owning — never free here
  FCSStatusCode    := 200;
{ end PATCH-RES-4 }
{ PATCH-RES-6 — free the per-request TWebResponse adapter (owned).
  Nil on the Indy path (never assigned there); owned on CrossSocket path. }
  if Assigned(FCSRawWebResponse) then
    FreeAndNil(FCSRawWebResponse);
{ end PATCH-RES-6 }
end;
{ =========================================================================== }

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
{ =========================================================================== }

destructor THorseResponse.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
{ ===========================================================================
  PATCH-RES-1 — free FCustomHeaders
  =========================================================================== }
  if Assigned(FCustomHeaders) then
    FCustomHeaders.Free;
{ =========================================================================== }
{ PATCH-RES-6 — free the owned TWebResponse adapter if Clear was not called
  before Destroy (e.g. pool shutdown path). }
  if Assigned(FCSRawWebResponse) then
    FCSRawWebResponse.Free;
{ end PATCH-RES-6 }
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

function THorseResponse.Send(const AContent: string): THorseResponse;
begin
{ PATCH-RES-4 — nil-guard }
  if not Assigned(FWebResponse) then
  begin
    FCSBody := AContent;
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
  if AContent = '' then
  begin
    FWebResponse.ContentStream := TMemoryStream.Create;
    FWebResponse.ContentLength := 0;
    Exit(Self);
  end;
{ end PATCH-RES-5 }
{$ENDIF}
  FWebResponse.Content := AContent;
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
  Delphi: TDictionary.Remove  FPC: TStringList delete by IndexOfName
  =========================================================================== }
{$IF NOT DEFINED(FPC)}
  FCustomHeaders.Remove(AName);
{$ELSE}
  I := FCustomHeaders.IndexOfName(AName);
  if I >= 0 then
    FCustomHeaders.Delete(I);
{$ENDIF}
{ =========================================================================== }
  Result := Self;
end;

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

{ PATCH-RES-4 — nil-guard: CrossSocket path captures stream + type as shadow fields }
  if not Assigned(FWebResponse) then
  begin
    FCSContentStream := AFileStream;   // non-owning
    FCSContentType   := LContentType;
    AddHeader('Content-Disposition', Format('inline; filename="%s"', [LFileName]));
    Exit;
  end;
{ end PATCH-RES-4 }

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

{ PATCH-RES-4 — nil-guard }
  if not Assigned(FWebResponse) then
  begin
    FCSContentStream := AFileStream;   // non-owning
    FCSContentType   := LContentType;
    AddHeader('Content-Disposition', Format('attachment; filename="%s"', [LFileName]));
    Exit;
  end;
{ end PATCH-RES-4 }

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

end.
