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
  PATCH-RES-1 — added System.Generics.Collections
  Reason: FCustomHeaders is declared as TDictionary<string, string>.
  Required only when Delphi (not FPC) is the compiler.
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
  iterates this dictionary directly to write headers to ICrossHttpResponse.
  AddHeader populates both FWebResponse.SetCustomHeader (Indy path) and
  this dictionary (CrossSocket path) so all existing middleware that calls
  Res.AddHeader continues to work on both providers without any change.
  =========================================================================== }
    FCustomHeaders: {$IF NOT DEFINED(FPC)}TDictionary<string, string>{$ELSE}TStringList{$ENDIF};
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
  PATCH-RES-3 — added CustomHeaders read-only property
  Reason: TResponseBridge.CopyHeaders reads this property to iterate and
  forward response headers to ICrossHttpResponse. Read-only — the bridge
  iterates only; all writes go through AddHeader as before.
  =========================================================================== }
    property CustomHeaders: {$IF NOT DEFINED(FPC)}TDictionary<string, string>{$ELSE}TStringList{$ENDIF} read FCustomHeaders;
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
  FWebResponse.SetCustomHeader(AName, AValue);
{ ===========================================================================
  PATCH-RES-1 — also populate FCustomHeaders so CrossSocket bridge can read it
  =========================================================================== }
  FCustomHeaders.AddOrSetValue(AName, AValue);
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
  FWebResponse.ContentType := AContentType;
  Result := Self;
end;

constructor THorseResponse.Create(const AWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF});
begin
  FWebResponse := AWebResponse;
{$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := THTTPStatus.Ok.ToInteger;
{$IF DEFINED(FPC)}
  FWebResponse.FreeContentStream := True;
{$ENDIF}
{ ===========================================================================
  PATCH-RES-1 — initialise FCustomHeaders
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
  inherited;
end;

function THorseResponse.RawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
begin
  Result := FWebResponse;
end;

function THorseResponse.Send(const AContent: string): THorseResponse;
begin
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
  FWebResponse.SetCustomHeader('Location', ALocation);
  Result := Status(THTTPStatus.SeeOther);
end;

function THorseResponse.RedirectTo(const ALocation: string; const AStatus: THTTPStatus): THorseResponse;
begin
  FWebResponse.SetCustomHeader('Location', ALocation);
  Result := Status(AStatus);
end;

function THorseResponse.RemoveHeader(const AName: string): THorseResponse;
var
  I: Integer;
begin
  I := FWebResponse.CustomHeaders.IndexOfName(AName);
  if I <> -1 then
    FWebResponse.CustomHeaders.Delete(I);
{ ===========================================================================
  PATCH-RES-1 — also remove from FCustomHeaders
  =========================================================================== }
{$IF NOT DEFINED(FPC)}
  FCustomHeaders.Remove(AName);
{$ELSE}
  I := FCustomHeaders.IndexOfName(AName);
  if I <> -1 then
    FCustomHeaders.Delete(I);
{$ENDIF}
{ =========================================================================== }
  Result := Self;
end;

function THorseResponse.Status(const AStatus: THTTPStatus): THorseResponse;
begin
{$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := AStatus.ToInteger;
  Result := Self;
end;

function THorseResponse.SendFile(const AFileStream: TStream; const AFileName: string; const AContentType: string): THorseResponse;
var
  LFileName: string;
begin
  Result := Self;
  AFileStream.Position := 0;
  LFileName := ExtractFileName(AFileName);

  FWebResponse.FreeContentStream := False;
  FWebResponse.ContentLength := AFileStream.Size;
  FWebResponse.ContentStream := AFileStream;
  FWebResponse.SetCustomHeader('Content-Disposition', Format('inline; filename="%s"', [LFileName]));

  FWebResponse.ContentType := AContentType;
  if (AContentType = EmptyStr) then
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
  LFileName: string;
begin
  Result := Self;
  AFileStream.Position := 0;
  LFileName := ExtractFileName(AFileName);

  FWebResponse.FreeContentStream := False;
  FWebResponse.ContentLength := AFileStream.Size;
  FWebResponse.ContentStream := AFileStream;
  FWebResponse.SetCustomHeader('Content-Disposition', Format('attachment; filename="%s"', [LFileName]));

  FWebResponse.ContentType := AContentType;
  if (AContentType = EmptyStr) then
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
  Result := {$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF};
end;

function THorseResponse.Status(const AStatus: Integer): THorseResponse;
begin
{$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := AStatus;
  Result := Self;
end;

end.
