unit Horse.Response;

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
  md5,
{$ELSE}
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  IdHashMessageDigest,
{$IF CompilerVersion > 32.0}
  Web.ReqMulti,
{$ENDIF}
{$ENDIF}
  Horse.Commons,
  Horse.Core.Files,
  Horse.Mime;

type
  THorseResponse = class
  private
    FWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
    FContent: TObject;
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
    function ETagMatch(const AContent: string): Boolean; overload; virtual;
    function ETagMatch(const AContent: TStream): Boolean; overload; virtual;
    function Status: Integer; overload; virtual;
    function AddHeader(const AName, AValue: string): THorseResponse; virtual;
    function RemoveHeader(const AName: string): THorseResponse; virtual;
    function Content: TObject; overload; virtual;
    function Content(const AContent: TObject): THorseResponse; overload; virtual;
    function ContentType(const AContentType: string): THorseResponse; virtual;
    function RawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF}; virtual;
    constructor Create(const AWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF});
    destructor Destroy; override;
  end;

implementation

function THorseResponse.AddHeader(const AName, AValue: string): THorseResponse;
begin
  FWebResponse.SetCustomHeader(AName, AValue);
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
end;

destructor THorseResponse.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
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

function THorseResponse.ETagMatch(const AContent: TStream): Boolean;
var
  {$IFNDEF FPC}
  lHash: TIdHashMessageDigest5;
  {$ENDIF}
  lETagRequest: string;
  lETagContent: string;
begin
  Result := False;

  if not Assigned(AContent) then
    Exit;

  AContent.Position := 0;
  {$IF DEFINED(FPC)}
  lETagContent := MD5Print(MD5Buffer(TMemoryStream(AContent).Memory^, AContent.Size));
  {$ELSE}
  lHash := TIdHashMessageDigest5.Create;
  try
    lETagContent := lHash.HashStreamAsHex(AContent);
  finally
    lHash.Free;
  end;
  {$ENDIF}

  if (lETagContent = '') then
    Exit;

  FWebResponse.SetCustomHeader('ETag', lETagContent);
  {$IF DEFINED(FPC)}
  lETagRequest := RawWebResponse.Request.GetFieldByName('if-None-Match');
  {$ELSE}
  lETagRequest := RawWebResponse.HTTPRequest.GetFieldByName('if-None-Match');
  {$ENDIF}

  if (lETagRequest = lETagContent) then
  begin
    Result := True;
    FWebResponse.ContentType := '';
    FWebResponse.Content := '';
    {$IF DEFINED(FPC)}
    FWebResponse.Code := THTTPStatus.NotModified.Tointeger;
    FWebResponse.SendContent;
    {$ELSE}
    FWebResponse.StatusCode := THTTPStatus.NotModified.ToInteger;
    FWebResponse.SendResponse;
    {$ENDIF}
  end;
end;

function THorseResponse.ETagMatch(const AContent: string): Boolean;
var
  lContentStream: TBytesStream;
begin
  lContentStream := TBytesStream.Create;
  try
    lContentStream.Write(AContent[1], Length(AContent) * SizeOf(Char));
    Result := ETagMatch(lContentStream);
  finally
    lContentStream.Free;
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
