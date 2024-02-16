unit Horse.Request;

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
{$IF CompilerVersion > 32.0}
  Web.ReqMulti,
{$ENDIF}
{$ENDIF}
  Horse.Core.Param,
  Horse.Core.Param.Header,
  Horse.Commons,
  Horse.Session;

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
    constructor Create(const AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF});
    destructor Destroy; override;
  end;

implementation

function THorseRequest.Body: string;
begin
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
    LParam := THorseCoreParamHeader.GetHeaders(FWebRequest);
    FHeaders := THorseCoreParam.Create(LParam).Required(False);
  end;
  Result := FHeaders;
end;

function THorseRequest.Host: string;
begin
  Result := FWebRequest.Host;
end;

function THorseRequest.ContentType: string;
begin
  Result := FWebRequest.ContentType;
end;

function THorseRequest.PathInfo: string;
var
  LPrefix: string;
begin
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

procedure THorseRequest.InitializeQuery;
var
  LItem, LKey, LValue: string;
  LEqualFirstPos: Integer;
begin
  FQuery := THorseCoreParam.Create(THorseList.Create).Required(False);
  for LItem in FWebRequest.QueryFields do
  begin
    LEqualFirstPos := Pos('=', LItem);
    LKey := Copy(LItem, 1, LEqualFirstPos - 1);
    LValue := Copy(LItem, LEqualFirstPos + 1, Length(LItem));
    FQuery.Dictionary.AddOrSetValue(LKey, LValue);
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

end.
