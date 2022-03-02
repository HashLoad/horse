unit Horse.Request;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Classes, fpHTTP, HTTPDefs,
{$ELSE}
  System.SysUtils, System.Classes, Web.HTTPApp,
  {$IF CompilerVersion > 32.0}
    Web.ReqMulti,
  {$ENDIF}
{$ENDIF}
  Horse.Core.Param, Horse.Core.Param.Header, Horse.Commons, Horse.Session;

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
    function Body: string; overload;
    function Body<T: class>: T; overload;
    function Body(const ABody: TObject): THorseRequest; overload;
    function Session<T: class>: T; overload;
    function Session(const ASession: TObject): THorseRequest; overload;
    function Headers: THorseCoreParam;
    function Query: THorseCoreParam;
    function Params: THorseCoreParam;
    function Cookie: THorseCoreParam;
    function ContentFields: THorseCoreParam;
    function MethodType: TMethodType;
    function RawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
    property Sessions: THorseSessions read FSessions;
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
  result := FHeaders;
end;

procedure THorseRequest.InitializeContentFields;
var
  I: Integer;
begin
  FContentFields := THorseCoreParam.Create(THorseList.Create).Required(False);
  if (not CanLoadContentFields) then
    Exit;
  for I := 0 to Pred(FWebRequest.ContentFields.Count) do
    FContentFields.Dictionary.AddOrSetValue(FWebRequest.ContentFields.Names[I], FWebRequest.ContentFields.ValueFromIndex[I]);
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
    LEqualFirstPos := Pos('=', Litem);
    LKey := Copy(Litem, 1, LEqualFirstPos - 1);
    LValue := Copy(Litem, LEqualFirstPos + 1, Length(LItem));
    FQuery.Dictionary.AddOrSetValue(LKey, LValue);
  end;
end;

function THorseRequest.IsFormURLEncoded: Boolean;
begin
  Result := StrLIComp(PChar(FWebRequest.ContentType), PChar(TMimeTypes.ApplicationXWWWFormURLEncoded.ToString),
    Length(TMimeTypes.ApplicationXWWWFormURLEncoded.ToString)) = 0;
end;

function THorseRequest.IsMultipartForm: Boolean;
begin
  Result := StrLIComp(PChar(FWebRequest.ContentType), PChar(TMimeTypes.MultiPartFormData.ToString),
    Length(TMimeTypes.MultiPartFormData.ToString)) = 0;
end;

function THorseRequest.MethodType: TMethodType;
begin
  Result := {$IF DEFINED(FPC)}StringCommandToMethodType(FWebRequest.Method);{$ELSE}FWebRequest.MethodType;{$ENDIF}
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

end.
