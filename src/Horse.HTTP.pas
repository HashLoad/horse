unit Horse.HTTP;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Classes, Generics.Collections, fpHTTP, HTTPDefs,
{$ELSE}
  System.SysUtils, System.Classes, Web.HTTPApp, System.Generics.Collections,
  {$IF CompilerVersion > 32.0}
  Web.ReqMulti,
  {$ENDIF}
{$ENDIF}
  Horse.Core.Param,
  Horse.Core.Param.Header,
  Horse.Commons;

type
  THorseSessions = class
  private
    FSessions: TObjectDictionary<TSessionClass, TSession>;
    function GetSession(const ASessionClass: TSessionClass): TSession; overload;
    function GetObject(const ASessionClass: TSessionClass): TObject; overload;
  public
    function SetSession(const ASessionClass: TSessionClass; AInstance: TSession): THorseSessions;
    property Session[const SessionClass: TSessionClass]: TSession read GetSession;
    property &Object[const SessionClass: TSessionClass]: TObject read GetObject;
    constructor Create;
    destructor Destroy; override;
  end;

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
    function Body(ABody: TObject): THorseRequest; overload;
    function Session<T: class>: T; overload;
    function Session(ASession: TObject): THorseRequest; overload;
    function Headers: THorseCoreParam;
    function Query: THorseCoreParam;
    function Params: THorseCoreParam;
    function Cookie: THorseCoreParam;
    function ContentFields: THorseCoreParam;
    function MethodType: TMethodType;
    function RawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
    property Sessions: THorseSessions read FSessions;
    constructor Create(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF});
    destructor Destroy; override;
  end;

  THorseHackRequest = class(THorseRequest)
  public
    function GetWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}; deprecated 'Dont use the THorseHackRequest class. Use RawWebRequest method of THorseRequest class';
    function GetParams: THorseList; deprecated 'Dont use the THorseHackRequest class. Use Params method of THorseRequest class';
    procedure SetBody(ABody: TObject); deprecated 'Dont use the THorseHackRequest class. Use Body method of THorseRequest class';
    procedure SetSession(ASession: TObject); deprecated 'Dont use the THorseHackRequest class. Use Session method of THorseRequest class';
  end;

  THorseResponse = class
  private
    FWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
    FContent: TObject;
  public
    function Send(AContent: string): THorseResponse; overload;
    function Send<T: class>(AContent: T): THorseResponse; overload;
    function Status(AStatus: Integer): THorseResponse; overload;
    function Status(AStatus: THTTPStatus): THorseResponse; overload;
    function Status: Integer; overload;
    function Content: TObject; overload;
    function Content(AContent: TObject): THorseResponse; overload;
    function ContentType(AContentType: string): THorseResponse;
    function RawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
    constructor Create(AWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF});
    destructor Destroy; override;
  end;

  THorseHackResponse = class(THorseResponse)
  public
    function GetWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF}; deprecated 'Dont use the THorseHackResponse class. Use RawWebResponse method of THorseResponse class';
    function GetContent: TObject; deprecated 'Dont use the THorseHackResponse class. Use Content method of THorseResponse class';
    procedure SetContent(AContent: TObject); deprecated 'Dont use the THorseHackResponse class. Use Content method of THorseResponse class';
  end;

implementation

const
  KEY = 0;
  VALUE = 1;

function THorseRequest.Body: string;
begin
  Result := FWebRequest.Content;
end;

function THorseRequest.Body(ABody: TObject): THorseRequest;
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

constructor THorseRequest.Create(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF});
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
    FHeaders := THorseCoreParam.create(LParam);
  end;
  result := FHeaders;
end;

procedure THorseRequest.InitializeContentFields;
var
  I: Integer;
begin
  FContentFields := THorseCoreParam.create(THorseList.Create);
  if (not CanLoadContentFields) then
    Exit;
  for I := 0 to Pred(FWebRequest.ContentFields.Count) do
    FContentFields.Dictionary.AddOrSetValue(FWebRequest.ContentFields.Names[I], FWebRequest.ContentFields.ValueFromIndex[I]);
end;

procedure THorseRequest.InitializeCookie;
var
  LParam: TArray<string>;
  LItem: string;
begin
  FCookie := THorseCoreParam.create(THorseList.Create);
  for LItem in FWebRequest.CookieFields do
  begin
    LParam := LItem.Split(['=']);
    FCookie.Dictionary.AddOrSetValue(LParam[KEY], LParam[VALUE]);
  end;
end;

procedure THorseRequest.InitializeParams;
begin
  FParams := THorseCoreParam.create(THorseList.Create);
end;

procedure THorseRequest.InitializeQuery;
var
  LItem: string;
  LKey: string;
  LValue: string;
  LEqualFirstPos: Integer;
begin
  FQuery := THorseCoreParam.create(THorseList.Create);
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

function THorseRequest.Session(ASession: TObject): THorseRequest;
begin
  Result := Self;
  FSession := ASession;
end;

function THorseRequest.Session<T>: T;
begin
  Result := T(FSession);
end;

{ THorseResponse }

function THorseResponse.Content(AContent: TObject): THorseResponse;
begin
  Result := Self;
  FContent := AContent;
end;

function THorseResponse.Content: TObject;
begin
  Result := FContent;
end;

function THorseResponse.ContentType(AContentType: string): THorseResponse;
begin
  FWebResponse.ContentType := AContentType;
  Result := Self;
end;

constructor THorseResponse.Create(AWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF});
begin
  FWebResponse := AWebResponse;
  {$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := THTTPStatus.Ok.ToInteger;
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

function THorseResponse.Send(AContent: string): THorseResponse;
begin
  FWebResponse.Content := AContent;
  Result := Self;
end;

function THorseResponse.Send<T>(AContent: T): THorseResponse;
begin
  FContent := AContent;
  Result := Self;
end;

function THorseResponse.Status(AStatus: THTTPStatus): THorseResponse;
begin
  {$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := AStatus.ToInteger;
  Result := Self;
end;

function THorseResponse.Status: Integer;
begin
  Result := {$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF};
end;

function THorseResponse.Status(AStatus: Integer): THorseResponse;
begin
  {$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := AStatus;
  Result := Self;
end;

{ THorseHackRequest }

function THorseHackResponse.GetContent: TObject;
begin
  Result := FContent;
end;

function THorseHackRequest.GetParams: THorseList;
begin
  Result := FParams.Dictionary;
end;

function THorseHackRequest.GetWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
begin
  Result := FWebRequest;
end;

procedure THorseHackRequest.SetBody(ABody: TObject);
begin
  FBody := ABody;
end;

procedure THorseHackRequest.SetSession(ASession: TObject);
begin
  FSession := ASession;
end;

{ THorseHackResponse }

function THorseHackResponse.GetWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
begin
  Result := FWebResponse;
end;

procedure THorseHackResponse.SetContent(AContent: TObject);
begin
  FContent := AContent;
end;

{ THorseSessions }

constructor THorseSessions.Create;
begin
  FSessions := TObjectDictionary<TSessionClass, TSession>.Create([doOwnsValues]);
end;

destructor THorseSessions.Destroy;
begin
  FSessions.Free;
  inherited Destroy;
end;

function THorseSessions.GetObject(const ASessionClass: TSessionClass): TObject;
begin
  Result := FSessions.Items[ASessionClass];
end;

function THorseSessions.GetSession(const ASessionClass: TSessionClass): TSession;
begin
  Result := FSessions.Items[ASessionClass];
end;

function THorseSessions.SetSession(const ASessionClass: TSessionClass;
  AInstance: TSession): THorseSessions;
begin
  Result := Self;
  if not ASessionClass.InheritsFrom(AInstance.ClassType) then
    raise Exception.CreateFmt('SessionClass differs from of instance[%s].', [AInstance.ClassType.ClassName]);
  FSessions.AddOrSetValue(ASessionClass, AInstance);
end;

end.
