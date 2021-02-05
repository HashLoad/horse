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
  {$IFDEF VER320_UP}
  Web.ReqMulti,
  {$ENDIF}
{$ENDIF}
  Horse.Commons;

type
  THorseList = {$IF DEFINED(FPC)}TDictionary<string, string>{$ELSE}TDictionary<string, string>{$ENDIF};

  THorseRequest = class
  private
    FWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
    FQuery: THorseList;
    FParams: THorseList;
    FContentFields: THorseList;
    FCookie: THorseList;
    FBody: TObject;
    FSession: TObject;
    procedure InitializeQuery;
    procedure InitializeParams;
    procedure InitializeContentFields;
    procedure InitializeCookie;
    function GetHeaders(AIndex: string): string;
    function IsMultipartForm: Boolean;
    function IsFormURLEncoded: Boolean;
    function CanLoadContentFields: Boolean;
  public
    function Body: string; overload;
    function Body<T: class>: T; overload;
    function Body(ABody: TObject): THorseRequest; overload;
    function Session<T: class>: T; overload;
    function Session(ASession: TObject): THorseRequest; overload;
    function Query: THorseList;
    function Params: THorseList;
    function Cookie: THorseList;
    function ContentFields: THorseList;
    function MethodType: TMethodType;
    function RawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
    property Headers[index: string]: string read GetHeaders;
    constructor Create(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF});
    destructor Destroy; override;
  end;

  THorseHackRequest = class(THorseRequest)
  public
    function GetWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}; deprecated 'Dont use the THorseHackRequest class';
    function GetParams: THorseList; deprecated 'Dont use the THorseHackRequest class';
    procedure SetBody(ABody: TObject); deprecated 'Dont use the THorseHackRequest class';
    procedure SetSession(ASession: TObject); deprecated 'Dont use the THorseHackRequest class';
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
    function GetWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF}; deprecated 'Dont use the THorseHackResponse class';
    function GetContent: TObject; deprecated 'Dont use the THorseHackResponse class';
    procedure SetContent(AContent: TObject); deprecated 'Dont use the THorseHackResponse class';
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

function THorseRequest.ContentFields: THorseList;
begin
  if not Assigned(FContentFields) then
    InitializeContentFields;
  Result := FContentFields;
end;

function THorseRequest.Cookie: THorseList;
begin
  if not Assigned(FCookie) then
    InitializeCookie;
  Result := FCookie;
end;

constructor THorseRequest.Create(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF});
begin
  FWebRequest := AWebRequest;
end;

destructor THorseRequest.Destroy;
begin
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
  inherited;
end;

function THorseRequest.GetHeaders(AIndex: string): string;
begin
  Result := FWebRequest.GetFieldByName(AIndex);
end;

procedure THorseRequest.InitializeContentFields;
var
  I: Integer;
begin
  FContentFields := THorseList.Create;
  if (not CanLoadContentFields) then
    Exit;
  for I := 0 to Pred(FWebRequest.ContentFields.Count) do
  begin
    FContentFields.AddOrSetValue(LowerCase(FWebRequest.ContentFields.Names[I]),
      FWebRequest.ContentFields.ValueFromIndex[I]);
  end;
end;

procedure THorseRequest.InitializeCookie;
var
  LParam: TArray<string>;
  LItem: string;
begin
  FCookie := THorseList.Create;
  for LItem in FWebRequest.CookieFields do
  begin
    LParam := LItem.Split(['=']);
    FCookie.Add(LParam[KEY], LParam[VALUE]);
  end;
end;

procedure THorseRequest.InitializeParams;
begin
  FParams := THorseList.Create;
end;

procedure THorseRequest.InitializeQuery;
var
  LItem: string;
  LKey: string;
  LValue: string;
  LEqualFirstPos : Integer;  
begin
  FQuery := THorseList.Create;
  for LItem in FWebRequest.QueryFields do
  begin
    LEqualFirstPos := Pos('=', Litem);
    LKey := Copy(Litem, 1, LEqualFirstPos - 1);
    LValue := Copy(Litem, LEqualFirstPos + 1, Length(LItem));
    FQuery.Add(LKey, LValue);
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
  Result := {$IF DEFINED(FPC)} StringCommandToMethodType(FWebRequest.Command){$ELSE}FWebRequest.MethodType;{$ENDIF}
end;

function THorseRequest.Params: THorseList;
begin
  if not Assigned(FParams) then
    InitializeParams;
  Result := FParams;
end;

function THorseRequest.Query: THorseList;
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
  Result := FParams;
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

end.
