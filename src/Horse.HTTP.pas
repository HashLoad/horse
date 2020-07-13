unit Horse.HTTP;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections, Web.HTTPApp, IdHTTPHeaderInfo, Horse.Commons;

type
  THorseList = TDictionary<string, string>;

  THorseRequest = class
  private
    FWebRequest: TWebRequest;
    FQuery: THorseList;
    FParams: THorseList;
    FCookie: THorseList;
    FBody: TObject;
    FSession: TObject;
    procedure InitializeQuery;
    procedure InitializeParams;
    procedure InitializeCookie;
    function GetHeaders(AIndex: string): string;
  public
    function Body: string; overload;
    function Body<T: class>: T; overload;
    function Session<T: class>: T;
    function Query: THorseList;
    function Params: THorseList;
    function Cookie: THorseList;
    function MethodType: TMethodType;
    property Headers[index: string]: string read GetHeaders;
    constructor Create(AWebRequest: TWebRequest);
    destructor Destroy; override;
  end;

  THorseHackRequest = class(THorseRequest)
  public
    function GetWebRequest: TWebRequest;
    function GetParams: THorseList;
    procedure SetBody(ABody: TObject);
    procedure SetSession(ASession: TObject);
  end;

  THorseResponse = class
  private
    FWebResponse: TWebResponse;
    FContent: TObject;
  public
    function Send(AContent: string): THorseResponse; overload;
    function Send<T: class>(AContent: T): THorseResponse; overload;
    function Status(AStatus: Integer): THorseResponse; overload;
    function Status(AStatus: THTTPStatus): THorseResponse; overload;
    function Status: Integer; overload;
    constructor Create(AWebResponse: TWebResponse);
    destructor Destroy; override;
  end;

  THorseHackResponse = class(THorseResponse)
  public
    function GetWebResponse: TWebResponse;
    function GetContent: TObject;
  end;

implementation

const
  KEY = 0;
  VALUE = 1;

function THorseRequest.Body: string;
begin
  Result := FWebRequest.Content;
end;

function THorseRequest.Body<T>: T;
begin
  Result := T(FBody);
end;

function THorseRequest.Cookie: THorseList;
begin
  Result := FCookie;
end;

constructor THorseRequest.Create(AWebRequest: TWebRequest);
begin
  FWebRequest := AWebRequest;
  InitializeQuery;
  InitializeParams;
  InitializeCookie;
end;

destructor THorseRequest.Destroy;
begin
  FQuery.Free;
  FParams.Free;
  FCookie.Free;
  if Assigned(FBody) then
    FBody.Free;
  inherited;
end;

function THorseRequest.GetHeaders(AIndex: string): string;
begin
  Result := FWebRequest.GetFieldByName(AIndex);
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
  LParam: TArray<string>;
  LItem: string;
begin
  FQuery := THorseList.Create;
  for LItem in FWebRequest.QueryFields do
  begin
    LParam := LItem.Split(['=']);
    FQuery.Add(LParam[KEY], LParam[VALUE]);
  end;
end;

function THorseRequest.MethodType: TMethodType;
begin
  Result := FWebRequest.MethodType;
end;

function THorseRequest.Params: THorseList;
begin
  Result := FParams;
end;

function THorseRequest.Query: THorseList;
begin
  Result := FQuery;
end;

function THorseRequest.Session<T>: T;
begin
  Result := T(FSession);
end;

{ THorseResponse }

constructor THorseResponse.Create(AWebResponse: TWebResponse);
begin
  FWebResponse := AWebResponse;
end;

destructor THorseResponse.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
  inherited;
end;

function THorseResponse.Send(AContent: string): THorseResponse;
begin
  FWebResponse.StatusCode := THTTPStatus.OK.ToInteger;
  FWebResponse.Content := AContent;
  Result := Self;
end;

function THorseResponse.Send<T>(AContent: T): THorseResponse;
begin
  FWebResponse.StatusCode := THTTPStatus.OK.ToInteger;
  FContent := AContent;
  Result := Self;
end;

function THorseResponse.Status(AStatus: THTTPStatus): THorseResponse;
begin
  FWebResponse.StatusCode := AStatus.ToInteger;
  Result := Self;
end;

function THorseResponse.Status: Integer;
begin
  Result := FWebResponse.StatusCode;
end;

function THorseResponse.Status(AStatus: Integer): THorseResponse;
begin
  FWebResponse.StatusCode := AStatus;
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

function THorseHackRequest.GetWebRequest: TWebRequest;
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

function THorseHackResponse.GetWebResponse: TWebResponse;
begin
  Result := FWebResponse;
end;

end.