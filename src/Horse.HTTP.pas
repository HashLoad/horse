unit Horse.HTTP;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, IdHTTPHeaderInfo, IdCustomHTTPServer;

type

  EHorseCallbackInterrupted = class(Exception)
    constructor Create; reintroduce;
  end;

  THorseList = TDictionary<string, string>;

  THorseRequest = class
  private
    FWebRequest: TIdHTTPRequestInfo;
    FQuery: THorseList;
    FParams: THorseList;
    FHeaders: THorseList;
    FBody: TObject;
    FSession: TObject;
    procedure InitializeQuery;
    procedure InitializeParams;
    procedure InitializeHeaders;
  public
    function Body: string; overload;
    function Body<T: class>: T; overload;
    function Session<T: class>: T;
    function Query: THorseList;
    function Params: THorseList;
    function Headers: THorseList;
    constructor Create(AWebRequest: TIdHTTPRequestInfo);
    destructor Destroy; override;
  end;

  THorseHackRequest = class(THorseRequest)
  public
    function GetWebRequest: TIdHTTPRequestInfo;
    function GetParams: THorseList;
    procedure SetBody(ABody: TObject);
    procedure SetSession(ASession: TObject);
  end;

  THorseResponse = class
  private
    FWebResponse: TIdHTTPResponseInfo;
    FContent: TObject;
  public
    function Send(AContent: string): THorseResponse; overload;
    function Send<T: class>(AContent: T): THorseResponse; overload;
    function Status(AStatus: Integer): THorseResponse; overload;
    function Status: Integer; overload;
    constructor Create(AWebResponse: TIdHTTPResponseInfo);
    destructor Destroy; override;
  end;

  THorseHackResponse = class(THorseResponse)
  public
    function GetWebResponse: TIdHTTPResponseInfo;
    function GetContent: TObject;
  end;

  TIdHTTPAppRequestHelper = class helper for TIdHTTPRequestInfo
  public
    function GetRequestInfo: TIdEntityHeaderInfo;
  end;

implementation

{ THorseRequest }

function THorseRequest.Body: string;
begin
  Result := FWebRequest.Document;
end;

function THorseRequest.Body<T>: T;
begin
  Result := T(FBody);
end;

constructor THorseRequest.Create(AWebRequest: TIdHTTPRequestInfo);
begin
  FWebRequest := AWebRequest;
  InitializeQuery;
  InitializeParams;
  InitializeHeaders;
end;

destructor THorseRequest.Destroy;
begin
  FQuery.Free;
  FParams.Free;
  FHeaders.Free;
  inherited;
end;

function THorseRequest.Headers: THorseList;
begin
  Result := FHeaders;
end;

procedure THorseRequest.InitializeHeaders;
var
  LKey, LValue, LHeader: string;
  LPosSeparator: Integer;
begin
  FHeaders := THorseList.Create;
  for LHeader in TIdHTTPRequestInfo(FWebRequest).GetRequestInfo.RawHeaders do
  begin
    LPosSeparator := Pos(':', LHeader);
    LKey := Copy(LHeader, 0, LPosSeparator - 1);
    LValue := Copy(LHeader, LPosSeparator + 1, LHeader.Length - LPosSeparator);
    FHeaders.Add(LowerCase(LKey), Trim(LValue));
  end;
end;

procedure THorseRequest.InitializeParams;
begin
  FParams := THorseList.Create;
end;

procedure THorseRequest.InitializeQuery;
const
  KEY = 0;
  VALUE = 1;
var
  LParam: TArray<string>;
  LItem: string;
begin
  FQuery := THorseList.Create;
  for LItem in FWebRequest.QueryParams.Split([';']) do
  begin
    LParam := LItem.Split(['=']);
    FQuery.Add(LParam[KEY], LParam[VALUE]);
  end;
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

constructor THorseResponse.Create(AWebResponse: TIdHTTPResponseInfo);
begin
  FWebResponse := AWebResponse;
end;

destructor THorseResponse.Destroy;
begin
  inherited;
end;

function THorseResponse.Send(AContent: string): THorseResponse;
begin
  FWebResponse.ResponseNo := 200;
  FWebResponse.ContentText := AContent;
  Result := Self;
end;

function THorseResponse.Send<T>(AContent: T): THorseResponse;
begin
  FWebResponse.ResponseNo := 200;
  FContent := AContent;
  Result := Self;
end;

function THorseResponse.Status: Integer;
begin
  Result := FWebResponse.ResponseNo;
end;

function THorseResponse.Status(AStatus: Integer): THorseResponse;
begin
  FWebResponse.ResponseNo := AStatus;
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

function THorseHackRequest.GetWebRequest: TIdHTTPRequestInfo;
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

function THorseHackResponse.GetWebResponse: TIdHTTPResponseInfo;
begin
  Result := FWebResponse;
end;

{ TIdHTTPAppRequestHelper }

function TIdHTTPAppRequestHelper.GetRequestInfo: TIdEntityHeaderInfo;
begin
  Result := Self;
end;

{ EHorseCallbackInterrupted }

constructor EHorseCallbackInterrupted.Create;
begin
  inherited Create('');
end;

end.
