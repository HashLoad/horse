unit Horse.Commons;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
  {$MODESWITCH TypeHelpers}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Classes, SysUtils, StrUtils, RegExpr
{$ELSE}
  System.Classes, System.SysUtils
{$ENDIF}
 ;

type
{$IF DEFINED(FPC)}
  TMethodType = (mtAny, mtGet, mtPut, mtPost, mtHead, mtDelete, mtPatch);
{$ENDIF}

{$SCOPEDENUMS ON}
  THTTPStatus = (
    Continue = 100,
    SwitchingProtocols = 101,
    Processing = 102,
    OK = 200,
    Created = 201,
    Accepted = 202,
    NonAuthoritativeInformation = 203,
    NoContent = 204,
    ResetContent = 205,
    PartialContent = 206,
    MultiStatus = 207,
    AlreadyReported = 208,
    IMUsed = 226,
    MultipleChoices = 300,
    MovedPermanently = 301,
    Found = 302,
    SeeOther = 303,
    NotModified = 304,
    UseProxy = 305,
    TemporaryRedirect = 307,
    PermanentRedirect = 308,
    BadRequest = 400,
    Unauthorized = 401,
    PaymentRequired = 402,
    Forbidden = 403,
    NotFound = 404,
    MethodNotAllowed = 405,
    NotAcceptable = 406,
    ProxyAuthenticationRequired = 407,
    RequestTimeout = 408,
    Conflict = 409,
    Gone = 410,
    LengthRequired = 411,
    PreconditionFailed = 412,
    PayloadTooLarge = 413,
    RequestURITooLong = 414,
    UnsupportedMediaType = 415,
    RequestedRangeNotSatisfiable = 416,
    ExpectationFailed = 417,
    Imateapot = 418,
    MisdirectedRequest = 421,
    UnprocessableEntity = 422,
    Locked = 423,
    FailedDependency = 424,
    UpgradeRequired = 426,
    PreconditionRequired = 428,
    TooManyRequests = 429,
    RequestHeaderFieldsTooLarge = 431,
    ConnectionClosedWithoutResponse = 444,
    UnavailableForLegalReasons = 451,
    ClientClosedRequest = 499,
    InternalServerError = 500,
    NotImplemented = 501,
    BadGateway = 502,
    ServiceUnavailable = 503,
    GatewayTimeout = 504,
    HTTPVersionNotSupported = 505,
    VariantAlsoNegotiates = 506,
    InsufficientStorage = 507,
    LoopDetected = 508,
    NotExtended = 510,
    NetworkAuthenticationRequired = 511,
    NetworkConnectTimeoutError = 599);

  TMimeTypes = (
    MultiPartFormData,
    ApplicationXWWWFormURLEncoded,
    ApplicationJSON,
    ApplicationOctetStream,
    ApplicationXML,
    ApplicationJavaScript,
    ApplicationPDF,
    ApplicationTypeScript,
    ApplicationZIP,
    TextPlain,
    TextCSS,
    TextCSV,
    TextHTML,
    ImageJPEG,
    ImagePNG,
    ImageGIF,
    Download);

  TMessageType = (Default, Error, Warning, Information);
  TLhsBracketsType = (Equal, NotEqual, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Range, Like);
{$SCOPEDENUMS OFF}

  TLhsBrackets = set of TLhsBracketsType;

  THTTPStatusHelper = {$IF DEFINED(FPC)} type {$ELSE} record {$ENDIF} helper for THTTPStatus
    function ToInteger: Integer;
  end;

  TMimeTypesHelper = {$IF DEFINED(FPC)} type {$ELSE} record {$ENDIF} helper for TMimeTypes
    function ToString: string;
  end;

  TLhsBracketsTypeHelper  = {$IF DEFINED(FPC)} type {$ELSE} record {$ENDIF} helper for TLhsBracketsType
    function ToString: string;
  end;

{$IF DEFINED(FPC)}
function StringCommandToMethodType(const ACommand: string): TMethodType;
function MatchRoute(const AText: string; const AValues: array of string): Boolean;
{$ENDIF}

implementation

{$IF DEFINED(FPC)}
function StringCommandToMethodType(const ACommand: string): TMethodType;
begin
  case AnsiIndexText(ACommand, ['ANY', 'DELETE', 'GET', 'HEAD', 'PATCH', 'POST', 'PUT']) of
    0: Result := TMethodType.mtAny;
    1: Result := TMethodType.mtDelete;
    2: Result := TMethodType.mtGet;
    3: Result := TMethodType.mtHead;
    4: Result := TMethodType.mtPatch;
    5: Result := TMethodType.mtPost;
    6: Result := TMethodType.mtPut;
  end;
end;

function MatchRoute(const AText: string; const AValues: array of string): Boolean;
  function ReplaceParams(const AValue: string): String;
  var
    LPart: string;
    LSplitedPath: TArray<string>;
  begin
    Result := AValue;
    LSplitedPath := AValue.Split(['/']);
    for LPart in LSplitedPath do
    begin
      if LPart.StartsWith(':') then
        Result := StringReplace(Result,LPart,'([^/]*)/',[]);
    end;
  end;
var
  I: Integer;
  RegexObj: TRegExpr;
  LText: string;
begin
  Result := False;
  RegexObj := TRegExpr.Create;
  try
    LText := Trim(AText);
    for I := Low(AValues) to High(AValues) do
    begin
      RegexObj.Expression := '^('+ReplaceParams(AValues[I])+')$';
      if not (LText.EndsWith('/')) then
        LText := LText + '/';

      if RegexObj.Exec(LText) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    RegexObj.Free;
  end;
end;

{$ENDIF}

{ TLhsBracketsTypeHelper }

function TLhsBracketsTypeHelper.ToString: string;
begin
  case Self of
    TLhsBracketsType.Equal:
      Result := '[eq]';
    TLhsBracketsType.NotEqual:
      Result := '[ne]';
    TLhsBracketsType.LessThan:
      Result := '[lt]';
    TLhsBracketsType.LessThanOrEqual:
      Result := '[lte]';
    TLhsBracketsType.GreaterThan:
      Result := '[gt]';
    TLhsBracketsType.GreaterThanOrEqual:
      Result := '[gte]';
    TLhsBracketsType.Range:
      Result := '[range]';
    TLhsBracketsType.Like:
      Result := '[like]';
  end;
end;

{ THTTPStatusHelper }

function THTTPStatusHelper.ToInteger: Integer;
begin
  Result := Ord(Self);
end;

{ TMimeTypesHelper }

function TMimeTypesHelper.ToString: string;
begin
  case Self of
    TMimeTypes.MultiPartFormData:
      Result := 'multipart/form-data';
    TMimeTypes.ApplicationXWWWFormURLEncoded:
      Result := 'application/x-www-form-urlencoded';
    TMimeTypes.ApplicationJSON:
      Result := 'application/json';
    TMimeTypes.ApplicationOctetStream:
      Result := 'application/octet-stream';
    TMimeTypes.ApplicationXML:
      Result := 'application/xml';
    TMimeTypes.ApplicationJavaScript:
      Result := 'application/javascript';
    TMimeTypes.ApplicationPDF:
      Result := 'application/pdf';
    TMimeTypes.ApplicationTypeScript:
      Result := 'application/typescript';
    TMimeTypes.ApplicationZIP:
      Result := 'application/zip';
    TMimeTypes.TextPlain:
      Result := 'text/plain';
    TMimeTypes.TextCSS:
      Result := 'text/css';
    TMimeTypes.TextCSV:
      Result := 'text/csv';
    TMimeTypes.TextHTML:
      Result := 'text/html';
    TMimeTypes.ImageJPEG:
      Result := 'image/jpeg';
    TMimeTypes.ImagePNG:
      Result := 'image/png';
    TMimeTypes.ImageGIF:
      Result := 'image/gif';
    TMimeTypes.Download:
      Result := 'application/x-download';
  end;
end;

end.
