unit Horse.Commons;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
  {$MODESWITCH TypeHelpers}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Classes,
  SysUtils,
  StrUtils,
  DateUtils,
  RegExpr;
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.RegularExpressions;
{$ENDIF}

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
  TLhsBracketsType = (Equal, NotEqual, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Range, Like,
    Contains, StartsWith, EndsWith);
{$SCOPEDENUMS OFF}

  TLhsBrackets = set of TLhsBracketsType;

  THTTPStatusHelper = {$IF DEFINED(FPC)} type {$ELSE} record {$ENDIF} helper for THTTPStatus
    function ToInteger: Integer;
  end;

  TMimeTypesHelper = {$IF DEFINED(FPC)} type {$ELSE} record {$ENDIF} helper
  for TMimeTypes
    function ToString: string;
  end;

  TLhsBracketsTypeHelper = {$IF DEFINED(FPC)} type {$ELSE} record {$ENDIF} helper for TLhsBracketsType
    function ToString: string;
  end;

{$IF DEFINED(FPC)}
function StringCommandToMethodType(const ACommand: string): TMethodType;
{$ENDIF}

function MatchRoute(const AText: string; const AValues: array of string): Boolean;
function IsDateTime(const AText: string; out AParsedValue: TDateTime): Boolean;

implementation

function IsDateTime(const AText: string; out AParsedValue: TDateTime): Boolean;
const
  // Formatos de data
  C_DATE_FORMATS: array of string = [
    // Dia, mês, ano
    'dd/MM/yyyy', 'dd-MM-yyyy', 'dd.MM.yyyy',
    'dd/MM/yy',   'dd-MM-yy',   'dd.MM.yy',

    // Ano, mês, dia
    'yyyy/MM/dd', 'yyyy-MM-dd', 'yyyy.MM.dd',
    'yy/MM/dd',   'yy-MM-dd',   'yy.MM.dd',

    // Mês, dia, ano
    'MM/dd/yyyy', 'MM-dd-yyyy', 'MM.dd.yyyy',
    'MM/dd/yy',   'MM-dd-yy',   'MM.dd.yy',

    // ISO padrão com T
    'yyyy-MM-dd"T"',            // usado como prefixo para os formatos DateTime ISO
    'yyyy-MM-dd'
  ];

  // Formatos de hora
  C_TIME_FORMATS: array of string = [
    // Horas e minutos
    'hh:nn', 'hh:nn AM/PM',

    // Horas, minutos e segundos
    'hh:nn:ss', 'hh:nn:ss AM/PM',

    // Com milissegundos
    'hh:nn:ss.zzz', 'hh:nn:ss.zzz AM/PM',

    // ISO 8601 / UTC com T
    '"T"hh:nn:ss',         // T + hora
    '"T"hh:nn:ss"Z"',      // UTC (Z)
    '"T"hh:nn:sszzz',      // UTC com milissegundos
    '"T"hh:nn:ss.zzz"Z"',  // UTC com ms e Z

    // ISO 8601 com timezone
    '"T"hh:nn:ss+hh:nn', '"T"hh:nn:ss-hh:nn',
    '"T"hh:nn:ss+hhmm',  '"T"hh:nn:ss-hhmm'
  ];

  // Formatos DateTime combinados
  C_DATETIME_FORMATS: array of string = [
    // Combinações comuns
    'dd/MM/yyyy hh:nn:ss',
    'dd-MM-yyyy hh:nn:ss',
    'yyyy-MM-dd hh:nn:ss',
    'yyyy/MM/dd hh:nn:ss',
    'dd/MM/yyyy hh:nn:ss AM/PM',
    'yyyy-MM-dd hh:nn:ss AM/PM',
    'yyyy-MM-dd"T"hh:nn:ss',
    'yyyy-MM-dd"T"hh:nn:ss.zzz',
    'yyyy-MM-dd"T"hh:nn:ss"Z"',
    'yyyy-MM-dd"T"hh:nn:ss.zzz"Z"',
    'yyyy-MM-dd"T"hh:nn:ss+hh:nn',
    'yyyy-MM-dd"T"hh:nn:ss-hh:nn',
    'yyyy-MM-dd"T"hh:nn:ss+hhmm',
    'yyyy-MM-dd"T"hh:nn:ss-hhmm'
  ];
var
  LFormat: string;
  LFormatSettings: TFormatSettings;
begin
  Result := False;
  AParsedValue := 0;
  LFormatSettings := {$IF DEFINED(FPC)}DefaultFormatSettings{$ELSE}TFormatSettings.Create{$ENDIF};
  // Tentativa com formatos DateTime
  for LFormat in C_DATETIME_FORMATS do
  begin
    try
      LFormatSettings.LongDateFormat := LFormat;
      AParsedValue := StrToDateTime(AText, LFormatSettings);
      Exit(True);
    except
      // Ignora erro de conversão
    end;
  end;

  // Tentativa com formatos apenas de data
  for LFormat in C_DATE_FORMATS do
  begin
    try
      LFormatSettings.ShortDateFormat := LFormat;
      AParsedValue := StrToDate(AText, LFormatSettings);
      Exit(True);
    except
      // Ignora erro de conversão
    end;
  end;

  // Tentativa com formatos apenas de hora
  for LFormat in C_TIME_FORMATS do
  begin
    try
      LFormatSettings.ShortTimeFormat := LFormat;
      AParsedValue := StrToTime(AText, LFormatSettings);
      Exit(True);
    except
      // Ignora erro de conversão
    end;
  end;
end;

{$IF DEFINED(FPC)}
function StringCommandToMethodType(const ACommand: string): TMethodType;
begin
  Result := TMethodType.mtAny;
  case AnsiIndexText(ACommand, ['DELETE', 'GET', 'HEAD', 'PATCH', 'POST', 'PUT']) of
    0:
      Result := TMethodType.mtDelete;
    1:
      Result := TMethodType.mtGet;
    2:
      Result := TMethodType.mtHead;
    3:
      Result := TMethodType.mtPatch;
    4:
      Result := TMethodType.mtPost;
    5:
      Result := TMethodType.mtPut;
  end;
end;
{$ENDIF}

function MatchRoute(const AText: string; const AValues: array of string): Boolean;

  function ReplaceParams(const AValue: string): string;
  var
    LPart: string;
    LSplitedPath: TArray<string>;
  begin
    Result := AValue;
    LSplitedPath := AValue.Split(['/']);
    for LPart in LSplitedPath do
    begin
      if LPart.StartsWith(':') then
        Result := StringReplace(Result, LPart, '([^/]*)', []);
    end;
    Result := Trim(Result);
    if not(Result.EndsWith('/')) then
      Result := Result + '/';
  end;

var
{$IF DEFINED(FPC)}
  LRegexObj: TRegExpr;
{$ENDIF}
  I: Integer;
  LText, LExpression: string;
begin
  Result := False;
{$IF DEFINED(FPC)}
  LRegexObj := TRegExpr.Create;
  try
{$ENDIF}
    LText := Trim(AText);
    if not(LText.EndsWith('/')) then
      LText := LText + '/';
    for I := Low(AValues) to High(AValues) do
    begin
      LExpression := '^(' + ReplaceParams(AValues[I]) + ')$';
{$IF DEFINED(FPC)}
      LRegexObj.Expression := '(?i)' + LExpression;
      if LRegexObj.Exec(LText) then
{$ELSE}
      if TRegEx.IsMatch(LText, LExpression, [roIgnoreCase]) then
{$ENDIF}
      begin
        Result := True;
        Exit;
      end;
    end;
{$IF DEFINED(FPC)}
  finally
    LRegexObj.Free;
  end;
{$ENDIF}
end;

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
    TLhsBracketsType.Contains:
      Result := '[contains]';
    TLhsBracketsType.StartsWith:
      Result := '[startsWith]';
    TLhsBracketsType.EndsWith:
      Result := '[endsWith]';
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
