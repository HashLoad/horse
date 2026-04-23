unit Horse.Commons;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
  {$MODESWITCH TypeHelpers}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  StrUtils;
{$ELSE}
  System.SysUtils;
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

implementation

uses  
{$IF DEFINED(FPC)}
  RegExpr;
{$ELSE}
  System.RegularExpressions;    
{$ENDIF}

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

  function MatchRouteOptimized(const APath, APattern: string): Boolean;
  var
    PathSegments, PatternSegments: TArray<string>;
    I, PathLen, PatternLen: Integer;
    TrimmedPath, TrimmedPattern: string;
  begin
    Result := False;

    // 1. Normaliza e divide os caminhos em segmentos
    TrimmedPath := APath;
    if TrimmedPath.StartsWith('/') then
      TrimmedPath := TrimmedPath.Substring(1);
    if TrimmedPath.EndsWith('/') then
      TrimmedPath := TrimmedPath.Substring(0, TrimmedPath.Length - 1);

    TrimmedPattern := APattern;
    if TrimmedPattern.StartsWith('/') then
      TrimmedPattern := TrimmedPattern.Substring(1);
    if TrimmedPattern.EndsWith('/') then
      TrimmedPattern := TrimmedPattern.Substring(0, TrimmedPattern.Length - 1);

    PathSegments := TrimmedPath.Split(['/']);
    PatternSegments := TrimmedPattern.Split(['/']);

    PathLen := Length(PathSegments);
    PatternLen := Length(PatternSegments);

    // 2. Regra de Falha Rápida: Se o número de segmentos não for igual, não há match.
    if PathLen <> PatternLen then
      Exit;

    // 3. Caso especial: rota raiz "/" (quando os paths vazios resultam em um array com um elemento vazio)
    if (PathLen = 1) and (PatternLen = 1) and (PathSegments[0] = '') and (PatternSegments[0] = '') then
    begin
      Result := True;
      Exit;
    end;

    // 4. Comparação segmento por segmento
    for I := 0 to PathLen - 1 do
    begin
      // Se o segmento do padrão não começa com ':', ele deve ser idêntico (case-insensitive)
      if not PatternSegments[I].StartsWith(':') then
      begin
        if AnsiCompareText(PathSegments[I], PatternSegments[I]) <> 0 then
          Exit; // Falha se segmentos estáticos não baterem
      end;
      // Se o segmento do padrão começa com ':', ele é um wildcard e aceita qualquer valor.
    end;

    // Se o loop terminou sem falhas, a rota corresponde.
    Result := True;
  end;

var
  LPattern: string;
begin
  for LPattern in AValues do
  begin
    if MatchRouteOptimized(AText, LPattern) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
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
