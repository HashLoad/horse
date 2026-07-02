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
  Web.HTTPApp,
  System.SysUtils;
{$ENDIF}

type
  TMethodType = (mtAny, mtGet, mtPut, mtPost, mtHead, mtDelete, mtPatch, mtQuery);

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

{$IF DEFINED(FPC)}
  THorseBufferSlice = record
  private
    FBuffer: TBytes;
    FStart: Integer;
    FLen: Integer;
  public
    class function Create(const ABuffer: TBytes; const AStart, ALen: Integer): THorseBufferSlice; static;
    class function Empty: THorseBufferSlice; static;
    function ToString: string;
    function Compare(const AText: string; ACaseInsensitive: Boolean = True): Boolean; overload;
    function Compare(const AOther: THorseBufferSlice; ACaseInsensitive: Boolean = True): Boolean; overload;
    function CompareBytes(const ABytes: TBytes; const AStart, ALen: Integer; ACaseInsensitive: Boolean = True): Boolean;
    function SubSlice(const AStartOffset, ALen: Integer): THorseBufferSlice;
    function IndexOf(const AByte: Byte; const AOffset: Integer = 0): Integer;
    property Buffer: TBytes read FBuffer;
    property Start: Integer read FStart;
    property Len: Integer read FLen;
    function IsEmpty: Boolean;
  end;
{$ELSE}
  THorseBufferSlice = record
  private
    FBuffer: TBytes;
    FStart: Integer;
    FLen: Integer;
    function GetIsEmpty: Boolean; inline;
  public
    class function Create(const ABuffer: TBytes; const AStart, ALen: Integer): THorseBufferSlice; static;
    class function Empty: THorseBufferSlice; static;
    function ToString: string;
    function Compare(const AText: string; ACaseInsensitive: Boolean = True): Boolean; overload;
    function Compare(const AOther: THorseBufferSlice; ACaseInsensitive: Boolean = True): Boolean; overload;
    function CompareBytes(const ABytes: TBytes; const AStart, ALen: Integer; ACaseInsensitive: Boolean = True): Boolean;
    function SubSlice(const AStartOffset, ALen: Integer): THorseBufferSlice;
    function IndexOf(const AByte: Byte; const AOffset: Integer = 0): Integer;
    property Buffer: TBytes read FBuffer;
    property Start: Integer read FStart;
    property Len: Integer read FLen;
    property IsEmpty: Boolean read GetIsEmpty;
  end;
{$ENDIF}

  THorseArenaAllocator = class
  private
    FBuffer: TBytes;
    FOffset: Integer;
    FSize: Integer;
  public
    constructor Create(ASize: Integer);
    destructor Destroy; override;
    function Allocate(ALen: Integer): THorseBufferSlice;
    procedure Reset;
  end;

  THTTPStatusHelper = {$IF DEFINED(FPC)} type {$ELSE} record {$ENDIF} helper for THTTPStatus
    function ToInteger: Integer;
  end;

  TMethodTypeHelper = {$IF DEFINED(FPC)} type {$ELSE} record {$ENDIF} helper for TMethodType
    class function FromString(const AMethod: string): TMethodType; static;
    function ToString: string;
  end;

  TMimeTypesHelper = {$IF DEFINED(FPC)} type {$ELSE} record {$ENDIF} helper for TMimeTypes
    function ToString: string;
  end;

  TLhsBracketsTypeHelper = {$IF DEFINED(FPC)} type {$ELSE} record {$ENDIF} helper for TLhsBracketsType
    function ToString: string;
  end;



function MatchRoute(const AText: string; const AValues: array of string): Boolean;

implementation

uses  
{$IF DEFINED(FPC)}
  RegExpr;
{$ELSE}
  System.RegularExpressions;    
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
    if not Result.EndsWith('/') then
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
    if not LText.EndsWith('/') then
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

{ TMethodTypeHelper }

class function TMethodTypeHelper.FromString(const AMethod: string): TMethodType;
var
  LMethod: string;
begin
  Result := mtAny;
  LMethod := UpperCase(AMethod);
  if LMethod = 'GET' then
    Result := mtGet
  else if LMethod = 'POST' then
    Result := mtPost
  else if LMethod = 'PUT' then
    Result := mtPut
  else if LMethod = 'PATCH' then
    Result := mtPatch
  else if LMethod = 'DELETE' then
    Result := mtDelete
  else if LMethod = 'HEAD' then
    Result := mtHead
  else if LMethod = 'QUERY' then
    Result := mtQuery;
end;

function TMethodTypeHelper.ToString: string;
begin
  case Self of
    mtAny:
      Result := 'Any';
    mtGet:
      Result := 'Get';
    mtPut:
      Result := 'Put';
    mtPost:
      Result := 'Post';
    mtHead:
      Result := 'Head';
    mtDelete:
      Result := 'Delete';
    mtPatch:
      Result := 'Patch';
    mtQuery:
      Result := 'Query';
  end;
end;

{ THorseBufferSlice }

{$IF DEFINED(FPC)}
class function THorseBufferSlice.Create(const ABuffer: TBytes; const AStart, ALen: Integer): THorseBufferSlice;
begin
  Result.FBuffer := ABuffer;
  Result.FStart := AStart;
  Result.FLen := ALen;
end;

class function THorseBufferSlice.Empty: THorseBufferSlice;
begin
  Result.FBuffer := nil;
  Result.FStart := 0;
  Result.FLen := 0;
end;

function THorseBufferSlice.ToString: string;
begin
  if FLen <= 0 then
    Result := ''
  else
    Result := TEncoding.UTF8.GetString(FBuffer, FStart, FLen);
end;

function THorseBufferSlice.Compare(const AText: string; ACaseInsensitive: Boolean): Boolean;
var
  I: Integer;
  B1, B2: Byte;
begin
  if Length(AText) <> FLen then
    Exit(False);
  if FLen = 0 then
    Exit(True);
  for I := 0 to FLen - 1 do
  begin
    B1 := FBuffer[FStart + I];
    B2 := Ord(AText[I + 1]);
    if ACaseInsensitive then
    begin
      if (B1 >= 65) and (B1 <= 90) then B1 := B1 + 32;
      if (B2 >= 65) and (B2 <= 90) then B2 := B2 + 32;
    end;
    if B1 <> B2 then
      Exit(False);
  end;
  Result := True;
end;

function THorseBufferSlice.Compare(const AOther: THorseBufferSlice; ACaseInsensitive: Boolean): Boolean;
var
  I: Integer;
  B1, B2: Byte;
begin
  if AOther.FLen <> FLen then
    Exit(False);
  if FLen = 0 then
    Exit(True);
  for I := 0 to FLen - 1 do
  begin
    B1 := FBuffer[FStart + I];
    B2 := AOther.FBuffer[AOther.FStart + I];
    if ACaseInsensitive then
    begin
      if (B1 >= 65) and (B1 <= 90) then B1 := B1 + 32;
      if (B2 >= 65) and (B2 <= 90) then B2 := B2 + 32;
    end;
    if B1 <> B2 then
      Exit(False);
  end;
  Result := True;
end;

function THorseBufferSlice.CompareBytes(const ABytes: TBytes; const AStart, ALen: Integer; ACaseInsensitive: Boolean): Boolean;
var
  I: Integer;
  B1, B2: Byte;
begin
  if ALen <> FLen then
    Exit(False);
  if FLen = 0 then
    Exit(True);
  for I := 0 to FLen - 1 do
  begin
    B1 := FBuffer[FStart + I];
    B2 := ABytes[AStart + I];
    if ACaseInsensitive then
    begin
      if (B1 >= 65) and (B1 <= 90) then B1 := B1 + 32;
      if (B2 >= 65) and (B2 <= 90) then B2 := B2 + 32;
    end;
    if B1 <> B2 then
      Exit(False);
  end;
  Result := True;
end;

function THorseBufferSlice.SubSlice(const AStartOffset, ALen: Integer): THorseBufferSlice;
begin
  if (AStartOffset < 0) or (AStartOffset + ALen > FLen) then
    raise ERangeError.Create('SubSlice range index out of bounds');
  Result.FBuffer := FBuffer;
  Result.FStart := FStart + AStartOffset;
  Result.FLen := ALen;
end;

function THorseBufferSlice.IndexOf(const AByte: Byte; const AOffset: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := AOffset to FLen - 1 do
  begin
    if FBuffer[FStart + I] = AByte then
      Exit(I);
  end;
end;

function THorseBufferSlice.IsEmpty: Boolean;
begin
  Result := (FLen <= 0) or (FBuffer = nil);
end;
{$ELSE}
class function THorseBufferSlice.Create(const ABuffer: TBytes; const AStart, ALen: Integer): THorseBufferSlice;
begin
  Result.FBuffer := ABuffer;
  Result.FStart := AStart;
  Result.FLen := ALen;
end;

class function THorseBufferSlice.Empty: THorseBufferSlice;
begin
  Result.FBuffer := nil;
  Result.FStart := 0;
  Result.FLen := 0;
end;

function THorseBufferSlice.GetIsEmpty: Boolean;
begin
  Result := (FLen <= 0) or (FBuffer = nil);
end;

function THorseBufferSlice.ToString: string;
begin
  if FLen <= 0 then
    Result := ''
  else
    Result := TEncoding.UTF8.GetString(FBuffer, FStart, FLen);
end;

function THorseBufferSlice.Compare(const AText: string; ACaseInsensitive: Boolean): Boolean;
var
  I: Integer;
  B1, B2: Byte;
begin
  if Length(AText) <> FLen then
    Exit(False);
  if FLen = 0 then
    Exit(True);
  for I := 0 to FLen - 1 do
  begin
    B1 := FBuffer[FStart + I];
    B2 := Ord(AText[I + 1]);
    if ACaseInsensitive then
    begin
      if (B1 >= 65) and (B1 <= 90) then B1 := B1 + 32;
      if (B2 >= 65) and (B2 <= 90) then B2 := B2 + 32;
    end;
    if B1 <> B2 then
      Exit(False);
  end;
  Result := True;
end;

function THorseBufferSlice.Compare(const AOther: THorseBufferSlice; ACaseInsensitive: Boolean): Boolean;
var
  I: Integer;
  B1, B2: Byte;
begin
  if AOther.FLen <> FLen then
    Exit(False);
  if FLen = 0 then
    Exit(True);
  for I := 0 to FLen - 1 do
  begin
    B1 := FBuffer[FStart + I];
    B2 := AOther.FBuffer[AOther.FStart + I];
    if ACaseInsensitive then
    begin
      if (B1 >= 65) and (B1 <= 90) then B1 := B1 + 32;
      if (B2 >= 65) and (B2 <= 90) then B2 := B2 + 32;
    end;
    if B1 <> B2 then
      Exit(False);
  end;
  Result := True;
end;

function THorseBufferSlice.CompareBytes(const ABytes: TBytes; const AStart, ALen: Integer; ACaseInsensitive: Boolean): Boolean;
var
  I: Integer;
  B1, B2: Byte;
begin
  if ALen <> FLen then
    Exit(False);
  if FLen = 0 then
    Exit(True);
  for I := 0 to FLen - 1 do
  begin
    B1 := FBuffer[FStart + I];
    B2 := ABytes[AStart + I];
    if ACaseInsensitive then
    begin
      if (B1 >= 65) and (B1 <= 90) then B1 := B1 + 32;
      if (B2 >= 65) and (B2 <= 90) then B2 := B2 + 32;
    end;
    if B1 <> B2 then
      Exit(False);
  end;
  Result := True;
end;

function THorseBufferSlice.SubSlice(const AStartOffset, ALen: Integer): THorseBufferSlice;
begin
  if (AStartOffset < 0) or (AStartOffset + ALen > FLen) then
    raise ERangeError.Create('SubSlice range index out of bounds');
  Result.FBuffer := FBuffer;
  Result.FStart := FStart + AStartOffset;
  Result.FLen := ALen;
end;

function THorseBufferSlice.IndexOf(const AByte: Byte; const AOffset: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := AOffset to FLen - 1 do
  begin
    if FBuffer[FStart + I] = AByte then
      Exit(I);
  end;
end;
{$ENDIF}

{ THorseArenaAllocator }

constructor THorseArenaAllocator.Create(ASize: Integer);
begin
  inherited Create;
  FSize := ASize;
  SetLength(FBuffer, FSize);
  FOffset := 0;
end;

destructor THorseArenaAllocator.Destroy;
begin
  FBuffer := nil;
  inherited;
end;

function THorseArenaAllocator.Allocate(ALen: Integer): THorseBufferSlice;
begin
  if FOffset + ALen > FSize then
  begin
    FSize := (FOffset + ALen) * 2;
    SetLength(FBuffer, FSize);
  end;
  Result := THorseBufferSlice.Create(FBuffer, FOffset, ALen);
  FOffset := FOffset + ALen;
end;

procedure THorseArenaAllocator.Reset;
begin
  FOffset := 0;
end;

end.
