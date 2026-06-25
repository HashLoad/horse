unit Horse.Core.Cookie;

{
  Horse — RFC 6265 Set-Cookie builder
  ===================================
  Provider-agnostic typed cookie used by THorseResponse.AddCookie / .Cookie.
  ToHeaderValue produces an RFC 6265 §4.1-compliant Set-Cookie *value* (without
  the "Set-Cookie:" prefix); each provider's response bridge emits one header
  line per cookie.

  Validation (RFC 6265 §4.1.1): the cookie name must be a token (no controls /
  separators); name and value must not contain control chars, CR, LF or ';'
  (anti-injection). Invalid input raises EHorseException. This validation lives
  ONLY here (the opt-in API) — it is never retrofitted onto Res.AddHeader.

  Dates: Expires is emitted as a fixed-locale IMF-fixdate
  ("Sun, 06 Nov 1994 08:49:37 GMT") — never DateTimeToStr (locale-dependent).
  The TDateTime passed to Expires(...) is treated as UTC.

  Dual-compilation: Delphi and Lazarus/FPC.
}

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
{$ELSE}
  System.SysUtils,
{$ENDIF}
  Horse.Exception;

type
  TSameSite = (ssNotSet, ssStrict, ssLax, ssNone);

  { Read-only snapshot of a cookie's fields, used by the Indy response path to
    map onto Web.HTTPApp.TCookie without exposing getters that would clash with
    the fluent setter method names (Path/HttpOnly/...).  Note: TCookie has no
    Max-Age, so the Indy mapper uses HasExpires/Expires only — see ToHeaderValue
    (CrossSocket/mORMot) for full Max-Age fidelity. }
  THorseCookieState = record
    Name:       string;
    Value:      string;
    Domain:     string;
    Path:       string;
    HasExpires: Boolean;
    Expires:    TDateTime;
    Secure:     Boolean;
    HttpOnly:   Boolean;
    SameSite:   TSameSite;
  end;

  THorseCookie = class
  private
    FName:       string;
    FValue:      string;
    FDomain:     string;
    FPath:       string;
    FExpires:    TDateTime;
    FHasExpires: Boolean;
    FMaxAge:     Integer;
    FHasMaxAge:  Boolean;
    FSecure:     Boolean;
    FHttpOnly:   Boolean;
    FSameSite:   TSameSite;
    procedure ValidateName(const AName: string);
    function  CheckedValue(const AValue, AWhat: string): string;
  public
    constructor Create(const AName: string = ''; const AValue: string = '');

    { Fluent attribute setters — return Self for chaining. }
    function Value(const AValue: string): THorseCookie;
    function Domain(const AValue: string): THorseCookie;
    function Path(const AValue: string): THorseCookie;
    function Expires(const AValue: TDateTime): THorseCookie;
    function MaxAge(const AValue: Integer): THorseCookie;
    function Secure(const AValue: Boolean = True): THorseCookie;
    function HttpOnly(const AValue: Boolean = True): THorseCookie;
    function SameSite(const AValue: TSameSite): THorseCookie;

    { RFC 6265 §4.1 Set-Cookie value (no "Set-Cookie:" prefix). }
    function ToHeaderValue: string;

    { Read-only field snapshot (for the Indy TCookie mapper). }
    function State: THorseCookieState;

    property Name:  string read FName;
  end;

implementation

const
  IMF_DAYS: array[1..7] of string =
    ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');   // DayOfWeek: 1=Sunday
  IMF_MONTHS: array[1..12] of string =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  { RFC 2616 separators that may not appear in a cookie-name token. }
  COOKIE_NAME_SEPARATORS = '()<>@,;:\"/[]?={} ' + #9;

function DateTimeToIMF(const ADateTime: TDateTime): string;
var
  Y, M, D, H, Mi, S, Ms: Word;
  LDow: Integer;
begin
  DecodeDate(ADateTime, Y, M, D);
  DecodeTime(ADateTime, H, Mi, S, Ms);
  LDow := DayOfWeek(ADateTime);   // 1 = Sunday .. 7 = Saturday
  Result := Format('%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT',
    [IMF_DAYS[LDow], D, IMF_MONTHS[M], Y, H, Mi, S]);
end;

{ THorseCookie }

constructor THorseCookie.Create(const AName, AValue: string);
begin
  inherited Create;
  FExpires    := 0;
  FHasExpires := False;
  FMaxAge     := 0;
  FHasMaxAge  := False;
  FSecure     := False;
  FHttpOnly   := False;
  FSameSite   := ssNotSet;
  if AName <> '' then
  begin
    ValidateName(AName);
    FName := AName;
  end;
  FValue := CheckedValue(AValue, 'value');
end;

procedure THorseCookie.ValidateName(const AName: string);
var
  C: Char;
begin
  if AName = '' then
    raise EHorseException.Create('Cookie name must not be empty');
  for C in AName do
    if (C <= #31) or (C = #127) or (Pos(C, COOKIE_NAME_SEPARATORS) > 0) then
      raise EHorseException.CreateFmt(
        'Invalid character in cookie name "%s" (RFC 6265 token)', [AName]);
end;

{ Reject control chars / CR / LF / ';' in attribute values (anti-injection);
  return the value unchanged when valid. AWhat names the field for the message. }
function THorseCookie.CheckedValue(const AValue, AWhat: string): string;
var
  C: Char;
begin
  for C in AValue do
    if (C < #32) or (C = #127) or (C = ';') then
      raise EHorseException.CreateFmt(
        'Invalid character in cookie %s (control char or ";")', [AWhat]);
  Result := AValue;
end;

function THorseCookie.Value(const AValue: string): THorseCookie;
begin
  FValue := CheckedValue(AValue, 'value');
  Result := Self;
end;

function THorseCookie.Domain(const AValue: string): THorseCookie;
begin
  FDomain := CheckedValue(AValue, 'Domain');
  Result := Self;
end;

function THorseCookie.Path(const AValue: string): THorseCookie;
begin
  FPath := CheckedValue(AValue, 'Path');
  Result := Self;
end;

function THorseCookie.Expires(const AValue: TDateTime): THorseCookie;
begin
  FExpires    := AValue;
  FHasExpires := True;
  Result := Self;
end;

function THorseCookie.MaxAge(const AValue: Integer): THorseCookie;
begin
  FMaxAge    := AValue;
  FHasMaxAge := True;
  Result := Self;
end;

function THorseCookie.Secure(const AValue: Boolean): THorseCookie;
begin
  FSecure := AValue;
  Result := Self;
end;

function THorseCookie.HttpOnly(const AValue: Boolean): THorseCookie;
begin
  FHttpOnly := AValue;
  Result := Self;
end;

function THorseCookie.SameSite(const AValue: TSameSite): THorseCookie;
begin
  FSameSite := AValue;
  Result := Self;
end;

function THorseCookie.ToHeaderValue: string;
begin
  if FName = '' then
    raise EHorseException.Create('Cookie name must be set before ToHeaderValue');

  Result := FName + '=' + FValue;

  if FPath <> '' then
    Result := Result + '; Path=' + FPath;
  if FDomain <> '' then
    Result := Result + '; Domain=' + FDomain;
  if FHasExpires then
    Result := Result + '; Expires=' + DateTimeToIMF(FExpires);
  if FHasMaxAge then
    Result := Result + '; Max-Age=' + IntToStr(FMaxAge);
  if FSecure then
    Result := Result + '; Secure';
  if FHttpOnly then
    Result := Result + '; HttpOnly';
  case FSameSite of
    ssStrict: Result := Result + '; SameSite=Strict';
    ssLax:    Result := Result + '; SameSite=Lax';
    ssNone:   Result := Result + '; SameSite=None';
  end;
end;

function THorseCookie.State: THorseCookieState;
begin
  Result.Name       := FName;
  Result.Value      := FValue;
  Result.Domain     := FDomain;
  Result.Path       := FPath;
  Result.HasExpires := FHasExpires;
  Result.Expires    := FExpires;
  Result.Secure     := FSecure;
  Result.HttpOnly   := FHttpOnly;
  Result.SameSite   := FSameSite;
end;

end.
