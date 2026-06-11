unit Horse.BasicAuthentication;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  base64,
  Classes,
{$ELSE}
  System.SysUtils,
  System.NetEncoding,
  System.Classes,
{$ENDIF}
  Horse,
  Horse.Commons;

const
  AUTHORIZATION = 'Authorization';
  REALM_MESSAGE = 'Enter credentials';

type
  IHorseBasicAuthenticationConfig = interface
    ['{DB16765F-156C-4BC1-8EDE-183CA9FE1985}']
    function Header(const AValue: string): IHorseBasicAuthenticationConfig; overload;
    function Header: string; overload;
    function RealmMessage(const AValue: string): IHorseBasicAuthenticationConfig; overload;
    function RealmMessage: string; overload;
    function SkipRoutes(const AValues: TArray<string>): IHorseBasicAuthenticationConfig; overload;
    function SkipRoutes(const AValue: string): IHorseBasicAuthenticationConfig; overload;
    function SkipRoutes: TArray<string>; overload;
  end;

  THorseBasicAuthenticationConfig = class(TInterfacedObject, IHorseBasicAuthenticationConfig)
  private
    FHeader: string;
    FRealmMessage: string;
    FSkipRoutes: TArray<string>;
    function Header(const AValue: string): IHorseBasicAuthenticationConfig; overload;
    function Header: string; overload;
    function RealmMessage(const AValue: string): IHorseBasicAuthenticationConfig; overload;
    function RealmMessage: string; overload;
    function SkipRoutes(const AValues: TArray<string>): IHorseBasicAuthenticationConfig; overload;
    function SkipRoutes(const AValue: string): IHorseBasicAuthenticationConfig; overload;
    function SkipRoutes: TArray<string>; overload;
  public
    constructor Create;
    class function New: IHorseBasicAuthenticationConfig;
  end;

type
  THorseBasicAuthentication = {$IF NOT DEFINED(FPC)} reference to {$ENDIF} function(const AUsername, APassword: string): Boolean;

procedure Middleware(Req: THorseRequest; Res: THorseResponse; Next: {$IF DEFINED(FPC)} TNextProc {$ELSE} TProc {$ENDIF});
function HorseBasicAuthentication(const AAuthenticate: THorseBasicAuthentication): THorseCallback; overload;
function HorseBasicAuthentication(const AAuthenticate: THorseBasicAuthentication; const AConfig: IHorseBasicAuthenticationConfig): THorseCallback; overload;

implementation

var
  Config: IHorseBasicAuthenticationConfig;
  Authenticate: THorseBasicAuthentication;

function HorseBasicAuthentication(const AAuthenticate: THorseBasicAuthentication): THorseCallback;
begin
  Result := HorseBasicAuthentication(AAuthenticate, THorseBasicAuthenticationConfig.New);
end;

function HorseBasicAuthentication(const AAuthenticate: THorseBasicAuthentication; const AConfig: IHorseBasicAuthenticationConfig): THorseCallback;
begin
  Config := AConfig;
  Authenticate := AAuthenticate;
  Result := Middleware;
end;

procedure Middleware(Req: THorseRequest; Res: THorseResponse; Next: {$IF DEFINED(FPC)} TNextProc {$ELSE} TProc {$ENDIF});
const
  BASIC_AUTH = 'basic ';
var
  LBasicAuthenticationEncode: string;
  LBase64String: string;
  LBasicAuthenticationDecode: TStringList;
  LIsAuthenticated: Boolean;
  LPathInfo: string;
begin
  LPathInfo := Req.RawWebRequest.PathInfo;
  if LPathInfo = EmptyStr then
    LPathInfo := '/';
  if MatchRoute(LPathInfo, Config.SkipRoutes) then
  begin
    Next();
    Exit;
  end;

  LBasicAuthenticationEncode := Req.Headers[Config.Header];

  if LBasicAuthenticationEncode.Trim.IsEmpty and not Req.Query.TryGetValue(Config.Header, LBasicAuthenticationEncode) then
  begin
    Res.Send('Authorization not found').Status(THTTPStatus.Unauthorized).RawWebResponse
{$IF DEFINED(FPC)}
      .WWWAuthenticate := Format('Basic realm=%s', [Config.RealmMessage]);
{$ELSE}
      .Realm := Config.RealmMessage;
{$ENDIF}
    raise EHorseCallbackInterrupted.Create;
  end;

  if not LBasicAuthenticationEncode.Trim.ToLower.StartsWith(BASIC_AUTH) then
  begin
    Res.Send('Invalid authorization type').Status(THTTPStatus.Unauthorized);
    raise EHorseCallbackInterrupted.Create;
  end;

  LBasicAuthenticationDecode := TStringList.Create;
  try
    LBasicAuthenticationDecode.Delimiter := ':';
    LBasicAuthenticationDecode.StrictDelimiter := True;
    LBase64String := LBasicAuthenticationEncode.Trim.Replace(BASIC_AUTH, '', [rfIgnoreCase]);
    LBasicAuthenticationDecode.DelimitedText := {$IF DEFINED(FPC)}DecodeStringBase64(LBase64String){$ELSE}TBase64Encoding.base64.Decode(LBase64String){$ENDIF};

    try
      LIsAuthenticated := Authenticate(LBasicAuthenticationDecode.Strings[0], LBasicAuthenticationDecode.Strings[1]);
    except
      on E: exception do
      begin
        Res.Send(E.Message).Status(THTTPStatus.InternalServerError);
        raise EHorseCallbackInterrupted.Create;
      end;
    end;
  finally
    LBasicAuthenticationDecode.Free;
  end;

  if not LIsAuthenticated then
  begin
    Res.Send('Unauthorized').Status(THTTPStatus.Unauthorized);
    raise EHorseCallbackInterrupted.Create;
  end;

  Next();
end;

{ THorseBasicAuthenticationConfig }

constructor THorseBasicAuthenticationConfig.Create;
begin
  FHeader := AUTHORIZATION;
  FRealmMessage := REALM_MESSAGE;
  FSkipRoutes := [];
end;

function THorseBasicAuthenticationConfig.Header: string;
begin
  Result := FHeader;
end;

function THorseBasicAuthenticationConfig.Header(const AValue: string): IHorseBasicAuthenticationConfig;
begin
  FHeader := AValue;
  Result := Self;
end;

class function THorseBasicAuthenticationConfig.New: IHorseBasicAuthenticationConfig;
begin
  Result := THorseBasicAuthenticationConfig.Create;
end;

function THorseBasicAuthenticationConfig.RealmMessage(const AValue: string): IHorseBasicAuthenticationConfig;
begin
  FRealmMessage := AValue;
  Result := Self;
end;

function THorseBasicAuthenticationConfig.RealmMessage: string;
begin
  Result := FRealmMessage;
end;

function THorseBasicAuthenticationConfig.SkipRoutes(const AValue: string): IHorseBasicAuthenticationConfig;
begin
  Result := SkipRoutes([AValue]);
end;

function THorseBasicAuthenticationConfig.SkipRoutes(const AValues: TArray<string>): IHorseBasicAuthenticationConfig;
var
  I: Integer;
begin
  FSkipRoutes := AValues;
  for I := 0 to Pred(Length(FSkipRoutes)) do
    if Copy(Trim(FSkipRoutes[I]), 1, 1) <> '/' then
      FSkipRoutes[I] := '/' + FSkipRoutes[I];
  Result := Self;
end;

function THorseBasicAuthenticationConfig.SkipRoutes: TArray<string>;
begin
  Result := FSkipRoutes;
end;

end.
