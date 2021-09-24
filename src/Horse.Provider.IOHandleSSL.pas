unit Horse.Provider.IOHandleSSL;

interface

uses
  IdSSLOpenSSL;

type
  THorseProviderIOHandleSSL = class
  private
    FKeyFile: string;
    FRootCertFile: string;
    FCertFile: string;
    FMethod: TIdSSLVersion;
    FSSLVersions: TIdSSLVersions;
    FOnGetPassword: TPasswordEvent;
    FActive: Boolean;
    procedure SetCertFile(const Value: string);
    procedure SetKeyFile(const Value: string);
    procedure SetRootCertFile(const Value: string);
    procedure SetMethod(const Value: TIdSSLVersion);
    procedure SetSSLVersions(const Value: TIdSSLVersions);
    procedure SetOnGetPassword(const Value: TPasswordEvent);
    procedure SetActive(const Value: Boolean);
    function GetCertFile: string;
    function GetKeyFile: string;
    function GetRootCertFile: string;
    function GetMethod: TIdSSLVersion;
    function GetSSLVersions: TIdSSLVersions;
    function GetOnGetPassword: TPasswordEvent;
    function GetActive: Boolean;
  public
    constructor Create;
    property Active: Boolean read GetActive write SetActive default True;
    property CertFile: string read GetCertFile write SetCertFile;
    property RootCertFile: string read GetRootCertFile write SetRootCertFile;
    property KeyFile: string read GetKeyFile write SetKeyFile;
    property Method: TIdSSLVersion read GetMethod write SetMethod;
    property SSLVersions: TIdSSLVersions read GetSSLVersions write SetSSLVersions;
    property OnGetPassword: TPasswordEvent read GetOnGetPassword write SetOnGetPassword;
  end;

implementation

{ THorseProviderIOHandleSSL }

constructor THorseProviderIOHandleSSL.Create;
begin
  FActive := True;
  FMethod := DEF_SSLVERSION;
  FSSLVersions := DEF_SSLVERSIONS;
end;
function THorseProviderIOHandleSSL.GetActive: Boolean;
begin
  Result := FActive;
end;
function THorseProviderIOHandleSSL.GetCertFile: string;
begin
  Result := FCertFile;
end;
function THorseProviderIOHandleSSL.GetKeyFile: string;
begin
  Result := FKeyFile;
end;
function THorseProviderIOHandleSSL.GetOnGetPassword: TPasswordEvent;
begin
  Result := FOnGetPassword;
end;
function THorseProviderIOHandleSSL.GetRootCertFile: string;
begin
  Result := FRootCertFile;
end;
function THorseProviderIOHandleSSL.GetMethod: TIdSSLVersion;
begin
  Result := FMethod;
end;

function THorseProviderIOHandleSSL.GetSSLVersions: TIdSSLVersions;
begin
  Result := FSSLVersions;
end;
procedure THorseProviderIOHandleSSL.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;
procedure THorseProviderIOHandleSSL.SetCertFile(const Value: string);
begin
  FCertFile := Value;
end;
procedure THorseProviderIOHandleSSL.SetSSLVersions(const Value: TIdSSLVersions);
begin
  FSSLVersions := Value;
end;

procedure THorseProviderIOHandleSSL.SetMethod(const Value: TIdSSLVersion);
begin
  FMethod := Value;
end;
procedure THorseProviderIOHandleSSL.SetKeyFile(const Value: string);
begin
  FKeyFile := Value;
end;
procedure THorseProviderIOHandleSSL.SetOnGetPassword(const Value: TPasswordEvent);
begin
  FOnGetPassword := Value;
end;
procedure THorseProviderIOHandleSSL.SetRootCertFile(const Value: string);
begin
  FRootCertFile := Value;
end;

end.