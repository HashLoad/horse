unit Horse.Provider.IOHandleSSL;

interface

uses IdSSLOpenSSL;

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
    procedure SetCertFile(const AValue: string);
    procedure SetKeyFile(const AValue: string);
    procedure SetRootCertFile(const AValue: string);
    procedure SetMethod(const AValue: TIdSSLVersion);
    procedure SetSSLVersions(const AValue: TIdSSLVersions);
    procedure SetOnGetPassword(const AValue: TPasswordEvent);
    procedure SetActive(const AValue: Boolean);
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

procedure THorseProviderIOHandleSSL.SetActive(const AValue: Boolean);
begin
  FActive := AValue;
end;

procedure THorseProviderIOHandleSSL.SetCertFile(const AValue: string);
begin
  FCertFile := AValue;
end;

procedure THorseProviderIOHandleSSL.SetSSLVersions(const AValue: TIdSSLVersions);
begin
  FSSLVersions := AValue;
end;

procedure THorseProviderIOHandleSSL.SetMethod(const AValue: TIdSSLVersion);
begin
  FMethod := AValue;
end;

procedure THorseProviderIOHandleSSL.SetKeyFile(const AValue: string);
begin
  FKeyFile := AValue;
end;

procedure THorseProviderIOHandleSSL.SetOnGetPassword(const AValue: TPasswordEvent);
begin
  FOnGetPassword := AValue;
end;

procedure THorseProviderIOHandleSSL.SetRootCertFile(const AValue: string);
begin
  FRootCertFile := AValue;
end;

end.