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
    FCipherList: string;
    FDHParamsFile: string;
    FOnGetPassword: TPasswordEvent;
    FActive: Boolean;
    procedure SetCertFile(const AValue: string);
    procedure SetKeyFile(const AValue: string);
    procedure SetRootCertFile(const AValue: string);
    procedure SetMethod(const AValue: TIdSSLVersion);
    procedure SetSSLVersions(const AValue: TIdSSLVersions);
    procedure SetOnGetPassword(const AValue: TPasswordEvent);
    procedure SetActive(const AValue: Boolean);
    procedure SetCipherList(const AValue: string);
    procedure SetDHParamsFile(const AValue: string);
    function GetCertFile: string;
    function GetKeyFile: string;
    function GetRootCertFile: string;
    function GetMethod: TIdSSLVersion;
    function GetSSLVersions: TIdSSLVersions;
    function GetOnGetPassword: TPasswordEvent;
    function GetActive: Boolean;
    function GetCipherList: string;
    function GetDHParamsFile: string;
    public
    constructor Create;
    property Active: Boolean read GetActive write SetActive default True;
    property CertFile: string read GetCertFile write SetCertFile;
    property RootCertFile: string read GetRootCertFile write SetRootCertFile;
    property KeyFile: string read GetKeyFile write SetKeyFile;
    property Method: TIdSSLVersion read GetMethod write SetMethod;
    property SSLVersions: TIdSSLVersions read GetSSLVersions write SetSSLVersions;
    /// <summary> A cipher suite is a set of cryptographic algorithms.
    /// The schannel SSP implementation of the TLS/SSL protocols use algorithms
    /// from a cipher suite to create keys and encrypt information.
    /// <see cref="https://learn.microsoft.com/en-us/windows/win32/secauthn/cipher-suites-in-schannel"/>
    /// </summary>
    property CipherList: string read GetCipherList write SetCipherList;
    /// <summary> Diffie–Hellman key exchange[nb 1] is a method of securely 
	/// exchanging cryptographic keys over a public channel and was one of 
	/// the first public-key protocols as conceived by Ralph Merkle and named 
	/// after Whitfield Diffie and Martin Hellman.[1][2] DH is one of the 
	/// earliest practical examples of public key exchange implemented within 
	/// the field of cryptography
    /// <see cref="https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange"/>
    /// </summary>
	property DHParamsFile: string read GetDHParamsFile write SetDHParamsFile;
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

function THorseProviderIOHandleSSL.GetCipherList: string;
begin
  Result := FCipherList;
end;

function THorseProviderIOHandleSSL.GetDHParamsFile: string;
begin
  Result := FDHParamsFile;
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

procedure THorseProviderIOHandleSSL.SetCipherList(const AValue: string);
begin
  FCipherList := AValue;
end;

procedure THorseProviderIOHandleSSL.SetDHParamsFile(const AValue: string);
begin
  FDHParamsFile := AValue;
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