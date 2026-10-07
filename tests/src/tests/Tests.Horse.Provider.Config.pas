unit Tests.Horse.Provider.Config;

interface

uses
  DUnitX.TestFramework;

type
  // THorseCrossSocketConfig is shared by the CrossSocket and nghttp2
  // providers. These tests pin what Default promises for the TLS fields, so a
  // provider that reads them can rely on "empty / htvDefault = configure
  // nothing" - in particular that adding SSLCipherSuitesTLS13 and
  // SSLMinVersion changed nothing for callers who never set them.
  [TestFixture]
  TTestHorseProviderConfig = class
  public
    [Test]
    procedure DefaultLeavesTls13CipherSuitesEmpty;
    [Test]
    procedure DefaultLeavesMinVersionAtLibraryDefault;
    [Test]
    procedure LibraryDefaultIsOrdinalZero;
    [Test]
    procedure DefaultLeavesExistingTlsFieldsUnchanged;
    [Test]
    procedure DefaultDoesNotRequestUnsupportedTls;
    [Test]
    [TestCase('Enabled', '0,SSLEnabled')]
    [TestCase('Certificate', '1,SSLCertFile')]
    [TestCase('Key', '2,SSLKeyFile')]
    [TestCase('Password', '3,SSLKeyPassword')]
    [TestCase('CA', '4,SSLCACertFile')]
    [TestCase('VerifyPeer', '5,SSLVerifyPeer')]
    [TestCase('CipherList', '6,SSLCipherList')]
    [TestCase('Tls13Suites', '7,SSLCipherSuitesTLS13')]
    [TestCase('Tls12Minimum', '8,SSLMinVersion')]
    [TestCase('Tls13Minimum', '9,SSLMinVersion')]
    procedure UnsupportedTlsIsRejected(const ACase: Integer; const AField: string);
    [Test]
    procedure ActiveProviderRejectsTlsBeforeChangingPort;
  end;

implementation

uses
  System.SysUtils, Horse.Provider.Config, Horse;

procedure TTestHorseProviderConfig.ActiveProviderRejectsTlsBeforeChangingPort;
var
  LConfig: THorseCrossSocketConfig;
  LPort: Integer;
  LRaised: Boolean;
begin
  LConfig := THorseCrossSocketConfig.Default;
  LConfig.SSLEnabled := True;
  LPort := THorse.Port;
  LRaised := False;
  try
    THorse.ListenWithConfig(9132, LConfig);
  except
    on E: Exception do
    begin
      LRaised := True;
      Assert.IsTrue(Pos('SSLEnabled', E.Message) > 0, E.Message);
    end;
  end;
  Assert.IsTrue(LRaised, 'Unsupported TLS must never silently start HTTP');
  Assert.AreEqual(LPort, THorse.Port, 'Validation must precede port mutation');
end;

procedure TTestHorseProviderConfig.DefaultDoesNotRequestUnsupportedTls;
begin
  ValidateNoUnsupportedTls(THorseCrossSocketConfig.Default, 'TestProvider');
  Assert.Pass;
end;

procedure TTestHorseProviderConfig.UnsupportedTlsIsRejected(const ACase: Integer;
  const AField: string);
var
  LConfig: THorseCrossSocketConfig;
begin
  LConfig := THorseCrossSocketConfig.Default;
  case ACase of
    0: LConfig.SSLEnabled := True;
    1: LConfig.SSLCertFile := 'cert.pem';
    2: LConfig.SSLKeyFile := 'key.pem';
    3: LConfig.SSLKeyPassword := 'password';
    4: LConfig.SSLCACertFile := 'ca.pem';
    5: LConfig.SSLVerifyPeer := True;
    6: LConfig.SSLCipherList := 'HIGH';
    7: LConfig.SSLCipherSuitesTLS13 := 'TLS_AES_256_GCM_SHA384';
    8: LConfig.SSLMinVersion := htvTLS12;
    9: LConfig.SSLMinVersion := htvTLS13;
  end;
  Assert.WillRaiseWithMessage(
    procedure
    begin
      ValidateNoUnsupportedTls(LConfig, 'TestProvider');
    end,
    Exception, 'TestProvider does not support ' + AField + ' in ListenWithConfig. ' +
      'Use the provider-specific TLS configuration or a TLS-capable provider.');
end;

procedure TTestHorseProviderConfig.DefaultLeavesTls13CipherSuitesEmpty;
begin
  Assert.AreEqual('', THorseCrossSocketConfig.Default.SSLCipherSuitesTLS13);
end;

procedure TTestHorseProviderConfig.DefaultLeavesMinVersionAtLibraryDefault;
begin
  Assert.IsTrue(THorseCrossSocketConfig.Default.SSLMinVersion = htvDefault,
    'Default must not impose a minimum TLS version');
end;

procedure TTestHorseProviderConfig.LibraryDefaultIsOrdinalZero;
begin
  // A record that is zero-initialised instead of built with Default (a
  // class field, a global) must still mean "configure nothing".
  Assert.AreEqual(0, Ord(htvDefault));
end;

procedure TTestHorseProviderConfig.DefaultLeavesExistingTlsFieldsUnchanged;
var
  LConfig: THorseCrossSocketConfig;
begin
  LConfig := THorseCrossSocketConfig.Default;
  Assert.IsFalse(LConfig.SSLEnabled);
  Assert.IsFalse(LConfig.SSLVerifyPeer);
  Assert.AreEqual('', LConfig.SSLCipherList);
  Assert.AreEqual('', LConfig.SSLCACertFile);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseProviderConfig);

end.
