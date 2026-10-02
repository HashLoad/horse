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
  end;

implementation

uses
  Horse.Provider.Config;

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
