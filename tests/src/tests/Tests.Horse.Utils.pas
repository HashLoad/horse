unit Tests.Horse.Utils;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestHorseUtils = class
  public
    [Test]
    [TestCase('TrailingPercent', '100%,100%')]
    [TestCase('IncompleteTriplet', 'value%2,value%2')]
    [TestCase('NonHexTriplet', '50%off,50%off')]
    [TestCase('PercentOnly', '%,%')]
    [TestCase('MixedValidAndInvalid', '%41%,%41%')]
    procedure InvalidPercentEncodingIsPreserved(const AInput, AExpected: string);

    [Test]
    [TestCase('EncodedPercent', '100%25,100%')]
    [TestCase('EncodedSpace', 'hello%20world,hello world')]
    [TestCase('LowercaseHex', '%2fapi,/api')]
    procedure ValidPercentEncodingIsDecoded(const AInput, AExpected: string);

    [Test]
    procedure ValueWithoutPercentIsUnchanged;
  end;

implementation

uses
  Horse.Utils;

procedure TTestHorseUtils.InvalidPercentEncodingIsPreserved(const AInput,
  AExpected: string);
begin
  Assert.AreEqual(AExpected, DecodeParam(AInput));
end;

procedure TTestHorseUtils.ValidPercentEncodingIsDecoded(const AInput,
  AExpected: string);
begin
  Assert.AreEqual(AExpected, DecodeParam(AInput));
end;

procedure TTestHorseUtils.ValueWithoutPercentIsUnchanged;
begin
  Assert.AreEqual('a+b c', DecodeParam('a+b c'));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseUtils);

end.
