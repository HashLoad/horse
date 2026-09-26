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

    [Test]
    [TestCase('PlusBecomesSpace', 'a+b,a b')]
    [TestCase('MultiplePlus', 'Jo+da+Silva,Jo da Silva')]
    [TestCase('EncodedPlusStaysPlus', 'a%2Bb,a+b')]
    [TestCase('EncodedSpace', 'hello%20world,hello world')]
    [TestCase('NoSpecialChars', 'abc,abc')]
    [TestCase('InvalidPercentIsPreserved', '100%,100%')]
    [TestCase('PlusDecodedWithInvalidPercentPreserved', 'a+b%,a b%')]
    procedure QueryParamDecodesPlusAsSpace(const AInput, AExpected: string);

    [Test]
    procedure QueryParamDecodesUtf8AndPlusTogether;

    [Test]
    procedure QueryParamDecodeDoesNotChangeRouteParamDecode;
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

procedure TTestHorseUtils.QueryParamDecodesPlusAsSpace(const AInput,
  AExpected: string);
begin
  Assert.AreEqual(AExpected, DecodeQueryParam(AInput));
end;

procedure TTestHorseUtils.QueryParamDecodesUtf8AndPlusTogether;
begin
  // %C3%A3 is U+00E3; written as a char code to stay independent of file encoding.
  Assert.AreEqual('Jo' + #$00E3 + 'o Silva', DecodeQueryParam('Jo%C3%A3o+Silva'));
end;

procedure TTestHorseUtils.QueryParamDecodeDoesNotChangeRouteParamDecode;
begin
  // '+' in a route segment is a literal plus; only query strings map it to space.
  Assert.AreEqual('a+b', DecodeParam('a+b'));
  Assert.AreEqual('a b', DecodeQueryParam('a+b'));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseUtils);

end.
