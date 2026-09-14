unit Tests.Horse.Request.DecodeOnce;

// Query and ContentFields values must be URL-decoded exactly once.
//
// Every path that fills these collections stores values that are ALREADY
// decoded: InitializeQuery runs DecodeParam on each key and value, WebBroker's
// ContentFields are decoded by ExtractHTTPFields, and non-WebBroker providers
// decode before storing. THorseCoreParam used to decode AGAIN on every indexed
// read, and GetItem wrote the result back, so a value holding a literal percent
// sign after the first decode broke:
//   '100%'   -> EConvertError "Error decoding URL style (%XX) encoded string"
//   '50%off' -> EConvertError "Invalid URL encoded character (%of)"
//   'a+%41'  -> silently 'a A'
// Field(...).AsString reads the dictionary directly and was always right, so
// every test compares Field, the first indexed read, a repeated indexed read
// and TryGetValue: they must all return the stored value unchanged.
//
// 'a+%41' is the case that discriminates on both compilers: on FPC DecodeParam
// uses HTTPDecode, which may not raise on a trailing percent sign, but it still
// turns + into a space and %41 into 'A'.

interface

uses
  DUnitX.TestFramework, Horse.Request, Horse.Core.Param, System.SysUtils;

type
  [TestFixture]
  TTestHorseRequestDecodeOnce = class
  private
    FRequest: THorseRequest;
    procedure AssertReturnedAsStored(const AParam: THorseCoreParam; const AStored: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('TrailingPercent', '100%')]
    [TestCase('PercentMidValue', '50%off')]
    [TestCase('PlusAndEncodedPercent', 'a+%41')]
    [TestCase('NoPercentControl', 'plain value')]
    procedure QueryReturnsStoredValue(AStored: string);

    [Test]
    [TestCase('TrailingPercent', '100%')]
    [TestCase('PercentMidValue', '50%off')]
    [TestCase('PlusAndEncodedPercent', 'a+%41')]
    [TestCase('NoPercentControl', 'plain value')]
    procedure ContentFieldsReturnStoredValue(AStored: string);

    [Test]
    procedure ParamsStillReturnStoredValue;
  end;

implementation

{ TTestHorseRequestDecodeOnce }

procedure TTestHorseRequestDecodeOnce.Setup;
begin
  FRequest := THorseRequest.Create(nil);
end;

procedure TTestHorseRequestDecodeOnce.TearDown;
begin
  FRequest.Free;
end;

procedure TTestHorseRequestDecodeOnce.AssertReturnedAsStored(const AParam: THorseCoreParam;
  const AStored: string);
var
  LValue: string;
begin
  AParam.Dictionary.AddOrSetValue('v', AStored);

  Assert.AreEqual(AStored, AParam.Field('v').AsString, 'Field(v).AsString');
  Assert.AreEqual(AStored, AParam['v'], 'first indexed read');
  // A second read catches the write-back: GetItem used to store its decoded
  // result, so the next read decoded that result again.
  Assert.AreEqual(AStored, AParam['v'], 'repeated indexed read');
  Assert.IsTrue(AParam.TryGetValue('v', LValue), 'TryGetValue must find the key');
  Assert.AreEqual(AStored, LValue, 'TryGetValue');
end;

procedure TTestHorseRequestDecodeOnce.QueryReturnsStoredValue(AStored: string);
begin
  AssertReturnedAsStored(FRequest.Query, AStored);
end;

procedure TTestHorseRequestDecodeOnce.ContentFieldsReturnStoredValue(AStored: string);
begin
  AssertReturnedAsStored(FRequest.ContentFields, AStored);
end;

procedure TTestHorseRequestDecodeOnce.ParamsStillReturnStoredValue;
begin
  // Params already used ADecodeValues=False; kept here so all three
  // collections are pinned to the same contract.
  AssertReturnedAsStored(FRequest.Params, '100%');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseRequestDecodeOnce);

end.
