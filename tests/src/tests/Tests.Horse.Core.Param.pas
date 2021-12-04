unit Tests.Horse.Core.Param;

interface

uses
  DUnitX.TestFramework,
  Horse.Exception,
  Horse.Core.Param,
  System.Generics.Collections,
  System.DateUtils,
  System.SysUtils;

type
  [TestFixture]
  TTestHorseCoreParam = class
  private
    FParams: TDictionary<string, String>;
    FHorseParam: THorseCoreParam;
    FData: TDateTime;
    FTime: TTime;
    FFormatSettings: TFormatSettings;

    function RequiredMessage(const AKey: String): string;
    function ConvertErrorMessage(const AKey, AValue, AType: String): string;

  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure ContainsKey;

    [Test]
    procedure NotContainsKey;

    [Test]
    procedure ContainsKeyDiferentCase;

    [Test]
    procedure ContainsValue;

    [Test]
    procedure NotContainsValue;

    [Test]
    procedure ToArray;

    [Test]
    procedure TryGetValueTrue;

    [Test]
    procedure TryGetValueFalse;

    [Test]
    procedure TryGetValueDiferentCase;

    [Test]
    procedure Content;

    [Test]
    procedure Count;

    [Test]
    procedure List;

    [Test]
    procedure Index;

    [Test]
    procedure IndexNotFound;

    [Test]
    procedure IndexDiferentCase;

    [Test]
    procedure AsBoolean;

    [Test]
    [TestCase('TrueValue1', 'True,true,true')]
    [TestCase('TrueValue2', '1,1,true')]
    [TestCase('FalseValue1', 'False,true,false')]
    [TestCase('FalseValue2', '5,1,false')]
    procedure AsBooleanParam(AParamValue, ATrueValue: string; AResult: Boolean);

    [Test]
    procedure AsBooleanNotRequired;

    [Test]
    procedure AsBooleanRequired;

    [Test]
    procedure AsCurrency;

    [Test]
    procedure AsCurrencyDecimalSeparator;

    [Test]
    procedure AsCurrencyNotRequired;

    [Test]
    procedure AsCurrencyRequired;

    [Test]
    procedure AsCurrencyErrorFormat;

    [Test]
    procedure AsDateTime;

    [Test]
    procedure AsDateTimeRequired;

    [Test]
    procedure AsDateTimeNotRequired;

    [Test]
    procedure AsDateTimeOnlyData;

    [Test]
    procedure AsDateTimeChangeFormat;

    [Test]
    procedure AsDateTimeInvalidFormat;

    [Test]
    procedure AsDate;

    [Test]
    procedure AsDateRequired;

    [Test]
    procedure AsDateNotRequired;

    [Test]
    procedure AsDateChangeFormat;

    [Test]
    procedure AsDateInvalidFormat;

    [Test]
    procedure AsExtended;

    [Test]
    procedure AsExtendedDecimalSeparator;

    [Test]
    procedure AsExtendedNotRequired;

    [Test]
    procedure AsExtendedRequired;

    [Test]
    procedure AsExtendedErrorFormat;

    [Test]
    procedure AsFloat;

    [Test]
    procedure AsFloatDecimalSeparator;

    [Test]
    procedure AsFloatNotRequired;

    [Test]
    procedure AsFloatRequired;

    [Test]
    procedure AsFloatErrorFormat;

    [Test]
    procedure AsInteger;

    [Test]
    procedure AsIntegerNotRequired;

    [Test]
    procedure AsIntegerRequired;

    [Test]
    procedure AsIntegerErrorFormat;

    [Test]
    procedure AsInt64;

    [Test]
    procedure AsInt64NotRequired;

    [Test]
    procedure AsInt64Required;

    [Test]
    procedure AsInt64ErrorFormat;

    [Test]
    procedure AsISO8601DateTime;

    [Test]
    procedure AsISO8601DateTimeOnlyData;

    [Test]
    procedure AsISO8601DateTimeNotRequired;

    [Test]
    procedure AsISO8601DateTimeRequired;

    [Test]
    procedure AsISO8601DateTimeErrorFormat;

    [Test]
    procedure AsString;

    [Test]
    procedure AsStringRequired;

    [Test]
    procedure AsStringNotRequired;

    [Test]
    procedure AsStringDiferentCase;

    [Test]
    procedure AsTime;

    [Test]
    procedure AsTimeRequired;

    [Test]
    procedure AsTimeNotRequired;

    [Test]
    procedure AsTimeChangeFormat;

    [Test]
    procedure AsTimeInvalidFormat;
  end;

implementation

{ TTestHorseCoreParam }

procedure TTestHorseCoreParam.AsBoolean;
begin
  FParams.AddOrSetValue('Key1', 'True');
  Assert.IsTrue(FHorseParam.AsBoolean('Key1'));
end;

procedure TTestHorseCoreParam.AsBooleanNotRequired;
begin
  Assert.IsFalse(FHorseParam.AsBoolean('Key1', False));
end;

procedure TTestHorseCoreParam.AsBooleanParam(AParamValue, ATrueValue: string; AResult: Boolean);
begin
  FParams.AddOrSetValue('Key1', AParamValue);
  Assert.AreEqual(AResult, FHorseParam.AsBoolean('Key1', True, ATrueValue));
end;

procedure TTestHorseCoreParam.AsBooleanRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsBoolean('Key1');
    end,
    EHorseException,
    RequiredMessage('Key1'));
end;

procedure TTestHorseCoreParam.AsCurrency;
begin
  FParams.AddOrSetValue('Key1', '5.5');
  Assert.AreEqual('5,5', CurrToStr( FHorseParam.AsCurrency('Key1'), FFormatSettings));
end;

procedure TTestHorseCoreParam.AsCurrencyDecimalSeparator;
begin
  FParams.AddOrSetValue('Key1', '5,5');
  Assert.AreEqual('5,5', CurrToStr( FHorseParam.AsCurrency('Key1'), FFormatSettings));
end;

procedure TTestHorseCoreParam.AsCurrencyErrorFormat;
begin
  FParams.AddOrSetValue('Key1', '5a');
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsCurrency('Key1');
    end,
    EHorseException,
    ConvertErrorMessage('Key1', '5a', 'numeric'));
end;

procedure TTestHorseCoreParam.AsCurrencyNotRequired;
begin
  Assert.AreEqual('0', CurrToStr(FHorseParam.AsCurrency('Key1', False)));
end;

procedure TTestHorseCoreParam.AsCurrencyRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsCurrency('Key1');
    end,
    EHorseException,
    RequiredMessage('Key1'));
end;

procedure TTestHorseCoreParam.AsDate;
begin
  FData := EncodeDate(2021, 11, 13);
  FParams.Add('Key', '2021-11-13 10:25:32');

  Assert.AreEqual(DateToStr(FData), DateToStr(FHorseParam.AsDate('Key')));
end;

procedure TTestHorseCoreParam.AsDateChangeFormat;
begin
  FData := EncodeDate(2021, 11, 13);
  FParams.Add('Key', '13/11/2021');

  Assert.AreEqual(DateToStr(FData), DateToStr(FHorseParam.AsDate('Key', True, 'dd/MM/yyyy')));
end;

procedure TTestHorseCoreParam.AsDateInvalidFormat;
begin
  FParams.Add('Key', '2021/11-13');

  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsDate('Key');
    end,
    EHorseException,
    ConvertErrorMessage('Key', '2021/11-13', 'date'));
end;

procedure TTestHorseCoreParam.AsDateNotRequired;
begin
  Assert.IsTrue(FHorseParam.AsDate('Key', False) = 0);
end;

procedure TTestHorseCoreParam.AsDateRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsDate('Key');
    end,
    EHorseException,
    RequiredMessage('Key'));
end;

procedure TTestHorseCoreParam.AsDateTime;
begin
  FData := EncodeDateTime(2021, 11, 13, 10, 25, 32, 0);
  FParams.Add('Key', '2021-11-13 10:25:32');

  Assert.AreEqual(DateToStr(FData), DateToStr(FHorseParam.AsDateTime('Key')));
end;

procedure TTestHorseCoreParam.AsDateTimeChangeFormat;
begin
  FData := EncodeDateTime(2021, 11, 13, 10, 25, 32, 0);
  FParams.Add('Key', '13/11/2021 10:25:32');

  Assert.AreEqual(DateToStr(FData), DateToStr(FHorseParam.AsDateTime('Key', True, 'dd/MM/yyyy')));
end;

procedure TTestHorseCoreParam.AsDateTimeInvalidFormat;
begin
  FParams.Add('Key', '2021/11-13 10:25:32');

  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsDateTime('Key');
    end,
    EHorseException,
    ConvertErrorMessage('Key', '2021/11-13 10:25:32', 'date'));
end;

procedure TTestHorseCoreParam.AsDateTimeNotRequired;
begin
  Assert.IsTrue(FHorseParam.AsDateTime('Key', False) = 0);
end;

procedure TTestHorseCoreParam.AsDateTimeOnlyData;
begin
  FData := EncodeDate(2021, 11, 13);
  FParams.Add('Key', '2021-11-13');

  Assert.AreEqual(DateToStr(FData), DateToStr(FHorseParam.AsDateTime('Key')));
end;

procedure TTestHorseCoreParam.AsDateTimeRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsDateTime('Key');
    end,
    EHorseException,
    RequiredMessage('Key'));
end;

procedure TTestHorseCoreParam.AsExtended;
begin
  FParams.AddOrSetValue('Key1', '5.5');
  Assert.AreEqual('5,5', FHorseParam.AsExtended('Key1').ToString(FFormatSettings));
end;

procedure TTestHorseCoreParam.AsExtendedDecimalSeparator;
begin
  FParams.AddOrSetValue('Key1', '5,5');
  Assert.AreEqual('5,5', FHorseParam.AsExtended('Key1').ToString(FFormatSettings));
end;

procedure TTestHorseCoreParam.AsExtendedErrorFormat;
begin
  FParams.AddOrSetValue('Key1', '5a');
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsExtended('Key1');
    end,
    EHorseException,
    ConvertErrorMessage('Key1', '5a', 'numeric'));
end;

procedure TTestHorseCoreParam.AsExtendedNotRequired;
begin
  Assert.AreEqual('0', FHorseParam.AsExtended('Key1', False).ToString);
end;

procedure TTestHorseCoreParam.AsExtendedRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsExtended('Key1');
    end,
    EHorseException,
    RequiredMessage('Key1'));
end;

procedure TTestHorseCoreParam.AsFloat;
begin
  FParams.AddOrSetValue('Key1', '5.5');
  Assert.AreEqual('5,5', FHorseParam.AsFloat('Key1').ToString(FFormatSettings));
end;

procedure TTestHorseCoreParam.AsFloatDecimalSeparator;
begin
  FParams.AddOrSetValue('Key1', '5,5');
  Assert.AreEqual('5,5', FHorseParam.AsFloat('Key1').ToString(FFormatSettings));
end;

procedure TTestHorseCoreParam.AsFloatErrorFormat;
begin
  FParams.AddOrSetValue('Key1', '5a');
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsFloat('Key1');
    end,
    EHorseException,
    ConvertErrorMessage('Key1', '5a', 'numeric'));
end;

procedure TTestHorseCoreParam.AsFloatNotRequired;
begin
  Assert.AreEqual('0', FHorseParam.AsFloat('Key1', False).ToString);
end;

procedure TTestHorseCoreParam.AsFloatRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsFloat('Key1');
    end,
    EHorseException,
    RequiredMessage('Key1'));
end;

procedure TTestHorseCoreParam.AsInt64;
begin
  FParams.AddOrSetValue('Key1', '5');
  Assert.AreEqual('5', FHorseParam.AsInt64('Key1').ToString);
end;

procedure TTestHorseCoreParam.AsInt64ErrorFormat;
begin
  FParams.AddOrSetValue('Key1', 'Value');
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsInt64('Key1');
    end,
    EHorseException,
    ConvertErrorMessage('Key1', 'Value', 'int64'));
end;

procedure TTestHorseCoreParam.AsInt64NotRequired;
begin
  Assert.AreEqual('0', FHorseParam.AsInt64('Key1', False).ToString);
end;

procedure TTestHorseCoreParam.AsInt64Required;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsInt64('Key1');
    end,
    EHorseException,
    RequiredMessage('Key1'));
end;

procedure TTestHorseCoreParam.AsInteger;
begin
  FParams.AddOrSetValue('Key1', '5');
  Assert.AreEqual(5, FHorseParam.AsInteger('Key1'));
end;

procedure TTestHorseCoreParam.AsIntegerErrorFormat;
begin
  FParams.AddOrSetValue('Key1', 'Value');
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsInteger('Key1');
    end,
    EHorseException,
    ConvertErrorMessage('Key1', 'Value', 'integer'));
end;

procedure TTestHorseCoreParam.AsIntegerNotRequired;
begin
  Assert.AreEqual(0, FHorseParam.AsInteger('Key1', False));
end;

procedure TTestHorseCoreParam.AsIntegerRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsInteger('Key1');
    end,
    EHorseException,
    RequiredMessage('Key1'));
end;

procedure TTestHorseCoreParam.AsISO8601DateTime;
begin
  FData := EncodeDateTime(2021, 11, 13, 10, 21, 22, 0);
  FParams.AddOrSetValue('Key', '2021-11-13T10:21:22');

  Assert.AreEqual(DateToStr(FData), DateToStr(FHorseParam.AsISO8601DateTime('Key')));
end;

procedure TTestHorseCoreParam.AsISO8601DateTimeErrorFormat;
begin
  FParams.AddOrSetValue('Key', '2021-11-13 10:21:22');
  Assert.WillNotRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsISO8601DateTime('Key');
    end,
    EHorseException,
    RequiredMessage('Key'));
end;

procedure TTestHorseCoreParam.AsISO8601DateTimeNotRequired;
begin
  Assert.IsTrue(FHorseParam.AsISO8601DateTime('Key', False) = 0);
end;

procedure TTestHorseCoreParam.AsISO8601DateTimeOnlyData;
begin
  FData := EncodeDateTime(2021, 11, 13, 10, 21, 22, 0);
  FParams.AddOrSetValue('Key', '2021-11-13');

  Assert.AreEqual(FormatDateTime('dd/MM/yyyy', FData), FormatDateTime('dd/MM/yyyy', FHorseParam.AsISO8601DateTime('Key')));
end;

procedure TTestHorseCoreParam.AsISO8601DateTimeRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsISO8601DateTime('Key');
    end,
    EHorseException,
    RequiredMessage('Key'));
end;

procedure TTestHorseCoreParam.AsString;
begin
  FParams.AddOrSetValue('Key', 'Value');
  Assert.AreEqual('Value', FHorseParam.AsString('Key'));
end;

procedure TTestHorseCoreParam.AsStringDiferentCase;
begin
  FParams.AddOrSetValue('key', 'Value');
  Assert.AreEqual('Value', FHorseParam.AsString('KEY'));
end;

procedure TTestHorseCoreParam.AsStringNotRequired;
begin
  Assert.IsEmpty(FHorseParam.AsString('Key', False));
end;

procedure TTestHorseCoreParam.AsStringRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsString('Key');
    end,
    EHorseException,
    RequiredMessage('Key'));
end;

procedure TTestHorseCoreParam.AsTime;
begin
  FTime := EncodeTime(10, 15, 54, 0);
  FParams.AddOrSetValue('Key', '10:15:54');

  Assert.AreEqual(FTime, FHorseParam.AsTime('Key'));
end;

procedure TTestHorseCoreParam.AsTimeChangeFormat;
begin
  FTime := EncodeTime(10, 15, 0, 0);
  FParams.AddOrSetValue('Key', '10:15:54');

  Assert.AreEqual(FTime, FHorseParam.AsTime('Key', True, 'hh:mm'));
end;

procedure TTestHorseCoreParam.AsTimeInvalidFormat;
begin
  FParams.AddOrSetValue('Key', '10/00');

  Assert.WillRaiseWithMessage(
    procedure
    begin
      Assert.AreEqual(FTime, FHorseParam.AsTime('Key'));
    end,
    EHorseException,
    ConvertErrorMessage('Key', '10/00', 'time'));
end;

procedure TTestHorseCoreParam.AsTimeNotRequired;
begin
  Assert.IsTrue(FHorseParam.AsTime('Key', False) = 0);
end;

procedure TTestHorseCoreParam.AsTimeRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      Assert.AreEqual(FTime, FHorseParam.AsTime('Key'));
    end,
    EHorseException,
    RequiredMessage('Key'));
end;

procedure TTestHorseCoreParam.ContainsKey;
begin
  FParams.AddOrSetValue('Key', 'Value');
  Assert.IsTrue(FHorseParam.ContainsKey('Key'));
end;

procedure TTestHorseCoreParam.ContainsKeyDiferentCase;
begin
  FParams.AddOrSetValue('key', 'Value');
  Assert.IsTrue(FHorseParam.ContainsKey('KEY'));
end;

procedure TTestHorseCoreParam.ContainsValue;
begin
  FParams.AddOrSetValue('Key', 'Value');
  Assert.IsTrue(FHorseParam.ContainsValue('Value'));
end;

procedure TTestHorseCoreParam.Content;
begin
  FParams.AddOrSetValue('Key1', 'Value1');
  FParams.AddOrSetValue('Key2', 'Value2');

  Assert.AreEqual(2, FHorseParam.Content.Count);
  Assert.AreEqual('Key1', FHorseParam.Content.Names[0]);
  Assert.AreEqual('Key2', FHorseParam.Content.Names[1]);
  Assert.AreEqual('Value1', FHorseParam.Content.ValueFromIndex[0]);
  Assert.AreEqual('Value2', FHorseParam.Content.ValueFromIndex[1]);
end;

function TTestHorseCoreParam.ConvertErrorMessage(const AKey, AValue, AType: String): string;
begin
  result := Format('The %s param ''%s'' is not valid a %s type.', [AKey, AValue, AType]);
end;

procedure TTestHorseCoreParam.Count;
begin
  FParams.AddOrSetValue('Key1', 'Value1');
  FParams.AddOrSetValue('Key2', 'Value2');

  Assert.AreEqual(2, FHorseParam.Count);
end;

procedure TTestHorseCoreParam.Index;
begin
  FParams.AddOrSetValue('Key1', 'Value1');
  Assert.AreEqual('Value1', FHorseParam['Key1']);
end;

procedure TTestHorseCoreParam.IndexDiferentCase;
begin
  FParams.AddOrSetValue('KEY1', 'Value1');
  Assert.AreEqual('Value1', FHorseParam['key1']);
end;

procedure TTestHorseCoreParam.IndexNotFound;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FParams.AddOrSetValue('Key1', 'Value1');
      Assert.AreEqual('Value1', FHorseParam['Value1']);
    end,
    EListError,
    'Item Value1 not found');

end;

procedure TTestHorseCoreParam.List;
begin
  FParams.AddOrSetValue('Key1', 'Value1');
  FParams.AddOrSetValue('Key2', 'Value2');

  Assert.AreEqual(2, FHorseParam.Count);
  Assert.AreEqual(FParams, FHorseParam.Dictionary);
end;

procedure TTestHorseCoreParam.NotContainsKey;
begin
  FParams.AddOrSetValue('Key', 'Value');
  Assert.IsFalse(FHorseParam.ContainsKey('Value'));
end;

procedure TTestHorseCoreParam.NotContainsValue;
begin
  FParams.AddOrSetValue('Key', 'Value');
  Assert.IsFalse(FHorseParam.ContainsValue('Key'));
end;

function TTestHorseCoreParam.RequiredMessage(const AKey: String): string;
begin
  result := Format('The %s param is required.', [AKey]);
end;

procedure TTestHorseCoreParam.Setup;
begin
  FParams := TDictionary<String, String>.Create;
  FHorseParam := THorseCoreParam.create(FParams);
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.DecimalSeparator := ',';
  FData := 0;
  FTime := 0;
end;

procedure TTestHorseCoreParam.TearDown;
begin
  FreeAndNil(FHorseParam);
end;

procedure TTestHorseCoreParam.ToArray;
var
  LPairs: TArray<TPair<String, String>>;
begin
  FParams.AddOrSetValue('Key1', 'Value1');
  FParams.AddOrSetValue('Key2', 'Value2');

  LPairs := FHorseParam.ToArray;

  Assert.AreEqual(2, Length(LPairs));
  Assert.AreEqual('Key1', LPairs[0].Key);
  Assert.AreEqual('Value1', LPairs[0].Value);
  Assert.AreEqual('Key2', LPairs[1].Key);
  Assert.AreEqual('Value2', LPairs[1].Value);
end;

procedure TTestHorseCoreParam.TryGetValueDiferentCase;
var
  LValue: String;
begin
  FParams.AddOrSetValue('KEY1', 'Value1');
  Assert.IsTrue(FHorseParam.TryGetValue('key1', LValue));
  Assert.AreEqual('Value1', LValue);
end;

procedure TTestHorseCoreParam.TryGetValueFalse;
var
  LValue: String;
begin
  Assert.IsFalse(FHorseParam.TryGetValue('Key1', LValue));
end;

procedure TTestHorseCoreParam.TryGetValueTrue;
var
  LValue: String;
begin
  FParams.AddOrSetValue('Key1', 'Value1');
  Assert.IsTrue(FHorseParam.TryGetValue('Key1', LValue));
  Assert.AreEqual('Value1', LValue);
end;

end.
