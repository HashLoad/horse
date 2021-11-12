unit Tests.Horse.Core.Param;

interface

uses
  DUnitX.TestFramework,
  Horse.Exception,
  Horse.Core.Param,
  System.Generics.Collections,
  System.SysUtils;

type
  [TestFixture]
  TTestHorseCoreParam = class
  private
    FParams: TDictionary<string, String>;
    FHorseParam: THorseCoreParam;

    function RequiredMessage(const AKey: String): string;
    function ConvertErrorMessage(const AKey, AValue: String): string;

  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure AsInteger;

    [Test]
    procedure AsIntegerNotRequired;

    [Test]
    procedure AsIntegerRequired;

    [Test]
    procedure AsIntegerErrorFormat;
  end;

implementation

{ TTestHorseCoreParam }

procedure TTestHorseCoreParam.AsInteger;
begin
  FParams.AddOrSetValue('IntParam', '5');
  Assert.AreEqual(5, FHorseParam.AsInteger('IntParam'));
end;

procedure TTestHorseCoreParam.AsIntegerErrorFormat;
begin
  FParams.AddOrSetValue('IntParam', 'Value');
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsInteger('IntParam');
    end,
    EHorseException,
    ConvertErrorMessage('IntParam', 'Value'));
end;

procedure TTestHorseCoreParam.AsIntegerNotRequired;
begin
  Assert.AreEqual(0, FHorseParam.AsInteger('IntParam', False));
end;

procedure TTestHorseCoreParam.AsIntegerRequired;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseParam.AsInteger('IntParam');
    end,
    EHorseException,
    RequiredMessage('IntParam'));
end;

function TTestHorseCoreParam.ConvertErrorMessage(const AKey, AValue: String): string;
begin
  result := Format('The %s param ''%s'' is not valid a integer type.', [AKey, AValue]);
end;

function TTestHorseCoreParam.RequiredMessage(const AKey: String): string;
begin
  result := Format('The %s param is required.', [AKey]);
end;

procedure TTestHorseCoreParam.Setup;
begin
  FParams := TDictionary<String, String>.Create;
  FHorseParam := THorseCoreParam.create(FParams);
end;

procedure TTestHorseCoreParam.TearDown;
begin
  FreeAndNil(FHorseParam);
end;

end.
