unit Tests.Horse.Core.Param;

interface

uses
  DUnitX.TestFramework,
  Horse.Core.Param,
  System.Generics.Collections,
  System.SysUtils;

type
  [TestFixture]
  TTestHorseCoreParam = class
  private
    FParams: TDictionary<string, String>;
    FHorseParam: THorseCoreParam;

  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure AsInteger;
  end;

implementation

{ TTestHorseCoreParam }

procedure TTestHorseCoreParam.AsInteger;
begin
  FParams.AddOrSetValue('IntParam', '5');
  Assert.AreEqual(5, FHorseParam.AsInteger('IntParam'));
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
