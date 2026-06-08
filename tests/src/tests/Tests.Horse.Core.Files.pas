unit Tests.Horse.Core.Files;

interface

uses
  DUnitX.TestFramework,
  Horse.Core.Files,
  System.Classes,
  System.SysUtils;

type
  [TestFixture]
  TTestHorseCoreFile = class
  private
    FHorseFile: THorseCoreFile;
    FFileName: String;

  public
    [Setup]
    procedure Setup;

    [Teardown]
    procedure Teardown;

    [Test]
    procedure EmptyFileName;

    [Test]
    procedure InvalidFileName;

    [Test]
    procedure DelphiFile;
  end;

implementation

{ TTestHorseCoreFile }

procedure TTestHorseCoreFile.DelphiFile;
begin
  FFileName := ExtractFilePath(GetModuleName(HInstance));
  FFileName := FFileName.Replace('tests\', 'src\Horse.pas');

  FHorseFile := THorseCoreFile.Create(FFileName);

  Assert.AreEqual('text/x-pascal', FHorseFile.ContentType);
  Assert.IsNotNull(FHorseFile.ContentStream);
  Assert.IsTrue(FHorseFile.Size > 0);
end;

procedure TTestHorseCoreFile.EmptyFileName;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseFile := THorseCoreFile.Create(EmptyStr);
    end,
    Exception,
    'Invalid FileName');
end;

procedure TTestHorseCoreFile.InvalidFileName;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      FHorseFile := THorseCoreFile.Create('C:\InvalidFile.txt2');
    end,
    Exception,
    'File not exist');
end;

procedure TTestHorseCoreFile.Setup;
begin
  FFileName := EmptyStr;
end;

procedure TTestHorseCoreFile.Teardown;
begin
  FreeAndNil(FHorseFile);
end;

end.
