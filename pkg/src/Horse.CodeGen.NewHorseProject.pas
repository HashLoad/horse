unit Horse.CodeGen.NewHorseProject;

interface

uses ToolsAPI, Horse.CodeGen.NewProject;

type
  THorseProjectFile = class(TNewProjectEx)
  private
    FDefaultPort: Integer;
    procedure SetDefaultPort(const AValue: Integer);
  protected
    function NewProjectSource(const ProjectName: string): IOTAFile; override;
    function GetFrameworkType: string; override;
  public
    constructor Create; overload;
    constructor Create(const APersonality: string); overload;
    property DefaultPort: Integer read FDefaultPort write SetDefaultPort;
  end;

implementation

uses Horse.CodeGen.SourceFile, Horse.CodeGen.Templates, System.SysUtils;

{ THorseProjectFile }

constructor THorseProjectFile.Create;
begin
  FFileName := EmptyStr;
  FDefaultPort := 0;
end;

constructor THorseProjectFile.Create(const APersonality: string);
begin
  Create;
  Personality := APersonality;
end;

function THorseProjectFile.GetFrameworkType: string;
begin
  Result := 'VCL';
end;

function THorseProjectFile.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := TSourceFile.Create(sHorseDPR, [ProjectName, FDefaultPort]);
end;

procedure THorseProjectFile.SetDefaultPort(const AValue: Integer);
begin
  FDefaultPort := AValue;
end;

end.
