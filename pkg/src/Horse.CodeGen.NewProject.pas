unit Horse.CodeGen.NewProject;

interface

uses PlatformAPI, ToolsAPI;

type
   TNewProject = class abstract(TNotifierObject, IOTACreator, IOTAProjectCreator, IOTAProjectCreator80)
   protected
    { IOTACreator }
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    { IOTAProjectCreator }
    function GetFileName: string;
    function GetOptionFileName: string; deprecated;
    function GetShowSource: Boolean;
    function NewOptionSource(const ProjectName: string): IOTAFile; deprecated;
    function NewProjectSource(const ProjectName: string): IOTAFile; virtual; abstract;
    procedure NewProjectResource(const Project: IOTAProject);
    procedure NewDefaultModule; deprecated;
    { IOTAProjectCreator80 }
    function GetProjectPersonality: string; virtual;
    procedure NewDefaultProjectModule(const Project: IOTAProject);
  private
    procedure SetFileName(const AValue: string);
  protected
    FFileName: string;
  public
    property FileName: string read GetFileName write SetFileName;
  end;

  TNewProjectEx = class(TNewProject, IOTAProjectCreator160)
  private
    FPersonality: string;
  protected
    function GetProjectPersonality: string; override;
    { IOTAProjectCreator160 }
    function GetPlatforms: TArray<string>;
    function GetFrameworkType: string; virtual;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);
  public
    property Personality: string read FPersonality write FPersonality;
  end;

implementation

uses System.SysUtils;

{ TNewProject }

function TNewProject.GetCreatorType: string;
begin
  Result := sConsole;
end;

function TNewProject.GetExisting: Boolean;
begin
  Result := False;
end;

function TNewProject.GetFileName: string;
begin
  Result := FFileName;
end;

function TNewProject.GetFileSystem: string;
begin
  Result := EmptyStr;
end;

function TNewProject.GetOptionFileName: string;
begin
  Result := EmptyStr;
end;

function TNewProject.GetOwner: IOTAModule;
begin
  Result := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
end;

function TNewProject.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TNewProject.GetShowSource: Boolean;
begin
  Result := False;
end;

function TNewProject.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TNewProject.NewDefaultModule;
begin

end;

procedure TNewProject.NewDefaultProjectModule(const Project: IOTAProject);
begin

end;

function TNewProject.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TNewProject.NewProjectResource(const Project: IOTAProject);
begin

end;

procedure TNewProject.SetFileName(const AValue: string);
begin
  FFileName := AValue;
end;

{ TNewProjectEx }

function TNewProjectEx.GetFrameworkType: string;
begin
  Result := EmptyStr;
end;

function TNewProjectEx.GetPlatforms: TArray<string>;
begin
  Result := TArray<string>.Create(cWin32Platform, cWin64Platform);
end;

function TNewProjectEx.GetPreferredPlatform: string;
begin
  Result := EmptyStr;
end;

function TNewProjectEx.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality
end;

procedure TNewProjectEx.SetInitialOptions(const NewProject: IOTAProject);
var
  LProjectOptionsConfigurations: IOTAProjectOptionsConfigurations;
begin
  if Supports(NewProject.ProjectOptions, IOTAProjectOptionsConfigurations, LProjectOptionsConfigurations) then
    LProjectOptionsConfigurations.BaseConfiguration.AsBoolean['UsingDelphiRTL'] := True;
end;

end.
