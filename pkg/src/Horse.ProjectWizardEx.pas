unit Horse.ProjectWizardEx;

interface

uses ToolsApi, VCL.Graphics, PlatformAPI;

type
  THorseNewProjectWizard = class
  public
    class procedure RegisterHorseProjectWizard(const APersonality: string);
  end;

implementation

uses DccStrs, System.IOUtils, VCL.Controls, VCL.Forms, WinApi.Windows, System.SysUtils, Horse.Views.Wizard,
  Horse.CodeGen.NewHorseProject, ExpertsRepository;

resourcestring
  sNewHorseProjectCaption = 'Horse Project';
  sNewHorseProjectHint = 'Create a new project';

{ THorseNewProjectWizard }

class procedure THorseNewProjectWizard.RegisterHorseProjectWizard(const APersonality: string);
begin
  RegisterPackageWizard(
    TExpertsRepositoryProjectWizardWithProc.Create(APersonality, sNewHorseProjectHint, sNewHorseProjectCaption,
    'Horse.Wizard.NewProjectWizard',
    'Horse', 'Horse - https://github.com/HashLoad/horse',
    procedure
    var
      LWizardForm: TFrmNewProject;
      LModuleServices: IOTAModuleServices;
      LProject: IOTAProject;
      LBuildConfiguration: IOTABuildConfiguration;
      LProjectSourceCreator: IOTACreator;
    begin
      LWizardForm := TFrmNewProject.Create(Application);
      try
        if LWizardForm.ShowModal = mrOk then
        begin
          if not LWizardForm.AddToProjectGroup then
          begin
            (BorlandIDEServices as IOTAModuleServices).CloseAll;
          end;
          LModuleServices := (BorlandIDEServices as IOTAModuleServices);

          LProjectSourceCreator := THorseProjectFile.Create(APersonality);
          THorseProjectFile(LProjectSourceCreator).DefaultPort := 9000;
          LModuleServices.CreateModule(LProjectSourceCreator);
          LProject := GetActiveProject;

          LBuildConfiguration := (LProject.ProjectOptions as IOTAProjectOptionsConfigurations).BaseConfiguration;
          LBuildConfiguration.SetValue(sUnitSearchPath, '$(Horse)');
          LBuildConfiguration.SetValue(sFramework, 'VCL');
        end;
      finally
        LWizardForm.Free;
      end;
    end,
    function: Cardinal
    begin
      Result := LoadIcon(HInstance, 'HashloadIcon');
    end,
    TArray<string>.Create(cWin32Platform, cWin64Platform), nil));
end;

end.
