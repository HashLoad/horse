unit Horse.Registration;

interface

procedure register;

implementation

uses ToolsApi, DesignIntf, System.SysUtils, Horse.ProjectWizardEx, Winapi.Windows;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  THorseNewProjectWizard.RegisterHorseProjectWizard(sDelphiPersonality);
end;

end.
