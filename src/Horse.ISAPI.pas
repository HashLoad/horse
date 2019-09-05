unit Horse.ISAPI;

interface

uses Horse.Core;

type
  THorse = class(THorseCore)
  public
    procedure Start; override;
  end;

implementation

{ THorse }

uses Web.WebBroker, System.Win.ComObj, Winapi.ActiveX, Horse.WebModule;

procedure THorse.Start;
begin
  inherited;
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end;

end.
