unit Horse.ISAPI;

interface

uses Horse.Core, Horse.HTTP, System.SysUtils, Horse.Router, Horse.Exception;

type
  EHorseException = Horse.Exception.EHorseException;
  EHorseCallbackInterrupted = Horse.Exception.EHorseCallbackInterrupted;
  TProc = System.SysUtils.TProc;
  THorseList = Horse.HTTP.THorseList;
  THorseRequest = Horse.HTTP.THorseRequest;
  THorseHackRequest = Horse.HTTP.THorseHackRequest;
  THorseResponse = Horse.HTTP.THorseResponse;
  THorseHackResponse = Horse.HTTP.THorseHackResponse;
  THorseCallback = Horse.Router.THorseCallback;

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
