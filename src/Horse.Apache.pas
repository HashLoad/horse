unit Horse.Apache;

interface

uses Web.WebBroker, Web.ApacheApp, Web.HTTPD24Impl, Horse.Core, Horse.HTTP, System.SysUtils, Horse.Router, Horse.Exception;

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

var
  GModuleData: TApacheModuleData;

implementation

{ THorse }

uses {$IFDEF MSWINDOWS} Winapi.ActiveX, System.Win.ComObj, {$ENDIF } Horse.WebModule;

procedure THorse.Start;
begin
  inherited;
{$IFDEF MSWINDOWS}
  CoInitFlags := COINIT_MULTITHREADED;
{$ENDIF}
  Web.ApacheApp.InitApplication(@GModuleData);
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end;

end.
