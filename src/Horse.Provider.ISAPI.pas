unit Horse.Provider.ISAPI;

interface

{$IF DEFINED(HORSE_ISAPI) AND NOT DEFINED(FPC)}
uses
  Horse.Provider.Abstract,
  System.SysUtils,
  Web.Win.ISAPIApp;

type
  THorseProvider = class(THorseProviderAbstract)
  private
    class procedure InternalListen; static;
  public
    class procedure Listen; overload; override;
    class procedure Listen(const ACallback: TProc); reintroduce; overload; static;
  end;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_ISAPI) AND NOT DEFINED(FPC)}
uses
  Web.WebBroker,
  System.Win.ComObj,
  Winapi.ActiveX,
  Horse.WebModule;

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

class procedure THorseProvider.InternalListen;
begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  DoOnListen;
  Application.Run;
end;

class procedure THorseProvider.Listen;
begin
  inherited;
  InternalListen;
end;

class procedure THorseProvider.Listen(const ACallback: TProc);
begin
  inherited;
  SetOnListen(ACallback);
  InternalListen;
end;
{$ENDIF}

end.
