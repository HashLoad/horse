unit Horse.Provider.ISAPI;

interface

{$IF DEFINED(HORSE_ISAPI) AND NOT DEFINED(FPC)}
uses Horse.Provider.Abstract, System.SysUtils, Web.Win.ISAPIApp;

type
  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class procedure InternalListen; static;
  public
    class procedure Listen; overload; override;
    class procedure Listen(const ACallback: TProc<T>); reintroduce; overload; static;
  end;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_ISAPI) AND NOT DEFINED(FPC)}
uses Web.WebBroker, System.Win.ComObj, Winapi.ActiveX, Horse.WebModule;

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

class procedure THorseProvider<T>.InternalListen;
begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  DoOnListen;
  Application.Run;
end;

class procedure THorseProvider<T>.Listen;
begin
  inherited;
  InternalListen;
end;

class procedure THorseProvider<T>.Listen(const ACallback: TProc<T>);
begin
  inherited;
  SetOnListen(ACallback);
  InternalListen;
end;
{$ENDIF}

end.
