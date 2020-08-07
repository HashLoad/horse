unit Horse.Provider.Apache;

interface

{$IF DEFINED(HORSE_APACHE)}


uses
  Horse.Provider.Abstract, System.SysUtils;

type

  THorseProvider = class(THorseProviderAbstract)
  private
    class procedure InternalListen; static;
  public
    class procedure Start; deprecated 'Use Listen instead';
    class procedure Listen; overload; override;
    class procedure Listen(ACallback: TProc<TObject>); reintroduce; overload; static;
  end;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_APACHE)}


uses Web.WebBroker, Web.ApacheApp, Web.HTTPD24Impl, {$IFDEF MSWINDOWS} Winapi.ActiveX, System.Win.ComObj, {$ENDIF } Horse.WebModule;

var
  ModuleData: TApacheModuleData;

  { THorseProvider }

class procedure THorseProvider.InternalListen;
begin
{$IFDEF MSWINDOWS}
  CoInitFlags := COINIT_MULTITHREADED;
{$ENDIF}
  Web.ApacheApp.InitApplication(@ModuleData);
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end;

class procedure THorseProvider.Listen;
begin
  inherited;
  InternalListen;
end;

class procedure THorseProvider.Listen(ACallback: TProc<TObject>);
begin
  inherited;
  SetOnListen(ACallback);
  InternalListen;
end;

class procedure THorseProvider.Start;
begin
  Listen;
end;

{$ENDIF}

end.
