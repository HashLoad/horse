unit Horse.Provider.Apache;

interface

{$IF DEFINED(HORSE_APACHE) AND NOT DEFINED(FPC)}


uses
  Horse.Provider.Abstract, System.SysUtils, Web.HTTPD24Impl;

type

  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class procedure InternalListen; static;
  public
    class procedure Start; deprecated 'Use Listen instead';
    class procedure Listen; overload; override;
    class procedure Listen(ACallback: TProc<T>); reintroduce; overload; static;
  end;

var
  ModuleData: TApacheModuleData;

{$ENDIF}

implementation

{$IF DEFINED(HORSE_APACHE) AND NOT DEFINED(FPC)}


uses Web.ApacheApp, Web.WebBroker, {$IFDEF MSWINDOWS} Winapi.ActiveX, System.Win.ComObj, {$ENDIF } Horse.WebModule;

  { THorseProvider<T:class> }

class procedure THorseProvider<T>.InternalListen;
begin
{$IFDEF MSWINDOWS}
  CoInitFlags := COINIT_MULTITHREADED;
{$ENDIF}
  Web.ApacheApp.InitApplication(@ModuleData);
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

class procedure THorseProvider<T>.Listen(ACallback: TProc<T>);
begin
  inherited;
  SetOnListen(ACallback);
  InternalListen;
end;

class procedure THorseProvider<T>.Start;
begin
  Listen;
end;

{$ENDIF}

end.
