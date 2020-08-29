unit Horse.Provider.Apache;

interface

{$IF DEFINED(HORSE_APACHE) AND NOT DEFINED(FPC)}


uses
  Horse.Provider.Abstract, System.SysUtils, Web.HTTPD24Impl;

type

  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class var FHandlerName: string;
    class var FDefaultModule: Pointer;
    class procedure InternalListen; static;
    class procedure SetHandlerName(const Value: string); static;
    class function GetHandlerName: string; static;
    class function GetDefaultModule: Pointer; static;
    class procedure SetDefaultModule(const Value: Pointer); static;
  public
    class property HandlerName: string read GetHandlerName write SetHandlerName;
    class property DefaultModule: Pointer read GetDefaultModule write SetDefaultModule;
    class procedure Start; deprecated 'Use Listen instead';
    class procedure Listen; overload; override;
    class procedure Listen(ACallback: TProc<T>); reintroduce; overload; static;
  end;

{$ENDIF}

implementation

{$IF DEFINED(HORSE_APACHE) AND NOT DEFINED(FPC)}


uses Web.WebBroker, Web.ApacheApp, {$IFDEF MSWINDOWS} Winapi.ActiveX, System.Win.ComObj, {$ENDIF } Horse.WebModule;

{ THorseProvider<T:class> }

class procedure THorseProvider<T>.InternalListen;
begin
{$IFDEF MSWINDOWS}
  CoInitFlags := COINIT_MULTITHREADED;
{$ENDIF}
  Web.ApacheApp.InitApplication(FDefaultModule, FHandlerName);
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

class function THorseProvider<T>.GetHandlerName: string;
begin
  Result := FHandlerName;
end;

class procedure THorseProvider<T>.SetHandlerName(const Value: string);
begin
  FHandlerName := Value;
end;

class function THorseProvider<T>.GetDefaultModule: Pointer;
begin
  Result := FDefaultModule;
end;

class procedure THorseProvider<T>.SetDefaultModule(const Value: Pointer);
begin
  FDefaultModule := Value;
end;

{$ENDIF}

end.
