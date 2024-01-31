unit Horse.Provider.Apache;

interface

{$IF DEFINED(HORSE_APACHE) AND NOT DEFINED(FPC)}
uses
  Horse.Provider.Abstract,
  System.SysUtils,
  Web.HTTPD24Impl;

type
  THorseProvider = class(THorseProviderAbstract)
  private
    class var FMaxConnections: Integer;
    class var FHandlerName: string;
    class var FDefaultModule: Pointer;

    class procedure SetMaxConnections(const AValue: Integer); static;
    class function GetMaxConnections: Integer; static;
    class procedure InternalListen; static;
    class procedure SetHandlerName(const AValue: string); static;
    class function GetHandlerName: string; static;
    class function GetDefaultModule: Pointer; static;
    class procedure SetDefaultModule(const AValue: Pointer); static;
  public
    class property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;
    class property HandlerName: string read GetHandlerName write SetHandlerName;
    class property DefaultModule: Pointer read GetDefaultModule write SetDefaultModule;
    class procedure Listen; overload; override;
    class procedure Listen(const ACallback: TProc); reintroduce; overload; static;
  end;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_APACHE) AND NOT DEFINED(FPC)}
uses
  Web.WebBroker,
  Web.ApacheApp,
{$IFDEF MSWINDOWS}
  Winapi.ActiveX,
  System.Win.ComObj,
{$ENDIF}
  Horse.WebModule;

class procedure THorseProvider.InternalListen;
begin
{$IFDEF MSWINDOWS}
  CoInitFlags := COINIT_MULTITHREADED;
{$ENDIF}
  Web.ApacheApp.InitApplication(FDefaultModule, UTF8String(FHandlerName));
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;

  if FMaxConnections > 0 then
  begin
    Application.MaxConnections:= FMaxConnections;
  end;

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

class function THorseProvider.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

class procedure THorseProvider.SetMaxConnections(const AValue: Integer);
begin
  FMaxConnections := AValue;
end;

class function THorseProvider.GetHandlerName: string;
begin
  Result := FHandlerName;
end;

class procedure THorseProvider.SetHandlerName(const AValue: string);
begin
  FHandlerName := AValue;
end;

class function THorseProvider.GetDefaultModule: Pointer;
begin
  Result := FDefaultModule;
end;

class procedure THorseProvider.SetDefaultModule(const AValue: Pointer);
begin
  FDefaultModule := AValue;
end;
{$ENDIF}

end.
