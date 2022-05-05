unit Horse.Provider.FPC.LCL;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(HORSE_LCL)}
uses SysUtils, Classes, httpdefs, fpHTTP, fphttpapp, Horse.Provider.Abstract, Horse.Constants, Horse.Proc;

type

  { THorseProvider }

  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FListenQueue: Integer;
    class var FHTTPApplication: THTTPApplication;
    class function GetDefaultHTTPApplication: THTTPApplication;
    class function HTTPApplicationIsNil: Boolean;
    class procedure SetListenQueue(const AValue: Integer); static;
    class procedure SetPort(const AValue: Integer); static;
    class procedure SetHost(const AValue: string); static;
    class function GetListenQueue: Integer; static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class function GetHost: string; static;
    class procedure InternalListen; virtual;
    class procedure InternalStopListen; virtual;
    class procedure DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
  public
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class property ListenQueue: Integer read GetListenQueue write SetListenQueue;
    class procedure StopListen; override;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0'; const ACallbackListen: TProc<T> = nil; const ACallbackStopListen: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallbackListen: TProc<T>; const ACallbackStopListen: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallbackListen: TProc<T> = nil; const ACallbackStopListen: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: TProc<T>; const ACallbackStopListen: TProc<T> = nil); reintroduce; overload; static;
    class function IsRunning: Boolean;
  end;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_LCL)}
uses Horse.WebModule;

class function THorseProvider<T>.GetDefaultHTTPApplication: THTTPApplication;
begin
  if HTTPApplicationIsNil then
    FHTTPApplication := Application;
  Result := FHTTPApplication;
end;

class function THorseProvider<T>.HTTPApplicationIsNil: Boolean;
begin
  Result := FHTTPApplication = nil;
end;

class function THorseProvider<T>.GetDefaultHost: string;
begin
  Result := DEFAULT_HOST;
end;

class function THorseProvider<T>.GetDefaultPort: Integer;
begin
  Result := DEFAULT_PORT;
end;

class function THorseProvider<T>.GetHost: string;
begin
  Result := FHost;
end;

class function THorseProvider<T>.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

class function THorseProvider<T>.GetPort: Integer;
begin
  Result := FPort;
end;

class procedure THorseProvider<T>.InternalListen;
begin
  inherited;
  if FPort <= 0 then
    FPort := GetDefaultPort;
  if FHost.IsEmpty then
    FHost := GetDefaultHost;
  if FListenQueue = 0 then
    FListenQueue := 15;
  FHTTPApplication := GetDefaultHTTPApplication;
  FHTTPApplication.Initialize;
  FHTTPApplication.AllowDefaultModule := True;
  FHTTPApplication.OnGetModule := DoGetModule;
  FHTTPApplication.Threaded := True;
  FHTTPApplication.QueueSize := FListenQueue;
  FHTTPApplication.Port := FPort;
  FHTTPApplication.LegacyRouting := True;
  FHTTPApplication.Address := FHost;
  FRunning := True;
  DoOnListen;
  FHTTPApplication.Run;
end;

class procedure THorseProvider<T>.InternalStopListen;
begin
  if not HTTPApplicationIsNil then
  begin
    FHTTPApplication.Terminate;
    DoOnStopListen;
    FRunning := False;
  end
  else
    raise Exception.Create('Horse not listen');
end;

class procedure THorseProvider<T>.DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass :=  THorseWebModule;
end;

class procedure THorseProvider<T>.StopListen;
begin
  InternalStopListen;
end;

class function THorseProvider<T>.IsRunning: Boolean;
begin
  Result := FRunning;
end;

class procedure THorseProvider<T>.Listen;
begin
  InternalListen;;
end;

class procedure THorseProvider<T>.Listen(const APort: Integer; const AHost: string; const ACallbackListen: TProc<T>; const ACallbackStopListen: TProc<T>);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallbackListen);
  SetOnStopListen(ACallbackStopListen);
  Listen;
end;

class procedure THorseProvider<T>.Listen(const AHost: string; const ACallbackListen: TProc<T>; const ACallbackStopListen: TProc<T>);
begin
  Listen(FPort, AHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider<T>.Listen(const ACallbackListen: TProc<T>; const ACallbackStopListen: TProc<T>);
begin
  Listen(FPort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider<T>.Listen(const APort: Integer; const ACallbackListen: TProc<T>; const ACallbackStopListen: TProc<T>);
begin
  Listen(APort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider<T>.SetHost(const AValue: string);
begin
  FHost := AValue.Trim;
end;

class procedure THorseProvider<T>.SetListenQueue(const AValue: Integer);
begin
  FListenQueue := AValue;
end;

class procedure THorseProvider<T>.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;
{$ENDIF}

end.
