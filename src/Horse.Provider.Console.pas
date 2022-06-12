unit Horse.Provider.Console;

interface

{$IF NOT DEFINED(FPC)}
uses Horse.Provider.Abstract, Horse.Constants, Horse.Provider.IOHandleSSL, IdHTTPWebBrokerBridge, IdSSLOpenSSL, IdContext,
  System.Classes, System.SyncObjs, System.SysUtils;

type
  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FEvent: TEvent;
    class var FMaxConnections: Integer;
    class var FListenQueue: Integer;
    class var FKeepConnectionAlive: Boolean;
    class var FIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
    class var FHorseProviderIOHandleSSL: THorseProviderIOHandleSSL;
    class function GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
    class function GetDefaultHorseProviderIOHandleSSL: THorseProviderIOHandleSSL;
    class function GetDefaultEvent: TEvent;
    class function HTTPWebBrokerIsNil: Boolean;
    class procedure OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    class procedure OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
    class procedure SetListenQueue(const AValue: Integer); static;
    class procedure SetMaxConnections(const AValue: Integer); static;
    class procedure SetPort(const AValue: Integer); static;
    class procedure SetIOHandleSSL(const AValue: THorseProviderIOHandleSSL); static;
    class procedure SetHost(const AValue: string); static;
    class procedure SetKeepConnectionAlive(const AValue: Boolean); static;
    class function GetListenQueue: Integer; static;
    class function GetMaxConnections: Integer; static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class function GetIOHandleSSL: THorseProviderIOHandleSSL; static;
    class function GetHost: string; static;
    class function GetKeepConnectionAlive: Boolean; static;
    class procedure InternalListen; virtual;
    class procedure InternalStopListen; virtual;
    class procedure InitServerIOHandlerSSLOpenSSL(const AIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge; const AHorseProviderIOHandleSSL: THorseProviderIOHandleSSL);
  public
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;
    class property ListenQueue: Integer read GetListenQueue write SetListenQueue;
    class property KeepConnectionAlive: Boolean read GetKeepConnectionAlive write SetKeepConnectionAlive;
    class property IOHandleSSL: THorseProviderIOHandleSSL read GetIOHandleSSL write SetIOHandleSSL;
    class procedure StopListen; override;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0'; const ACallbackListen: TProc<T> = nil; const ACallbackStopListen: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallbackListen: TProc<T>; const ACallbackStopListen: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallbackListen: TProc<T> = nil; const ACallbackStopListen: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: TProc<T>; const ACallbackStopListen: TProc<T> = nil); reintroduce; overload; static;
    class function IsRunning: Boolean;
    class destructor UnInitialize;
  end;
{$ENDIF}

implementation

{$IF NOT DEFINED(FPC)}
uses Web.WebReq, Horse.WebModule, IdCustomTCPServer;

class function THorseProvider<T>.GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
begin
  if HTTPWebBrokerIsNil then
  begin
    FIdHTTPWebBrokerBridge := TIdHTTPWebBrokerBridge.Create(nil);
    FIdHTTPWebBrokerBridge.OnParseAuthentication := OnAuthentication;
    FIdHTTPWebBrokerBridge.OnQuerySSLPort := OnQuerySSLPort;
  end;
  Result := FIdHTTPWebBrokerBridge;
end;

class function THorseProvider<T>.HTTPWebBrokerIsNil: Boolean;
begin
  Result := FIdHTTPWebBrokerBridge = nil;
end;

class procedure THorseProvider<T>.OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
begin
  VUseSSL := (FHorseProviderIOHandleSSL <> nil) and (FHorseProviderIOHandleSSL.Active);
end;

class function THorseProvider<T>.GetDefaultEvent: TEvent;
begin
  if FEvent = nil then
    FEvent := TEvent.Create;
  Result := FEvent;
end;

class function THorseProvider<T>.GetDefaultHorseProviderIOHandleSSL: THorseProviderIOHandleSSL;
begin
  if FHorseProviderIOHandleSSL = nil then
    FHorseProviderIOHandleSSL := THorseProviderIOHandleSSL.Create;
  Result := FHorseProviderIOHandleSSL;
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

class function THorseProvider<T>.GetKeepConnectionAlive: Boolean;
begin
  Result := FKeepConnectionAlive;
end;

class function THorseProvider<T>.IsRunning: Boolean;
begin
  Result := FRunning;
end;

class function THorseProvider<T>.GetIOHandleSSL: THorseProviderIOHandleSSL;
begin
  Result := GetDefaultHorseProviderIOHandleSSL;
end;

class function THorseProvider<T>.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

class function THorseProvider<T>.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

class function THorseProvider<T>.GetPort: Integer;
begin
  Result := FPort;
end;

class procedure THorseProvider<T>.InitServerIOHandlerSSLOpenSSL(const AIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge; const AHorseProviderIOHandleSSL: THorseProviderIOHandleSSL);
var
  LIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
begin
  LIOHandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(AIdHTTPWebBrokerBridge);
  LIOHandleSSL.SSLOptions.CertFile := AHorseProviderIOHandleSSL.CertFile;
  LIOHandleSSL.SSLOptions.RootCertFile := AHorseProviderIOHandleSSL.RootCertFile;
  LIOHandleSSL.SSLOptions.KeyFile := AHorseProviderIOHandleSSL.KeyFile;
  LIOHandleSSL.SSLOptions.Method := AHorseProviderIOHandleSSL.Method;
  LIOHandleSSL.SSLOptions.SSLVersions := AHorseProviderIOHandleSSL.SSLVersions;
  LIOHandleSSL.OnGetPassword := AHorseProviderIOHandleSSL.OnGetPassword;
  AIdHTTPWebBrokerBridge.IOHandler := LIOHandleSSL;
end;

class procedure THorseProvider<T>.InternalListen;
var
  LAttach: string;
  LIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
begin
  inherited;
  if FPort <= 0 then
    FPort := GetDefaultPort;

  if FHost.IsEmpty then
    FHost := GetDefaultHost;

  LIdHTTPWebBrokerBridge := GetDefaultHTTPWebBroker;
  WebRequestHandler.WebModuleClass := WebModuleClass;
  try
    if FMaxConnections > 0 then
    begin
      WebRequestHandler.MaxConnections := FMaxConnections;
      GetDefaultHTTPWebBroker.MaxConnections := FMaxConnections;
    end;

    if FListenQueue = 0 then
      FListenQueue := IdListenQueueDefault;

    if FHorseProviderIOHandleSSL <> nil then
      InitServerIOHandlerSSLOpenSSL(LIdHTTPWebBrokerBridge, GetDefaultHorseProviderIOHandleSSL);
    LIdHTTPWebBrokerBridge.ListenQueue := FListenQueue;

    LIdHTTPWebBrokerBridge.Bindings.Clear;
    if FHost <> GetDefaultHost then
    begin
      LIdHTTPWebBrokerBridge.Bindings.Add;
      LIdHTTPWebBrokerBridge.Bindings.Items[0].IP := FHost;
      LIdHTTPWebBrokerBridge.Bindings.Items[0].Port := FPort;
    end;

    LIdHTTPWebBrokerBridge.KeepAlive := FKeepConnectionAlive;
    LIdHTTPWebBrokerBridge.DefaultPort := FPort;
    LIdHTTPWebBrokerBridge.Active := True;
    LIdHTTPWebBrokerBridge.StartListening;
    FRunning := True;
    DoOnListen;

    if IsConsole then
    begin
      while FRunning do
        GetDefaultEvent.WaitFor();
    end
  except
    on E: Exception do
    begin
      if IsConsole then
      begin
        Writeln(E.ClassName, ': ', E.Message);
        Read(LAttach);
      end
      else
      {$IF CompilerVersion >= 32.0}
        raise AcquireExceptionObject;
      {$ELSE}
        raise;
      {$ENDIF}
    end;
  end;
end;

class procedure THorseProvider<T>.InternalStopListen;
begin
  if not HTTPWebBrokerIsNil then
  begin
    GetDefaultHTTPWebBroker.StopListening;
    GetDefaultHTTPWebBroker.Active := False;
    DoOnStopListen;
    FRunning := False;
    if FEvent <> nil then
      GetDefaultEvent.SetEvent;
  end
  else
    raise Exception.Create('Horse not listen');
end;

class procedure THorseProvider<T>.StopListen;
begin
  InternalStopListen;
end;

class procedure THorseProvider<T>.Listen;
begin
  InternalListen;
end;

class procedure THorseProvider<T>.Listen(const APort: Integer; const AHost: string; const ACallbackListen, ACallbackStopListen: TProc<T>);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallbackListen);
  SetOnStopListen(ACallbackStopListen);
  InternalListen;
end;

class procedure THorseProvider<T>.Listen(const AHost: string; const ACallbackListen, ACallbackStopListen: TProc<T>);
begin
  Listen(FPort, AHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider<T>.Listen(const ACallbackListen, ACallbackStopListen: TProc<T>);
begin
  Listen(FPort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider<T>.Listen(const APort: Integer; const ACallbackListen, ACallbackStopListen: TProc<T>);
begin
  Listen(APort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider<T>.OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled := True;
end;

class procedure THorseProvider<T>.SetHost(const AValue: string);
begin
  FHost := AValue.Trim;
end;

class procedure THorseProvider<T>.SetKeepConnectionAlive(const AValue: Boolean);
begin
  FKeepConnectionAlive := AValue;
end;

class procedure THorseProvider<T>.SetIOHandleSSL(const AValue: THorseProviderIOHandleSSL);
begin
  FHorseProviderIOHandleSSL := AValue;
end;

class procedure THorseProvider<T>.SetListenQueue(const AValue: Integer);
begin
  FListenQueue := AValue;
end;

class procedure THorseProvider<T>.SetMaxConnections(const AValue: Integer);
begin
  FMaxConnections := AValue;
end;

class procedure THorseProvider<T>.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

class destructor THorseProvider<T>.UnInitialize;
begin
  FreeAndNil(FIdHTTPWebBrokerBridge);
  if FEvent <> nil then
    FreeAndNil(FEvent);
  if FHorseProviderIOHandleSSL <> nil then
    FreeAndNil(FHorseProviderIOHandleSSL);
end;
{$ENDIF}

end.
