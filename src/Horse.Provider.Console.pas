unit Horse.Provider.Console;

interface

{$IF NOT DEFINED(FPC)}
uses
  Horse.Provider.Abstract,
  Horse.Constants,
  Horse.Provider.IOHandleSSL.Contract,
  Horse.Provider.IOHandleSSL,
  IdHTTPWebBrokerBridge,
  IdSSLOpenSSL,
  IdSSL,
  IdContext,
  System.Classes,
  System.SyncObjs,
  System.SysUtils;

type
  THorseProvider = class(THorseProviderAbstract)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FEvent: TEvent;
    class var FMaxConnections: Integer;
    class var FListenQueue: Integer;
    class var FKeepConnectionAlive: Boolean;
    class var FIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
    class var FHorseProviderIOHandleSSL: IHorseProviderIOHandleSSL;
    class function GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
    class function GetDefaultHorseProviderIOHandleSSL: IHorseProviderIOHandleSSL;
    class function GetDefaultEvent: TEvent;
    class function HTTPWebBrokerIsNil: Boolean;
    class procedure OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    class procedure OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
    class procedure SetListenQueue(const AValue: Integer); static;
    class procedure SetMaxConnections(const AValue: Integer); static;
    class procedure SetPort(const AValue: Integer); static;
    class procedure SetIOHandleSSL(const AValue: IHorseProviderIOHandleSSL); static;
    class procedure SetHost(const AValue: string); static;
    class procedure SetKeepConnectionAlive(const AValue: Boolean); static;
    class function GetListenQueue: Integer; static;
    class function GetMaxConnections: Integer; static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class function GetIOHandleSSL: IHorseProviderIOHandleSSL; static;
    class function GetHost: string; static;
    class function GetKeepConnectionAlive: Boolean; static;
    class procedure InternalListen; virtual;
    class procedure InternalStopListen; virtual;
    class procedure InitServerIOHandlerSSLOpenSSL(const AIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge; const AHorseProviderIOHandleSSL: IHorseProviderIOHandleSSL);
  public
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;
    class property ListenQueue: Integer read GetListenQueue write SetListenQueue;
    class property KeepConnectionAlive: Boolean read GetKeepConnectionAlive write SetKeepConnectionAlive;
    class property IOHandleSSL: IHorseProviderIOHandleSSL read GetIOHandleSSL write SetIOHandleSSL;
    class procedure StopListen; override;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0'; const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallbackListen: TProc; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: TProc; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class function IsRunning: Boolean;
    class destructor UnInitialize;
  end;
{$ENDIF}

implementation

{$IF NOT DEFINED(FPC)}
uses
  Web.WebReq,
  Horse.WebModule,
  IdCustomTCPServer;

class function THorseProvider.GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
begin
  if HTTPWebBrokerIsNil then
  begin
    FIdHTTPWebBrokerBridge := TIdHTTPWebBrokerBridge.Create(nil);
    FIdHTTPWebBrokerBridge.OnParseAuthentication := OnAuthentication;
    FIdHTTPWebBrokerBridge.OnQuerySSLPort := OnQuerySSLPort;
  end;
  Result := FIdHTTPWebBrokerBridge;
end;

class function THorseProvider.HTTPWebBrokerIsNil: Boolean;
begin
  Result := FIdHTTPWebBrokerBridge = nil;
end;

class procedure THorseProvider.OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
begin
  VUseSSL := (FHorseProviderIOHandleSSL <> nil) and (FHorseProviderIOHandleSSL.Active);
end;

class function THorseProvider.GetDefaultEvent: TEvent;
begin
  if FEvent = nil then
    FEvent := TEvent.Create;
  Result := FEvent;
end;

class function THorseProvider.GetDefaultHorseProviderIOHandleSSL: IHorseProviderIOHandleSSL;
begin
  if FHorseProviderIOHandleSSL = nil then
    FHorseProviderIOHandleSSL := THorseProviderIOHandleSSL.New;
  Result := FHorseProviderIOHandleSSL;
end;

class function THorseProvider.GetDefaultHost: string;
begin
  Result := DEFAULT_HOST;
end;

class function THorseProvider.GetDefaultPort: Integer;
begin
  Result := DEFAULT_PORT;
end;

class function THorseProvider.GetHost: string;
begin
  Result := FHost;
end;

class function THorseProvider.GetKeepConnectionAlive: Boolean;
begin
  Result := FKeepConnectionAlive;
end;

class function THorseProvider.IsRunning: Boolean;
begin
  Result := FRunning;
end;

class function THorseProvider.GetIOHandleSSL: IHorseProviderIOHandleSSL;
begin
  Result := GetDefaultHorseProviderIOHandleSSL;
end;

class function THorseProvider.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

class function THorseProvider.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

class function THorseProvider.GetPort: Integer;
begin
  Result := FPort;
end;

class procedure THorseProvider.InitServerIOHandlerSSLOpenSSL(const AIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge; const AHorseProviderIOHandleSSL: IHorseProviderIOHandleSSL);
var
  LIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
begin
  LIOHandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(AIdHTTPWebBrokerBridge);
  LIOHandleSSL.SSLOptions.CertFile := AHorseProviderIOHandleSSL.CertFile;
  LIOHandleSSL.SSLOptions.RootCertFile := AHorseProviderIOHandleSSL.RootCertFile;
  LIOHandleSSL.SSLOptions.KeyFile := AHorseProviderIOHandleSSL.KeyFile;
  LIOHandleSSL.SSLOptions.Method := AHorseProviderIOHandleSSL.Method;
  LIOHandleSSL.SSLOptions.SSLVersions := AHorseProviderIOHandleSSL.SSLVersions;
  LIOHandleSSL.SSLOptions.CipherList := AHorseProviderIOHandleSSL.CipherList;
  LIOHandleSSL.SSLOptions.DHParamsFile := AHorseProviderIOHandleSSL.DHParamsFile;
  LIOHandleSSL.OnGetPassword := AHorseProviderIOHandleSSL.OnGetPassword;
  AIdHTTPWebBrokerBridge.IOHandler := LIOHandleSSL;
end;

class procedure THorseProvider.InternalListen;
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

class procedure THorseProvider.InternalStopListen;
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

class procedure THorseProvider.StopListen;
begin
  InternalStopListen;
end;

class procedure THorseProvider.Listen;
begin
  InternalListen;
end;

class procedure THorseProvider.Listen(const APort: Integer; const AHost: string; const ACallbackListen, ACallbackStopListen: TProc);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallbackListen);
  SetOnStopListen(ACallbackStopListen);
  InternalListen;
end;

class procedure THorseProvider.Listen(const AHost: string; const ACallbackListen, ACallbackStopListen: TProc);
begin
  Listen(FPort, AHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider.Listen(const ACallbackListen, ACallbackStopListen: TProc);
begin
  Listen(FPort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider.Listen(const APort: Integer; const ACallbackListen, ACallbackStopListen: TProc);
begin
  Listen(APort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider.OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled := True;
end;

class procedure THorseProvider.SetHost(const AValue: string);
begin
  FHost := AValue.Trim;
end;

class procedure THorseProvider.SetKeepConnectionAlive(const AValue: Boolean);
begin
  FKeepConnectionAlive := AValue;
end;

class procedure THorseProvider.SetIOHandleSSL(const AValue: IHorseProviderIOHandleSSL);
begin
  FHorseProviderIOHandleSSL := AValue;
end;

class procedure THorseProvider.SetListenQueue(const AValue: Integer);
begin
  FListenQueue := AValue;
end;

class procedure THorseProvider.SetMaxConnections(const AValue: Integer);
begin
  FMaxConnections := AValue;
end;

class procedure THorseProvider.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

class destructor THorseProvider.UnInitialize;
begin
  FreeAndNil(FIdHTTPWebBrokerBridge);
  if FEvent <> nil then
    FreeAndNil(FEvent);
end;

initialization
  THorseProvider.SetKeepConnectionAlive(True);
{$ENDIF}

end.
