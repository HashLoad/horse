unit Horse.Provider.Daemon;

interface

{$IF DEFINED(HORSE_DAEMON) AND NOT DEFINED(FPC)}
uses
  Horse.Provider.Abstract,
  Horse.Constants,
  Horse.Provider.IOHandleSSL.Contract,
  IdHTTPWebBrokerBridge,
  IdSSLOpenSSL,
  IdContext,
  Horse.Provider.IOHandleSSL,
  System.SyncObjs,
  System.SysUtils,
  Posix.SysTypes;

type
  THorseProvider = class(THorseProviderAbstract)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FMaxConnections: Integer;
    class var FListenQueue: Integer;
    class var FKeepConnectionAlive: Boolean;
    class var FIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
    class var FHorseProviderIOHandleSSL: IHorseProviderIOHandleSSL;
    class function GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
    class function GetDefaultHorseProviderIOHandleSSL: IHorseProviderIOHandleSSL;
    class function HTTPWebBrokerIsNil: Boolean;
    class procedure OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    class procedure OnQuerySSLPort(APort: Word; var VUseSSL: Boolean);
    class procedure SetListenQueue(const AValue: Integer); static;
    class procedure SetMaxConnections(const AValue: Integer); static;
    class procedure SetPort(const AValue: Integer); static;
    class procedure SetIOHandleSSL(const AValue: IHorseProviderIOHandleSSL); static;
    class procedure SetHost(const AValue: string); static;
    class procedure SetKeepConnectionAlive(const AValue: Boolean); static;
    class function GetKeepConnectionAlive: Boolean; static;
    class function GetListenQueue: Integer; static;
    class function GetMaxConnections: Integer; static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class function GetIOHandleSSL: IHorseProviderIOHandleSSL; static;
    class function GetHost: string; static;
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
    class destructor UnInitialize;
  end;

var
  FEvent: TEvent;
  FRunning: Boolean;
  FPID: pid_t;
  FId: Integer;

const
  EXIT_FAILURE = 1;
  EXIT_SUCCESS = 0;

procedure HandleSignals(SigNum: Integer); cdecl;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_DAEMON) AND NOT DEFINED(FPC)}
uses
  Web.WebReq,
  Horse.WebModule,
  IdCustomTCPServer,
  Posix.Stdlib,
  Posix.SysStat,
  Posix.Unistd,
  Posix.Signal,
  Posix.Fcntl,
  ThirdParty.Posix.Syslog;

procedure HandleSignals(SigNum: Integer); cdecl;
begin
  case SigNum of
    SIGTERM:
      begin
        FRunning := False;
        FEvent.SetEvent;
      end;
    SIGHUP:
      begin
        Syslog(LOG_NOTICE, 'daemon: reloading config');
      end;
  end;
end;

class function THorseProvider.GetKeepConnectionAlive: Boolean;
begin
  Result := FKeepConnectionAlive;
end;

class procedure THorseProvider.SetKeepConnectionAlive(const AValue: Boolean);
begin
  FKeepConnectionAlive := AValue;
end;

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
  LIOHandleSSL.OnGetPassword := AHorseProviderIOHandleSSL.OnGetPassword;
  AIdHTTPWebBrokerBridge.IOHandler := LIOHandleSSL;
end;

class procedure THorseProvider.InternalListen;
var
  LIdx: Integer;
  LIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
begin
  inherited;
  FEvent := TEvent.Create;
  try
    openlog(nil, LOG_PID or LOG_NDELAY, LOG_DAEMON);
    if getppid() > 1 then
    begin
      FPID := fork();
      if FPID < 0 then
        raise Exception.Create('Error forking the process');
      if FPID > 0 then
        Halt(EXIT_SUCCESS);
      if setsid() < 0 then
        raise Exception.Create('Impossible to create an independent session');
      Signal(SIGCHLD, TSignalHandler(SIG_IGN));
      Signal(SIGHUP, HandleSignals);
      Signal(SIGTERM, HandleSignals);
      FPID := fork();
      if FPID < 0 then
        raise Exception.Create('Error forking the process');
      if FPID > 0 then
        Halt(EXIT_SUCCESS);
      for LIdx := sysconf(_SC_OPEN_MAX) downto 0 do
        __close(LIdx);
      FId := __open('/dev/null', O_RDWR);
      dup(FId);
      dup(FId);
      umask(027);
      chdir('/');
    end;
    try
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
        Syslog(LOG_INFO, Format(START_RUNNING, [FHost, FPort]));
      except
        on E: Exception do
          Syslog(LOG_ERR, E.ClassName + ': ' + E.Message);
      end;
      while FRunning do
        FEvent.WaitFor();
      ExitCode := EXIT_SUCCESS;
    except
      on E: Exception do
      begin
        Syslog(LOG_ERR, 'Error: ' + E.Message);
        ExitCode := EXIT_FAILURE;
      end;
    end;
    Syslog(LOG_NOTICE, 'daemon stopped');
    closelog();
  finally
    FEvent.Free;
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
      FEvent.SetEvent;
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
  InternalListen;;
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
end;

initialization
  THorseProvider.SetKeepConnectionAlive(True);
{$ENDIF}

end.
