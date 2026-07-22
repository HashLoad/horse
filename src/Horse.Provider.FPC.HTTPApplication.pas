unit Horse.Provider.FPC.HTTPApplication;

{ PATCH-FPCHTTP-1: ListenWithConfig override — same root cause as PATCH-CONSOLE-1. }

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(FPC)}
uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fphttpapp,
  Horse.Provider.Abstract,
  Horse.Provider.Config,
  Horse.Constants,
  Horse.Proc;

type
  THorseProvider = class(THorseProviderAbstract)
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
    class procedure DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
    {$IF FPC_FULLVERSION >= 30301}
    class procedure EnableServerKeepAlive(const AApplication: THTTPApplication);
    {$ENDIF}
  public
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class property ListenQueue: Integer read GetListenQueue write SetListenQueue;
    class function GetActivePort: Integer; override;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0';
      const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallbackListen: TProc;
      const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallbackListen: TProc = nil;
      const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: TProc;
      const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    // PATCH-FPCHTTP-1
    class procedure ListenWithConfig(const APort: Integer;
      const AConfig: THorseCrossSocketConfig); override;
    class function IsRunning: Boolean;
  end;
{$ENDIF}

implementation

{$IF DEFINED(FPC)}

uses
  Horse.WebModule,
  Horse.Response
  {$IF FPC_FULLVERSION >= 30301}, custhttpapp{$ENDIF};

{$IF FPC_FULLVERSION >= 30301}
const
  { Per-request keep-alive lifetime handed to the embedded fphttpserver so its
    threaded connection loop (TFPHTTPConnectionThread) actually reuses sockets
    instead of closing after one request. Reset after every request. }
  DEFAULT_KEEPALIVE_TIMEOUT_MS = 15000;

type
  { descendants declared in this unit so protected members are reachable
    regardless of their visibility in fcl-web:
    - THorseHTTPServerHandlerAccess exposes HTTPServer;
    - THorseEmbeddedServerAccess exposes KeepConnections, writable only in
      the protected TFPCustomHttpServer base (TEmbeddedHttpServer descends
      from it, so the property never becomes public on it). }
  THorseHTTPServerHandlerAccess = class(custhttpapp.TFPHTTPServerHandler);
  THorseEmbeddedServerAccess = class(custhttpapp.TEmbeddedHttpServer);

class procedure THorseProvider.EnableServerKeepAlive(const AApplication: THTTPApplication);
var
  LHandler: TFPHTTPServerHandler;
  LServer: TEmbeddedHttpServer;
begin
  { FServer is created in the TFPHTTPServerHandler constructor (its own
    getters read FServer with no nil-check, and the app sets Port/Threaded
    through them before Run without crashing), so by the time the handler
    exists the embedded server does too. }
  LHandler := AApplication.HTTPHandler;
  if LHandler = nil then
    Exit;
  LServer := THorseHTTPServerHandlerAccess(LHandler).HTTPServer;
  if LServer <> nil then
  begin
    { KeepConnections alone is necessary but NOT sufficient in threaded mode
      (Threaded := True → TFPHTTPConnectionThread): its keep-alive loop is
      gated on BOTH AllowNewRequest (needs KeepConnections) AND WaitUntil>0,
      and WaitUntil is 0 whenever KeepConnectionTimeout <= 0 (the default) —
      so the thread handles exactly one request and exits, closing the
      socket, no matter what KeepConnections says. A positive
      KeepConnectionTimeout is what actually keeps the connection looping.
      The window resets after every request (SetWaitUntil is called each
      iteration), so this is the per-request keep-alive lifetime, not a hard
      total cap. }
    THorseEmbeddedServerAccess(LServer).KeepConnections := True;
    if THorseEmbeddedServerAccess(LServer).KeepConnectionTimeout <= 0 then
      THorseEmbeddedServerAccess(LServer).KeepConnectionTimeout := DEFAULT_KEEPALIVE_TIMEOUT_MS;
  end;
end;
{$ENDIF}

class function THorseProvider.GetDefaultHTTPApplication: THTTPApplication;
begin
  if HTTPApplicationIsNil then
    FHTTPApplication := Application;
  Result := FHTTPApplication;
end;

class function THorseProvider.HTTPApplicationIsNil: Boolean;
begin
  Result := FHTTPApplication = nil;
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

class function THorseProvider.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

class function THorseProvider.GetPort: Integer;
begin
  Result := FPort;
end;

class function THorseProvider.GetActivePort: Integer;
begin
  Result := FPort;
end;

class procedure THorseProvider.InternalListen;
var
  LHTTPApplication: THTTPApplication;
begin
  TriggerBeforeListen;
  inherited;
  if FPort <= 0 then
    FPort := GetDefaultPort;
  if FHost.IsEmpty then
    FHost := GetDefaultHost;
  if FListenQueue = 0 then
    FListenQueue := 15;
  LHTTPApplication := GetDefaultHTTPApplication;
  LHTTPApplication.AllowDefaultModule := True;
  LHTTPApplication.OnGetModule := DoGetModule;
  LHTTPApplication.Threaded := True;
  LHTTPApplication.QueueSize := FListenQueue;
  LHTTPApplication.Port := FPort;
  LHTTPApplication.LegacyRouting := True;
  LHTTPApplication.Address := FHost;
  LHTTPApplication.Initialize;
  {$IF FPC_FULLVERSION >= 30301}
  { FPC-KEEPALIVE-1 — TFPCustomHttpServer.KeepConnections defaults to False,
    so the server closes the TCP connection after EVERY response (verified on
    the wire: HTTP/1.1, no Connection header, immediate close). Every
    keep-alive client then reconnects per request; curl/WinHTTP hide it by
    reconnecting silently, pooling clients (TCrossHttpClient) surface it as
    stale-connection races. Enabling it restores normal HTTP/1.1 semantics.
    Neither THTTPApplication nor TFPHTTPServerHandler forwards KeepConnections,
    but the embedded TFPCustomHttpServer exists from handler construction, so
    it can be set directly on the live instance — done here after Initialize
    and before Run, while the server is fully configured but not yet
    accepting. }
  EnableServerKeepAlive(LHTTPApplication);
  {$ENDIF}
  FRunning := True;
  DoOnListen;
  LHTTPApplication.Run;
end;

class procedure THorseProvider.DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass := THorseWebModule;
end;

class function THorseProvider.IsRunning: Boolean;
begin
  Result := FRunning;
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

class procedure THorseProvider.Listen(const APort: Integer; const ACallbackListen, ACallbackStopListen: TProc);
begin
  Listen(APort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider.Listen(const AHost: string; const ACallbackListen, ACallbackStopListen: TProc);
begin
  Listen(FPort, AHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider.Listen(const ACallbackListen, ACallbackStopListen: TProc);
begin
  Listen(FPort, FHost, ACallbackListen, ACallbackStopListen);
end;

// PATCH-FPCHTTP-1
class procedure THorseProvider.ListenWithConfig(const APort: Integer;
  const AConfig: THorseCrossSocketConfig);
begin
  SetPort(APort);
  InternalListen;
end;

class procedure THorseProvider.SetHost(const AValue: string);
begin
  FHost := AValue;
end;

class procedure THorseProvider.SetListenQueue(const AValue: Integer);
begin
  FListenQueue := AValue;
end;

class procedure THorseProvider.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;
{$ENDIF}

end.
