unit Horse.Provider.FPC.HTTPApplication;

{ PATCH-FPCHTTP-1: ListenWithConfig override — same root cause as PATCH-CONSOLE-1. }
{ PATCH-FPCHTTP-2: TCP_NODELAY on every accepted connection via OnAllowConnect.
  fphttpserver never calls fpSetSockOpt(TCP_NODELAY) on accepted sockets; without
  it the classic Nagle + delayed-ACK interaction can add ~40 ms per request on
  non-loopback links (server holds the last segment until Nagle flushes, client
  waits for that ACK before sending the next request).
  Fix: hook TSocketServer.OnAllowConnect (fires immediately after fpAccept, before
  the connection thread is created) and set TCP_NODELAY on the raw descriptor.
  BENCH-FPCHTTP-1 confirmed (2026-08-28): re-test with both keepalive+TCP_NODELAY
  compiled correctly showed the same 44 ms stall.  TCP_NODELAY had no effect —
  the root cause is fphttpserver's keepalive loop poll interval (~40 ms fixed),
  NOT Nagle.  TCP_NODELAY is retained: harmless with KeepConnections=False and
  genuinely useful on non-loopback links (VMs, containers, production hosts).
  Guards: FPC >= 3.3.1 (custhttpapp needed to reach the embedded server);
          UNIX (fpSetSockOpt path; non-UNIX body is a documented no-op). }

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
    { PATCH-FPCHTTP-2 }
    class procedure EnableServerNoDelay(const AApplication: THTTPApplication);
    class procedure SetNoDelayOnAccept(Sender: TObject; ASocket: Longint; var Allow: Boolean);
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
  {$IF FPC_FULLVERSION >= 30301}, custhttpapp{$ENDIF}
  {$IFDEF UNIX}, Sockets{$ENDIF};

{$IF FPC_FULLVERSION >= 30301}
const
  DEFAULT_KEEPALIVE_TIMEOUT_MS = 15000;
  {$IFDEF UNIX}
  { TCP_NODELAY = 1 on every POSIX platform (Linux/macOS/FreeBSD/Solaris).
    Declared here to avoid pulling in platform-specific units (linux.pp / bsd.pp)
    just for this one constant — the value is mandated by POSIX and never changes. }
  HORSE_TCP_NODELAY = 1;
  {$ENDIF}

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

{ PATCH-FPCHTTP-2 — TCP_NODELAY on each accepted connection.
  TSocketServer.OnAllowConnect fires immediately after fpAccept() returns the
  raw descriptor, before CreateStream wraps it and before the connection thread
  starts.  Setting TCP_NODELAY here applies it to every accepted socket without
  subclassing TEmbeddedHttpServer.  The Allow parameter is intentionally left
  unchanged (default True) — this hook is used only for the setsockopt call. }
{$IFDEF UNIX}
class procedure THorseProvider.SetNoDelayOnAccept(Sender: TObject; ASocket: Longint; var Allow: Boolean);
var
  LNoDelay: LongInt;
begin
  LNoDelay := 1;
  fpSetSockOpt(ASocket, IPPROTO_TCP, HORSE_TCP_NODELAY, @LNoDelay, SizeOf(LNoDelay));
end;
{$ELSE}
class procedure THorseProvider.SetNoDelayOnAccept(Sender: TObject; ASocket: Longint; var Allow: Boolean);
begin
  { TCP_NODELAY via fpSetSockOpt is a POSIX path; Windows FPC would need
    WinSock2.setsockopt. The Nagle stall was only measured on Linux loopback,
    so this non-UNIX branch is a documented no-op for now. }
end;
{$ENDIF}

class procedure THorseProvider.EnableServerNoDelay(const AApplication: THTTPApplication);
var
  LHandler: TFPHTTPServerHandler;
  LServer: TEmbeddedHttpServer;
begin
  LHandler := AApplication.HTTPHandler;
  if LHandler = nil then
    Exit;
  LServer := THorseHTTPServerHandlerAccess(LHandler).HTTPServer;
  { OnAllowConnect is PROTECTED on TFPCustomHttpServer (forwarded from the
    inner TInetServer but not re-published as public by TEmbeddedHttpServer).
    Use the friend class, the same pattern as KeepConnections / KeepConnectionTimeout.
    TSocketServer.Accept calls it immediately after fpAccept and before
    TSocketStream is created — the raw descriptor is valid for setsockopt. }
  if LServer <> nil then
    THorseEmbeddedServerAccess(LServer).OnAllowConnect := THorseProvider.SetNoDelayOnAccept;
end;
{$ENDIF} // FPC_FULLVERSION >= 30301

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
  { FPC-KEEPALIVE-1 / BENCH-FPCHTTP-1 — keepalive REMOVED (confirmed root cause).
    Sequence of events:
      1. FPC-KEEPALIVE-1 (earlier): KeepConnections=True + KeepConnectionTimeout=15000
         added to prevent stale-connection races with persistent clients.
      2. First P1 bench (2026-08-28): 44 ms stall appeared; TCP_NODELAY was also
         added (PATCH-FPCHTTP-2) but the build used a STALE PPU — TCP_NODELAY never
         compiled in.  Removed keepalive, dropped to 0.501 ms.
      3. Second P1 bench (2026-08-28): re-enabled keepalive with TCP_NODELAY compiled
         correctly via the friend-class fix.  Stall STILL 44 ms — Nagle ruled out.
    Root cause: fphttpserver's TFPHTTPConnectionThread keepalive loop calls
    select(fd, ~40 ms) between requests to poll for graceful-shutdown signals.
    Every response cycle waits one full poll interval even when the next request
    is already queued.  This is a fixed constant in the fphttpserver source;
    without patching fphttpserver itself it cannot be reduced.
    Consequence: KeepConnections=False (the default).  The server closes the TCP
    connection after each response without advertising Connection: close.
    Persistent-connection clients that reuse the socket receive ECONNRESET;
    TCrossHttpClient handles this via PATCH-CSHTTP-3 (one retry on stale reuse).
    Other clients (h2load, curl) reconnect transparently.  Per-request TCP
    connect + OS thread creation cost: ~0.5 ms on loopback.  This is the correct
    trade-off — a consistent 0.5 ms is far better than a consistent 44 ms. }
  { PATCH-FPCHTTP-2 — TCP_NODELAY on every accepted socket (Nagle ruled out as the
    44 ms stall cause; retained for non-loopback deployments). }
  EnableServerNoDelay(LHTTPApplication);
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
