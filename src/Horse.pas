unit Horse;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{ ===========================================================================
  BACKWARDS-COMPATIBILITY CONTRACT (PATCH-HORSE-2)
  ---------------------------------------------------------------------------
  Every existing Horse project and middleware compiled against an earlier
  Horse release continues to compile and run identically under PATCH-HORSE-2.
  Future maintainers: do NOT modify this file without re-validating each
  guarantee below.

  GUARANTEED PRESERVED:

  G1. Legacy define names continue to work as before. Setting any of:
        HORSE_CROSSSOCKET, HORSE_VCL, HORSE_DAEMON, HORSE_LCL,
        HORSE_APACHE,      HORSE_ISAPI, HORSE_CGI, HORSE_FCGI,
        HORSE_NOPROVIDER
      selects the same concrete THorseProvider class as the pre-PATCH-HORSE-2
      Horse.pas did. The alias block below translates each old define to its
      new namespaced counterpart before the chain runs.

  G2. Default-provider paths are unchanged.
        - Delphi + no HORSE_* defines  →  Horse.Provider.Console.THorseProvider
        - FPC    + no HORSE_* defines  →  Horse.Provider.FPC.HTTPApplication.THorseProvider
        - HORSE_NOPROVIDER             →  Horse.Provider.Abstract.THorseProviderAbstract

  G3. THorseRequest / THorseResponse public API is untouched by PATCH-HORSE-2.
      Middleware that uses Req.Body, Req.Headers, Req.Params, Req.Query,
      Req.Cookie, Res.Send, Res.Status, Res.AddHeader, Req.RawWebRequest,
      Res.RawWebResponse, etc. behaves identically.

  G4. The hybrid-adapter interfaces IHorseRawRequest / IHorseRawResponse
      and their generic adapters TInterfacedWebRequest / TInterfacedWebResponse
      are untouched. Middleware that pokes Req.RawWebRequest.* on the
      CrossSocket path keeps working.

  G5. The existing Horse.Provider.CrossSocket.pas (console-shape) is the
      unit selected when only HORSE_PROVIDER_CROSSSOCKET / HORSE_CROSSSOCKET
      is set with no HORSE_APPTYPE_*. Existing projects pick up no new
      behaviour unless they explicitly opt into a HORSE_APPTYPE_* combination.

  G6. PATCH-HORSE-1's compile-time guard rejects only architecturally-
      impossible combinations: HORSE_PROVIDER_* × HORSE_HOST_*, VCL-on-FPC,
      LCL-on-Delphi, ISAPI-on-FPC, and HORSE_NOPROVIDER × anything else.
      Combinations that the old chain rejected for *implementation* reasons
      (HORSE_CROSSSOCKET + HORSE_VCL/DAEMON/LCL) are now expressible via the
      cross-product Provider units in horse-provider-crosssocket — these are
      ADDITIONS, never reductions of the working surface.

  G7. No existing concrete provider class (Horse.Provider.Console / VCL /
      Daemon / Apache / ISAPI / CGI / FPC.*) is renamed or removed. All
      class methods (Listen, StopListen, ListenWithConfig, Execute) keep
      their existing signatures.

  G8. boss.json: the horse-provider-crosssocket package's version bump
      (1.0.4 → 1.0.5) requires horse >= 3.1.98 only for consumers that
      explicitly upgrade. Consumers pinned to earlier versions continue
      working against the pre-PATCH-HORSE-2 horse.

  HOW TO VERIFY: see patches/horse-provider-crosssocket/samples/tests/
  HorseCSTestServer.dpr (legacy baseline with HORSE_CROSSSOCKET) — must
  still pass 88/89 against the unmodified HorseCSTestClient.
  =========================================================================== }

{ ===========================================================================
  PATCH-HORSE-2 — Define-namespace normalisation (three orthogonal axes)
  ---------------------------------------------------------------------------
  The Horse compile-time configuration is split into THREE explicit axes:

    A · Provider          HORSE_PROVIDER_*  — HTTP transport library
                          Indy        (Delphi default — implicit)
                          fphttpserver(FPC default    — implicit)
                          CrossSocket (HORSE_PROVIDER_CROSSSOCKET)
                          mORMot      (HORSE_PROVIDER_MORMOT)
                          OverbyteICS (HORSE_PROVIDER_ICS)
                                      v1: Delphi + Windows only.
                                      Selecting it on FPC or non-Windows
                                      triggers a FATAL below.

    B · Application type  HORSE_APPTYPE_*   — binary lifecycle shape
                          Console     (default — implicit)
                          VCL         (HORSE_APPTYPE_VCL — Delphi only)
                          Daemon      (HORSE_APPTYPE_DAEMON)
                          LCL         (HORSE_APPTYPE_LCL — FPC only)
                          HTTPApp     (FPC default — implicit)

    C · Host-managed      HORSE_HOST_*      — web server owns the socket
                          Apache      (HORSE_HOST_APACHE)
                          ISAPI       (HORSE_HOST_ISAPI — Delphi only)
                          CGI         (HORSE_HOST_CGI)
                          FCGI        (HORSE_HOST_FCGI — FPC only)

  Axis C wins outright when set — no Provider is involved (the host IS the
  transport). Axis A and Axis B compose freely (subject to platform support):
  any HORSE_PROVIDER_* can combine with any HORSE_APPTYPE_*, with the
  cross-product unit selected by the chain below.

  See doc/providers.md for the full architectural model.

  ---------------------------------------------------------------------------
  Legacy define aliases — full backwards compatibility for Horse <3.2 code.
  Setting an old single-purpose define is equivalent to setting its new
  namespaced counterpart. Every existing .dproj / .lpi continues to compile.
  =========================================================================== }
{$IFDEF HORSE_CROSSSOCKET} {$DEFINE HORSE_PROVIDER_CROSSSOCKET} {$ENDIF}
{$IFDEF HORSE_VCL}         {$DEFINE HORSE_APPTYPE_VCL}          {$ENDIF}
{$IFDEF HORSE_DAEMON}      {$DEFINE HORSE_APPTYPE_DAEMON}       {$ENDIF}
{$IFDEF HORSE_LCL}         {$DEFINE HORSE_APPTYPE_LCL}          {$ENDIF}
{$IFDEF HORSE_APACHE}      {$DEFINE HORSE_HOST_APACHE}          {$ENDIF}
{$IFDEF HORSE_ISAPI}       {$DEFINE HORSE_HOST_ISAPI}           {$ENDIF}
{$IFDEF HORSE_CGI}         {$DEFINE HORSE_HOST_CGI}             {$ENDIF}
{$IFDEF HORSE_FCGI}        {$DEFINE HORSE_HOST_FCGI}            {$ENDIF}

{ ===========================================================================
  PATCH-HORSE-1 — Architecturally-impossible combination guard (expanded)
  ---------------------------------------------------------------------------
  Originally PATCH-HORSE-1 rejected any HORSE_CROSSSOCKET combined with any
  other provider define. PATCH-HORSE-2's three-axis model permits orthogonal
  Provider × Application-type combinations, so the guard narrows to the
  combinations that remain genuinely impossible:

    1. Self-hosted Provider × Host-managed runtime
       The host owns the socket; a self-hosted transport cannot coexist.
       (HORSE_PROVIDER_* × HORSE_HOST_*)
    2. Cross-platform Application-type mismatch
       VCL is Delphi-only; LCL is FPC/Lazarus-only. Setting the wrong one
       on the wrong compiler is a project-configuration error.
    3. HORSE_NOPROVIDER × anything else
       The escape hatch (compile without any provider) is exclusive.
  =========================================================================== }

{ Rule 1 — self-hosted Provider × host-managed runtime }
{$IF DEFINED(HORSE_PROVIDER_CROSSSOCKET)}
  {$IF DEFINED(HORSE_HOST_ISAPI)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_CROSSSOCKET cannot combine with HORSE_HOST_ISAPI — IIS owns the socket; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_HOST_APACHE)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_CROSSSOCKET cannot combine with HORSE_HOST_APACHE — Apache owns the socket; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_HOST_CGI)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_CROSSSOCKET cannot combine with HORSE_HOST_CGI — the web server owns the socket; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_HOST_FCGI)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_CROSSSOCKET cannot combine with HORSE_HOST_FCGI — FastCGI talks to a web server; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
{$IFEND}
{$IF DEFINED(HORSE_PROVIDER_MORMOT)}
  {$IF DEFINED(HORSE_HOST_ISAPI)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_MORMOT cannot combine with HORSE_HOST_ISAPI — IIS owns the socket; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_HOST_APACHE)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_MORMOT cannot combine with HORSE_HOST_APACHE — Apache owns the socket; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_HOST_CGI)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_MORMOT cannot combine with HORSE_HOST_CGI — the web server owns the socket; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_HOST_FCGI)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_MORMOT cannot combine with HORSE_HOST_FCGI — FastCGI talks to a web server; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$DEFINE FPC_HAS_EXPLICIT_INTERLOCKED_POINTER}
{$IFEND}
{$IF DEFINED(HORSE_PROVIDER_ICS)}
  {$IF DEFINED(HORSE_HOST_ISAPI)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_ICS cannot combine with HORSE_HOST_ISAPI — IIS owns the socket; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_HOST_APACHE)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_ICS cannot combine with HORSE_HOST_APACHE — Apache owns the socket; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_HOST_CGI)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_ICS cannot combine with HORSE_HOST_CGI — the web server owns the socket; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_HOST_FCGI)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_ICS cannot combine with HORSE_HOST_FCGI — FastCGI talks to a web server; a self-hosted Provider cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(FPC)}
    {$MESSAGE FATAL 'HORSE_PROVIDER_ICS is Delphi-only. ICS POSIX support (Ics.Posix.*) rides the Delphi POSIX RTL; FPC/Lazarus support is deferred until drapid/ICS_Lazarus is validated against ICS 9.7.'}
  {$ENDIF}
  {$IF NOT (DEFINED(MSWINDOWS) OR DEFINED(POSIX))}
    {$MESSAGE FATAL 'HORSE_PROVIDER_ICS supports Windows and Delphi POSIX (Linux64 / macOS) targets only.'}
  {$IFEND}
{$IFEND}

{ Rule 2 — cross-platform Application-type mismatch }
{$IF DEFINED(HORSE_APPTYPE_VCL) and DEFINED(FPC)}
  {$MESSAGE FATAL 'HORSE_APPTYPE_VCL is Delphi-only — use HORSE_APPTYPE_LCL for Lazarus/FPC.'}
{$IFEND}
{$IF DEFINED(HORSE_APPTYPE_LCL) and not DEFINED(FPC)}
  {$MESSAGE FATAL 'HORSE_APPTYPE_LCL is FPC/Lazarus-only — use HORSE_APPTYPE_VCL for Delphi.'}
{$IFEND}
{$IF DEFINED(HORSE_HOST_ISAPI) and DEFINED(FPC)}
  {$MESSAGE FATAL 'HORSE_HOST_ISAPI is Delphi-only — IIS ISAPI extensions require Delphi.'}
{$IFEND}

{ Rule 3 — HORSE_NOPROVIDER × anything else }
{$IF DEFINED(HORSE_NOPROVIDER)}
  {$IF DEFINED(HORSE_PROVIDER_CROSSSOCKET) or DEFINED(HORSE_PROVIDER_MORMOT) or DEFINED(HORSE_PROVIDER_ICS) or DEFINED(HORSE_APPTYPE_VCL) or DEFINED(HORSE_APPTYPE_DAEMON) or DEFINED(HORSE_APPTYPE_LCL) or DEFINED(HORSE_HOST_APACHE) or DEFINED(HORSE_HOST_ISAPI) or DEFINED(HORSE_HOST_CGI) or DEFINED(HORSE_HOST_FCGI)}
    {$MESSAGE FATAL 'HORSE_NOPROVIDER is mutually exclusive with all HORSE_PROVIDER_*, HORSE_APPTYPE_*, and HORSE_HOST_* defines — remove one.'}
  {$IFEND}
{$IFEND}

{ Rule 4 — mutually-exclusive Providers (Axis A admits at most one) }
{$IF DEFINED(HORSE_PROVIDER_CROSSSOCKET) and DEFINED(HORSE_PROVIDER_MORMOT)}
  {$MESSAGE FATAL 'HORSE_PROVIDER_CROSSSOCKET and HORSE_PROVIDER_MORMOT are mutually exclusive — pick exactly one transport Provider per build.'}
{$IFEND}
{$IF DEFINED(HORSE_PROVIDER_CROSSSOCKET) and DEFINED(HORSE_PROVIDER_ICS)}
  {$MESSAGE FATAL 'HORSE_PROVIDER_CROSSSOCKET and HORSE_PROVIDER_ICS are mutually exclusive — pick exactly one transport Provider per build.'}
{$IFEND}
{$IF DEFINED(HORSE_PROVIDER_MORMOT) and DEFINED(HORSE_PROVIDER_ICS)}
  {$MESSAGE FATAL 'HORSE_PROVIDER_MORMOT and HORSE_PROVIDER_ICS are mutually exclusive — pick exactly one transport Provider per build.'}
{$IFEND}
{$IF DEFINED(HORSE_PROVIDER_HTTPSYS) and (DEFINED(HORSE_PROVIDER_CROSSSOCKET) or DEFINED(HORSE_PROVIDER_MORMOT))}
  {$MESSAGE FATAL 'HORSE_PROVIDER_HTTPSYS is mutually exclusive with other transport Providers — pick exactly one per build.'}
{$IFEND}
{$IF DEFINED(HORSE_PROVIDER_EPOLL) and (DEFINED(HORSE_PROVIDER_CROSSSOCKET) or DEFINED(HORSE_PROVIDER_MORMOT) or DEFINED(HORSE_PROVIDER_HTTPSYS))}
  {$MESSAGE FATAL 'HORSE_PROVIDER_EPOLL is mutually exclusive with other transport Providers — pick exactly one per build.'}
{$IFEND}
{ =========================================================================== }

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  { Axis C — host-managed runtime wins outright }
  {$IF DEFINED(HORSE_HOST_APACHE)}
  Horse.Provider.FPC.Apache,
  {$ELSEIF DEFINED(HORSE_HOST_CGI)}
  Horse.Provider.FPC.CGI,
  {$ELSEIF DEFINED(HORSE_HOST_FCGI)}
  Horse.Provider.FPC.FastCGI,
  { Axis A · Provider × Axis B · Application type (self-hosted) }
  {$ELSEIF DEFINED(HORSE_PROVIDER_CROSSSOCKET)}
    {$IF DEFINED(HORSE_APPTYPE_DAEMON)}
    Horse.Provider.CrossSocket.FPC.Daemon,
    {$ELSEIF DEFINED(HORSE_APPTYPE_LCL)}
    Horse.Provider.CrossSocket.FPC.LCL,
    {$ELSE}
    Horse.Provider.CrossSocket,    { Console-shape — also covers FPC HTTPApplication }
    {$ENDIF}
  {$ELSEIF DEFINED(HORSE_PROVIDER_MORMOT)}
    {$IF DEFINED(HORSE_APPTYPE_DAEMON)}
    Horse.Provider.Mormot.FPC.Daemon,
    {$ELSEIF DEFINED(HORSE_APPTYPE_LCL)}
    Horse.Provider.Mormot.FPC.LCL,
    {$ELSE}
    Horse.Provider.Mormot.FPC.HTTPApplication,   { FPC default shape for mORMot }
    {$ENDIF}
  {$ELSEIF DEFINED(HORSE_PROVIDER_HTTPSYS)}
    {$IFDEF MSWINDOWS}
    Horse.Provider.HttpSys,
    {$ELSE}
    {$MESSAGE ERROR 'HORSE_PROVIDER_HTTPSYS is only supported on Windows.'}
    {$ENDIF}
  {$ELSEIF DEFINED(HORSE_PROVIDER_EPOLL)}
    {$IFDEF LINUX}
    Horse.Provider.Epoll,
    {$ELSE}
    {$MESSAGE ERROR 'HORSE_PROVIDER_EPOLL is only supported on Linux.'}
    {$ENDIF}
  {$ELSEIF DEFINED(HORSE_PROVIDER_IOCP)}
    {$IFDEF MSWINDOWS}
    Horse.Provider.IOCP,
    {$ELSE}
    {$MESSAGE ERROR 'HORSE_PROVIDER_IOCP is only supported on Windows.'}
    {$ENDIF}
  {$ELSEIF DEFINED(HORSE_APPTYPE_DAEMON)}
  Horse.Provider.FPC.Daemon,
  {$ELSEIF DEFINED(HORSE_APPTYPE_LCL)}
  Horse.Provider.FPC.LCL,
  {$ELSE}
  Horse.Provider.FPC.HTTPApplication,   { FPC default — fphttpserver under HTTPApplication }
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_NOPROVIDER)}
  System.SysUtils,
  Horse.Provider.Abstract,
{$ELSEIF DEFINED(HORSE_HOST_ISAPI)}
  System.SysUtils,
  Horse.Provider.ISAPI,
{$ELSEIF DEFINED(HORSE_HOST_APACHE)}
  System.SysUtils,
  Horse.Provider.Apache,
{$ELSEIF DEFINED(HORSE_HOST_CGI)}
  System.SysUtils,
  Horse.Provider.CGI,
{$ELSEIF DEFINED(HORSE_PROVIDER_HTTPSYS)}
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Horse.Provider.HttpSys,
  {$ELSE}
  Horse.Provider.Console,
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_PROVIDER_EPOLL)}
  System.SysUtils,
  {$IFDEF LINUX}
  Horse.Provider.Epoll,
  {$ELSE}
  Horse.Provider.Console,
  {$ENDIF}

{$ELSEIF DEFINED(HORSE_PROVIDER_IOCP)}
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Horse.Provider.IOCP,
  {$ELSE}
  Horse.Provider.Console,
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_PROVIDER_CROSSSOCKET)}
  System.SysUtils,
  {$IF DEFINED(HORSE_APPTYPE_VCL)}
  Horse.Provider.CrossSocket.VCL,
  {$ELSEIF DEFINED(HORSE_APPTYPE_DAEMON)}
  Horse.Provider.CrossSocket.Daemon,
  {$ELSE}
  Horse.Provider.CrossSocket,    { Console-shape — Delphi default for CrossSocket }
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_PROVIDER_MORMOT)}
  System.SysUtils,
  {$IF DEFINED(HORSE_APPTYPE_VCL)}
  Horse.Provider.Mormot.VCL,
  {$ELSEIF DEFINED(HORSE_APPTYPE_DAEMON)}
  Horse.Provider.Mormot.Daemon,
  {$ELSE}
  Horse.Provider.Mormot,         { Console-shape — Delphi default for mORMot }
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_PROVIDER_ICS)}
  System.SysUtils,
  {$IF DEFINED(HORSE_APPTYPE_VCL)}
  Horse.Provider.ICS.VCL,
  {$ELSEIF DEFINED(HORSE_APPTYPE_DAEMON)}
  Horse.Provider.ICS.Daemon,
  {$ELSE}
  Horse.Provider.ICS,            { Console-shape — Delphi default for ICS }
  {$ENDIF}
{$ELSE}
  System.SysUtils,
  Horse.Provider.Console,
  Horse.Provider.Daemon,
  Horse.Provider.ISAPI,
  Horse.Provider.Apache,
  Horse.Provider.CGI,
  Horse.Provider.VCL,
{$ENDIF}
  Horse.Mime,
  Horse.Core,
  Horse.Proc,
  Horse.Request,
  Horse.Response,
  Horse.Commons,
  Horse.Core.Param,
  Horse.Core.RouterTree,
  Horse.Exception,
  Horse.Exception.Interrupted,
  Horse.Core.Param.Config,
  Horse.Callback,
  Horse.Provider.Config,
  Horse.Core.Router.Radix,
  Horse.Instance;

type
  EHorseException = Horse.Exception.EHorseException;
  THorseOnSendString = Horse.Core.THorseOnSendString;
  THorseOnSendBytes = Horse.Core.THorseOnSendBytes;
  EHorseCallbackInterrupted = Horse.Exception.Interrupted.EHorseCallbackInterrupted;
  TProc = Horse.Proc.TProc;
  TNextProc = Horse.Proc.TNextProc;
  THorseList = Horse.Core.Param.THorseList;
  THorseCoreParam = Horse.Core.Param.THorseCoreParam;
  THorseCoreParamConfig = Horse.Core.Param.Config.THorseCoreParamConfig;
  THorseRequest = Horse.Request.THorseRequest;
  THorseResponse = Horse.Response.THorseResponse;
  THorseCallback = Horse.Callback.THorseCallback;
  THTTPStatus = Horse.Commons.THTTPStatus;
  TMimeTypes = Horse.Commons.TMimeTypes;
  THorseMimeTypes = Horse.Mime.THorseMimeTypes;
  TMessageType = Horse.Commons.TMessageType;
  THorseModule = Horse.Core.THorseModule;
  PHorseModule = Horse.Core.PHorseModule;
  PHorseCore = Horse.Core.PHorseCore;
  PHorseRouterTree = Horse.Core.RouterTree.PHorseRouterTree;
  THorseCrossSocketConfig = Horse.Provider.Config.THorseCrossSocketConfig;
  THorseRadixRouter = Horse.Core.Router.Radix.THorseRadixRouter;
  THorseInstance = Horse.Instance.THorseInstance;
  THorseServerLifecycleProc = Horse.Instance.THorseServerLifecycleProc;
  THorseServerLifecycleMethod = Horse.Instance.THorseServerLifecycleMethod;
  IHorseStartup = Horse.Instance.IHorseStartup;
  {$IF DEFINED(FPC)}
  THorseListenCallback = TProc;
  {$ELSE}
  THorseListenCallback = System.SysUtils.TProc;
  {$ENDIF}

{ PATCH-HORSE-2 — THorseProvider resolution follows the same three-axis model
  as the uses clause above:
    Stage 1 — Axis C (host-managed) wins outright
    Stage 2 — Axis A (Provider) × Axis B (Application type) compose }
{$IF DEFINED(HORSE_HOST_ISAPI)}
  THorseProvider = Horse.Provider.ISAPI.THorseProvider;
{$ELSEIF DEFINED(HORSE_HOST_APACHE)}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.Apache.THorseProvider;
  {$ELSE}
    Horse.Provider.Apache.THorseProvider;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_HOST_CGI)}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.CGI.THorseProvider;
  {$ELSE}
    Horse.Provider.CGI.THorseProvider;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_HOST_FCGI)}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.FastCGI.THorseProvider;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_PROVIDER_HTTPSYS)}
  THorseProvider =
  {$IFDEF MSWINDOWS}
    Horse.Provider.HttpSys.THorseProviderHttpSys;
  {$ELSE}
    Horse.Provider.Console.THorseProvider;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_PROVIDER_EPOLL)}
  THorseProvider =
  {$IFDEF LINUX}
    Horse.Provider.Epoll.THorseProviderEpoll;
  {$ELSE}
    Horse.Provider.Console.THorseProvider;
  {$ENDIF}

{$ELSEIF DEFINED(HORSE_PROVIDER_IOCP)}
  THorseProvider =
  {$IFDEF MSWINDOWS}
    Horse.Provider.IOCP.THorseProviderIOCP;
  {$ELSE}
    Horse.Provider.Console.THorseProvider;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_PROVIDER_CROSSSOCKET)}
  {$IF DEFINED(HORSE_APPTYPE_VCL)}
    THorseProvider = Horse.Provider.CrossSocket.VCL.THorseProviderCrossSocketVCL;
  {$ELSEIF DEFINED(HORSE_APPTYPE_DAEMON)}
    THorseProvider =
    {$IF DEFINED(FPC)}
      Horse.Provider.CrossSocket.FPC.Daemon.THorseProviderCrossSocketFPCDaemon;
    {$ELSE}
      Horse.Provider.CrossSocket.Daemon.THorseProviderCrossSocketDaemon;
    {$ENDIF}
  {$ELSEIF DEFINED(HORSE_APPTYPE_LCL)}
    THorseProvider = Horse.Provider.CrossSocket.FPC.LCL.THorseProviderCrossSocketFPCLCL;
  {$ELSE}
    THorseProvider = Horse.Provider.CrossSocket.THorseProviderCrossSocket;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_PROVIDER_MORMOT)}
  {$IF DEFINED(HORSE_APPTYPE_VCL)}
    THorseProvider = Horse.Provider.Mormot.VCL.THorseProviderMormotVCL;
  {$ELSEIF DEFINED(HORSE_APPTYPE_DAEMON)}
    THorseProvider =
    {$IF DEFINED(FPC)}
      Horse.Provider.Mormot.FPC.Daemon.THorseProviderMormotFPCDaemon;
    {$ELSE}
      Horse.Provider.Mormot.Daemon.THorseProviderMormotDaemon;
    {$ENDIF}
  {$ELSEIF DEFINED(HORSE_APPTYPE_LCL)}
    THorseProvider = Horse.Provider.Mormot.FPC.LCL.THorseProviderMormotFPCLCL;
  {$ELSEIF DEFINED(FPC)}
    THorseProvider = Horse.Provider.Mormot.FPC.HTTPApplication.THorseProviderMormotFPCHTTPApplication;
  {$ELSE}
    THorseProvider = Horse.Provider.Mormot.THorseProviderMormot;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_PROVIDER_ICS)}
  { v1: Delphi + Windows only. The FATAL guards above keep FPC/non-Windows
    builds from reaching this branch — see PATCH-HORSE-1 expansion. }
  {$IF DEFINED(HORSE_APPTYPE_VCL)}
    THorseProvider = Horse.Provider.ICS.VCL.THorseProviderICSVCL;
  {$ELSEIF DEFINED(HORSE_APPTYPE_DAEMON)}
    THorseProvider = Horse.Provider.ICS.Daemon.THorseProviderICSDaemon;
  {$ELSE}
    THorseProvider = Horse.Provider.ICS.THorseProviderICS;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_APPTYPE_DAEMON)}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.Daemon.THorseProvider;
  {$ELSE}
    Horse.Provider.Daemon.THorseProvider;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_APPTYPE_LCL)}
  THorseProvider = Horse.Provider.FPC.LCL.THorseProvider;
{$ELSEIF DEFINED(HORSE_APPTYPE_VCL)}
  THorseProvider = Horse.Provider.VCL.THorseProvider;
{$ELSE}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.HTTPApplication.THorseProvider;
  {$ELSEIF DEFINED(HORSE_NOPROVIDER)}
    Horse.Provider.Abstract.THorseProviderAbstract;
  {$ELSE}
    Horse.Provider.Console.THorseProvider;
  {$ENDIF}
{$ENDIF}

  THorse = class(THorseProvider)
  private
    class function GetActiveRequests: Integer; static; inline;
    class function GetIsShuttingDown: Boolean; static; inline;
  public
    class procedure UseRadixRouter;
    class property ActiveRequests: Integer read GetActiveRequests;
    class property IsShuttingDown: Boolean read GetIsShuttingDown;

    class function UseStartup(const AStartup: IHorseStartup): THorse;
    class function AddOnBeforeListen(const ACallback: THorseServerLifecycleProc): THorse; overload;
    class function AddOnAfterListen(const ACallback: THorseServerLifecycleProc): THorse; overload;
    class function AddOnBeforeStop(const ACallback: THorseServerLifecycleProc): THorse; overload;
    class function AddOnAfterStop(const ACallback: THorseServerLifecycleProc): THorse; overload;

    class function AddOnBeforeListen(const ACallback: THorseServerLifecycleMethod): THorse; overload;
    class function AddOnAfterListen(const ACallback: THorseServerLifecycleMethod): THorse; overload;
    class function AddOnBeforeStop(const ACallback: THorseServerLifecycleMethod): THorse; overload;
    class function AddOnAfterStop(const ACallback: THorseServerLifecycleMethod): THorse; overload;
    class function AddOnTelemetry(const ACallback: THorseOnTelemetry): THorse;
  end;

implementation

uses
  Horse.Core.Base,
  Horse.Provider.Abstract;

{ THorse }

class function THorse.GetActiveRequests: Integer;
begin
  Result := THorseCore.GetActiveRequests;
end;

class function THorse.GetIsShuttingDown: Boolean;
begin
  Result := THorseCore.GetIsShuttingDown;
end;

class procedure THorse.UseRadixRouter;
begin
  THorse.Routes := THorseRadixRouter.Create;
end;



class function THorse.UseStartup(const AStartup: IHorseStartup): THorse;
var
  LInstance: THorseCoreBase;
begin
  Result := THorse(Self);
  LInstance := ResolveBuildingInstance;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).UseStartup(AStartup);
end;

class function THorse.AddOnBeforeListen(const ACallback: THorseServerLifecycleProc): THorse;
var
  LInstance: THorseCoreBase;
begin
  Result := THorse(Self);
  LInstance := ResolveBuildingInstance;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).AddOnBeforeListen(ACallback)
  else
    THorseProviderAbstract.AddOnBeforeListen(ACallback);
end;

class function THorse.AddOnAfterListen(const ACallback: THorseServerLifecycleProc): THorse;
var
  LInstance: THorseCoreBase;
begin
  Result := THorse(Self);
  LInstance := ResolveBuildingInstance;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).AddOnAfterListen(ACallback)
  else
    THorseProviderAbstract.AddOnAfterListen(ACallback);
end;

class function THorse.AddOnBeforeStop(const ACallback: THorseServerLifecycleProc): THorse;
var
  LInstance: THorseCoreBase;
begin
  Result := THorse(Self);
  LInstance := ResolveBuildingInstance;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).AddOnBeforeStop(ACallback)
  else
    THorseProviderAbstract.AddOnBeforeStop(ACallback);
end;

class function THorse.AddOnAfterStop(const ACallback: THorseServerLifecycleProc): THorse;
var
  LInstance: THorseCoreBase;
begin
  Result := THorse(Self);
  LInstance := ResolveBuildingInstance;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).AddOnAfterStop(ACallback)
  else
    THorseProviderAbstract.AddOnAfterStop(ACallback);
end;

class function THorse.AddOnBeforeListen(const ACallback: THorseServerLifecycleMethod): THorse;
var
  LInstance: THorseCoreBase;
begin
  Result := THorse(Self);
  LInstance := ResolveBuildingInstance;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).AddOnBeforeListen(ACallback)
  else
    THorseProviderAbstract.AddOnBeforeListen(ACallback);
end;

class function THorse.AddOnAfterListen(const ACallback: THorseServerLifecycleMethod): THorse;
var
  LInstance: THorseCoreBase;
begin
  Result := THorse(Self);
  LInstance := ResolveBuildingInstance;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).AddOnAfterListen(ACallback)
  else
    THorseProviderAbstract.AddOnAfterListen(ACallback);
end;

class function THorse.AddOnBeforeStop(const ACallback: THorseServerLifecycleMethod): THorse;
var
  LInstance: THorseCoreBase;
begin
  Result := THorse(Self);
  LInstance := ResolveBuildingInstance;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).AddOnBeforeStop(ACallback)
  else
    THorseProviderAbstract.AddOnBeforeStop(ACallback);
end;

class function THorse.AddOnAfterStop(const ACallback: THorseServerLifecycleMethod): THorse;
var
  LInstance: THorseCoreBase;
begin
  Result := THorse(Self);
  LInstance := ResolveBuildingInstance;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).AddOnAfterStop(ACallback)
  else
    THorseProviderAbstract.AddOnAfterStop(ACallback);
end;

class function THorse.AddOnTelemetry(const ACallback: THorseOnTelemetry): THorse;
var
  LInstance: THorseCoreBase;
begin
  Result := THorse(Self);
  LInstance := ResolveBuildingInstance;
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).AddOnTelemetry(ACallback)
  else
    THorseCore.AddOnTelemetry(ACallback);
end;

end.
