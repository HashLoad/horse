unit Horse;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{ ===========================================================================
  PATCH-HORSE-1 — incompatible provider define guard
  Fires a fatal compile error when HORSE_CROSSSOCKET is combined with any
  define whose provider would silently take precedence over it in the
   $IF/$ELSEIF  chains below.  Without this guard, a misconfigured project
  compiles cleanly but runs the wrong provider (e.g. Indy Console instead of
  CrossSocket) with no diagnostic.  Four lines of Pascal — zero runtime cost.
  =========================================================================== }
{$IF DEFINED(HORSE_CROSSSOCKET)}
  {$IF DEFINED(HORSE_ISAPI)}
    {$MESSAGE FATAL 'HORSE_CROSSSOCKET is incompatible with HORSE_ISAPI — remove one define. ISAPI is a host-managed transport; CrossSocket owns the socket directly.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_APACHE)}
    {$MESSAGE FATAL 'HORSE_CROSSSOCKET is incompatible with HORSE_APACHE — remove one define. Apache mod is a host-managed transport; CrossSocket owns the socket directly.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_CGI)}
    {$MESSAGE FATAL 'HORSE_CROSSSOCKET is incompatible with HORSE_CGI — remove one define. CGI is a host-managed transport; CrossSocket owns the socket directly.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_FCGI)}
    {$MESSAGE FATAL 'HORSE_CROSSSOCKET is incompatible with HORSE_FCGI — remove one define. FastCGI is a host-managed transport; CrossSocket owns the socket directly.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_DAEMON)}
    {$MESSAGE FATAL 'HORSE_CROSSSOCKET is incompatible with HORSE_DAEMON — remove one define. Both providers own the process event loop; they cannot coexist.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_LCL)}
    {$MESSAGE FATAL 'HORSE_CROSSSOCKET is incompatible with HORSE_LCL — remove one define. LCL drives a GUI message loop that conflicts with CrossSocket IOCP/epoll.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_VCL)}
    {$MESSAGE FATAL 'HORSE_CROSSSOCKET is incompatible with HORSE_VCL — remove one define. VCL drives a GUI message loop that conflicts with CrossSocket IOCP/epoll.'}
  {$ENDIF}
  {$IF DEFINED(HORSE_NOPROVIDER)}
    {$MESSAGE FATAL 'HORSE_CROSSSOCKET is incompatible with HORSE_NOPROVIDER — remove one define. HORSE_NOPROVIDER suppresses all provider units including CrossSocket.'}
  {$ENDIF}
{$ENDIF}
{ =========================================================================== }

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  {$IF DEFINED(HORSE_CROSSSOCKET)}
  Horse.Provider.CrossSocket,
  {$ELSE}
  Horse.Provider.FPC.HTTPApplication,
  {$IF DEFINED(HORSE_APACHE)}
  Horse.Provider.FPC.Apache,
  {$ELSEIF DEFINED(HORSE_CGI)}
  Horse.Provider.FPC.CGI,
  {$ELSEIF DEFINED(HORSE_FCGI)}
  Horse.Provider.FPC.FastCGI,
  {$ELSEIF DEFINED(HORSE_DAEMON)}
  Horse.Provider.FPC.Daemon,
  {$ELSEIF DEFINED(HORSE_LCL)}
  Horse.Provider.FPC.LCL,
  {$ENDIF}
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_NOPROVIDER)}
  System.SysUtils,
  Horse.Provider.Abstract,
{$ELSEIF DEFINED(HORSE_CROSSSOCKET)}
  System.SysUtils,
  Horse.Provider.CrossSocket,
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
  Horse.Provider.Config;

type
  EHorseException = Horse.Exception.EHorseException;
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

{$IF DEFINED(HORSE_ISAPI)}
  THorseProvider = Horse.Provider.ISAPI.THorseProvider;
{$ELSEIF DEFINED(HORSE_APACHE)}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.Apache.THorseProvider;
  {$ELSE}
    Horse.Provider.Apache.THorseProvider;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_CGI)}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.CGI.THorseProvider;
  {$ELSE}
    Horse.Provider.CGI.THorseProvider;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_FCGI)}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.FastCGI.THorseProvider;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_DAEMON)}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.Daemon.THorseProvider;
  {$ELSE}
    Horse.Provider.Daemon.THorseProvider;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_LCL)}
  THorseProvider = Horse.Provider.FPC.LCL.THorseProvider;
{$ELSEIF DEFINED(HORSE_VCL)}
  THorseProvider = Horse.Provider.VCL.THorseProvider;
{$ELSEIF DEFINED(HORSE_CROSSSOCKET)}
  THorseProvider = Horse.Provider.CrossSocket.THorseProviderCrossSocket;
{$ELSE}
  THorseProvider =
  {$IF DEFINED(FPC)}
    {$IF DEFINED(HORSE_CROSSSOCKET)}
    Horse.Provider.CrossSocket.THorseProviderCrossSocket;
    {$ELSE}
    Horse.Provider.FPC.HTTPApplication.THorseProvider;
    {$ENDIF}
  {$ELSEIF DEFINED(HORSE_NOPROVIDER)}
    Horse.Provider.Abstract.THorseProviderAbstract;
  {$ELSE}
    Horse.Provider.Console.THorseProvider;
  {$ENDIF}
{$ENDIF}

  THorse = class(THorseProvider);

implementation

end.
