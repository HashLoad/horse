unit Horse;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
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
  Horse.Callback;

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
{$ELSE}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.HTTPApplication.THorseProvider;
  {$ELSE}
    Horse.Provider.Console.THorseProvider;
  {$ENDIF}
{$ENDIF}

  THorse = class(THorseProvider);

implementation

end.
