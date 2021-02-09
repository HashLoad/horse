unit Horse;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Horse.Provider.FPC.HTTPApplication, Horse.Provider.FPC.Apache, Horse.Provider.FPC.CGI, Horse.Provider.FPC.FastCGI,
  Horse.Provider.FPC.Daemon,
{$ELSE}
  System.SysUtils, Horse.Provider.Console, Horse.Provider.Daemon, Horse.Provider.ISAPI, Horse.Provider.Apache, Horse.Provider.CGI, Horse.Provider.VCL,
{$ENDIF}
  Horse.Core, Horse.Proc, Horse.HTTP, Horse.Commons, Horse.Core.RouterTree, Horse.Exception, Horse.Provider.Abstract;

type
  EHorseException = Horse.Exception.EHorseException;
  EHorseCallbackInterrupted = Horse.Exception.EHorseCallbackInterrupted;
  TProc = Horse.Proc.TProc;
  TNextProc = Horse.Proc.TNextProc;
  THorseList = Horse.HTTP.THorseList;
  THorseRequest = Horse.HTTP.THorseRequest;
  THorseHackRequest = Horse.HTTP.THorseHackRequest;
  THorseResponse = Horse.HTTP.THorseResponse;
  THorseHackResponse = Horse.HTTP.THorseHackResponse;
  THorseCallback = Horse.Core.RouterTree.THorseCallback;
  THTTPStatus = Horse.Commons.THTTPStatus;
  TMimeTypes = Horse.Commons.TMimeTypes;
  TMessageType = Horse.Commons.TMessageType;
  THorseModule = Horse.Core.THorseModule;

  PHorseModule = Horse.Core.PHorseModule;
  PHorseCore = Horse.Core.PHorseCore;
  PHorseRouterTree = Horse.Core.RouterTree.PHorseRouterTree;

  THorse = class;

{$IF DEFINED(HORSE_ISAPI)}
  THorseProvider = Horse.Provider.ISAPI.THorseProvider<THorse>;
{$ELSEIF DEFINED(HORSE_APACHE)}
  THorseProvider =
  {$IF DEFINED(FPC)}
      Horse.Provider.FPC.Apache.THorseProvider<THorse>
    {$ELSE}
      Horse.Provider.Apache.THorseProvider<THorse>
  {$ENDIF};
{$ELSEIF DEFINED(HORSE_CGI)}
  THorseProvider =
  {$IF DEFINED(FPC)}
      Horse.Provider.FPC.CGI.THorseProvider<THorse>
    {$ELSE}
      Horse.Provider.CGI.THorseProvider<THorse>
  {$ENDIF};
{$ELSEIF DEFINED(HORSE_FCGI)}
  THorseProvider =
  {$IF DEFINED(FPC)}
      Horse.Provider.FPC.FastCGI.THorseProvider<THorse>;
  {$ENDIF}
{$ELSEIF DEFINED(HORSE_DAEMON)}
  THorseProvider =
  {$IF DEFINED(FPC)}
    Horse.Provider.FPC.Daemon.THorseProvider<THorse>
  {$ELSE}
     Horse.Provider.Daemon.THorseProvider<THorse>
  {$ENDIF};
{$ELSEIF DEFINED(HORSE_VCL)}
  THorseProvider = Horse.Provider.VCL.THorseProvider<THorse>;
{$ELSE}
  THorseProvider =
  {$IF DEFINED(FPC)}
      Horse.Provider.FPC.HTTPApplication.THorseProvider<THorse>
    {$ELSE}
      Horse.Provider.Console.THorseProvider<THorse>
  {$ENDIF};
{$ENDIF}

  THorse = class(THorseProvider);

implementation

end.
