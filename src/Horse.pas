unit Horse;
{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Horse.Proc,
{$ELSE}
  System.SysUtils,
{$ENDIF}
  Horse.Core, Horse.HTTP, Horse.Commons, Horse.Core.RouterTree, Horse.Exception, Horse.Provider.Abstract,
  {$IF DEFINED(FPC)} Horse.Provider.FPCHTTPApplication, Horse.Provider.FPCCGIApplication {$ELSE}
  Horse.Provider.Console, Horse.Provider.Daemon, Horse.Provider.ISAPP, Horse.Provider.Apache, Horse.Provider.CGI{$ENDIF};

type
  EHorseException = Horse.Exception.EHorseException;
  EHorseCallbackInterrupted = Horse.Exception.EHorseCallbackInterrupted;
  TProc = {$IF DEFINED(FPC)} Horse.Proc.TProc {$ELSE} System.SysUtils.TProc {$ENDIF};
  THorseList = Horse.HTTP.THorseList;
  THorseRequest = Horse.HTTP.THorseRequest;
  THorseHackRequest = Horse.HTTP.THorseHackRequest;
  THorseResponse = Horse.HTTP.THorseResponse;
  THorseHackResponse = Horse.HTTP.THorseHackResponse;
  THorseCallback = Horse.Core.RouterTree.THorseCallback;
  THTTPStatus = Horse.Commons.THTTPStatus;
  TMimeTypes = Horse.Commons.TMimeTypes;
  TMessageType = Horse.Commons.TMessageType;
  THorse = class;

{$IF DEFINED(HORSE_ISAPP)}
  THorseProvider = Horse.Provider.ISAPP.THorseProvider<THorse>;
{$ELSEIF DEFINED(HORSE_APACHE)}
  THorseProvider = Horse.Provider.Apache.THorseProvider<THorse>;
{$ELSEIF DEFINED(HORSE_CGI)}
  THorseProvider =
  {$IF DEFINED(FPC)}
      Horse.Provider.FPCCGIApplication.THorseProvider<THorse>
    {$ELSE}
      Horse.Provider.CGI.THorseProvider<THorse>
  {$ENDIF};
{$ELSEIF DEFINED(HORSE_DAEMON)}
  THorseProvider = Horse.Provider.Daemon.THorseProvider<THorse>;
{$ELSE}
  THorseProvider =
  {$IF DEFINED(FPC)}
      Horse.Provider.FPCHTTPApplication.THorseProvider<THorse>
    {$ELSE}
      Horse.Provider.Console.THorseProvider<THorse>
  {$ENDIF};
{$ENDIF}

  THorse = class(THorseProvider);

implementation


end.
