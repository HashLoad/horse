unit Horse;

interface

uses
  System.SysUtils,
  Horse.Core, Horse.HTTP, Horse.Commons, Horse.Core.RouterTree, Horse.Exception, Horse.Provider.Abstract,
  Horse.Provider.Console, Horse.Provider.Daemon, Horse.Provider.ISAPP, Horse.Provider.Apache, Horse.Provider.CGI;

type
  EHorseException = Horse.Exception.EHorseException;
  EHorseCallbackInterrupted = Horse.Exception.EHorseCallbackInterrupted;
  TProc = System.SysUtils.TProc;
  THorseList = Horse.HTTP.THorseList;
  THorseRequest = Horse.HTTP.THorseRequest;
  THorseHackRequest = Horse.HTTP.THorseHackRequest;
  THorseResponse = Horse.HTTP.THorseResponse;
  THorseHackResponse = Horse.HTTP.THorseHackResponse;
  THorseCallback = Horse.Core.RouterTree.THorseCallback;
  THTTPStatus = Horse.Commons.THTTPStatus;
  TMimeTypes = Horse.Commons.TMimeTypes;
  TMessageType = Horse.Commons.TMessageType;
  THTTPStatusHelper = Horse.Commons.THTTPStatusHelper;
  TMimeTypesHelper = Horse.Commons.TMimeTypesHelper;

{$IF DEFINED(HORSE_ISAPP)}
  THorseProvider = Horse.Provider.ISAPP.THorseProvider;
{$ELSEIF DEFINED(HORSE_APACHE)}
  THorseProvider = Horse.Provider.Apache.THorseProvider;
{$ELSEIF DEFINED(HORSE_CGI)}
  THorseProvider = Horse.Provider.CGI.THorseProvider;
{$ELSEIF DEFINED(HORSE_DAEMON)}
  THorseProvider = Horse.Provider.Daemon.THorseProvider;
{$ELSE}
  THorseProvider = Horse.Provider.Console.THorseProvider;
{$ENDIF}

  THorse = class(THorseProvider);

implementation


end.
