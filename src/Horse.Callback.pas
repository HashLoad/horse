unit Horse.Callback;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}
{$MODESWITCH FUNCTIONREFERENCES+}
{$ENDIF}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Generics.Collections,
  fpHTTP,
{$ELSE}
  Web.HTTPApp,
  System.Generics.Collections,
{$ENDIF}
  Horse.Request,
  Horse.Response,
  Horse.Proc,
  Horse.Commons;

type
{$IF DEFINED(FPC)}
  THorseCallbackRequest = {$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}reference to {$ENDIF}procedure(AReq: THorseRequest);
  THorseCallbackResponse = {$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}reference to {$ENDIF}procedure(ARes: THorseResponse);
  THorseCallbackRequestResponse = {$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}reference to {$ENDIF}procedure(AReq: THorseRequest; ARes: THorseResponse);
  THorseCallback = {$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}reference to {$ENDIF}procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc);
  TCallNextPath = function(var APath: TQueue<string>; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean of object;
{$ELSE}
  THorseCallbackRequest = reference to procedure(AReq: THorseRequest);
  THorseCallbackResponse = reference to procedure(ARes: THorseResponse);
  THorseCallbackRequestResponse = reference to procedure(AReq: THorseRequest; ARes: THorseResponse);
  THorseCallback = reference to procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc);
  TCallNextPath = reference to function(var APath: TQueue<string>; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
{$ENDIF}

implementation

end.
