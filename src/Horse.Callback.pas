unit Horse.Callback;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$DEFINE CALLBACKNAMED}

interface

uses
{$IF DEFINED(FPC)}
  Generics.Collections, fpHTTP,
{$ELSE}
  Web.HTTPApp, System.Generics.Collections,
{$ENDIF}
  Horse.Request, Horse.Response, Horse.Proc, Horse.Commons;

type
{$IF DEFINED(FPC)}
  THorseCallbackRequest = procedure(AReq: THorseRequest);
  THorseCallbackResponse = procedure(ARes: THorseResponse);
  TCallNextPath = function(var APath: TQueue<string>; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean of object;
  {$IFDEF CALLBACKNAMED}
    THorseCallbackRequestResponse = procedure(AReq: THorseRequest; ARes: THorseResponse; ACallbackName: String);
    THorseCallback = procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc; ACallbackName: String);
  {$ELSE}
    THorseCallbackRequestResponse = procedure(AReq: THorseRequest; ARes: THorseResponse);
    THorseCallback = procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc);
  {$ENDIF}
{$ELSE}
  THorseCallbackRequest = reference to procedure(AReq: THorseRequest);
  THorseCallbackResponse = reference to procedure(ARes: THorseResponse);
  TCallNextPath = reference to function(var APath: TQueue<string>; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
  {$IFDEF CALLBACKNAMED}
    THorseCallbackRequestResponse = reference to procedure(AReq: THorseRequest; ARes: THorseResponse; ACallbackName: String);
    THorseCallback = reference to procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc; ACallbackName: String);
  {$ELSE}
    THorseCallbackRequestResponse = reference to procedure(AReq: THorseRequest; ARes: THorseResponse);
    THorseCallback = reference to procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc);
  {$ENDIF}
{$ENDIF}
 TMiddlewares = TObjectDictionary<string, THorseCallback>;

implementation

end.
