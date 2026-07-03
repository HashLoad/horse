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
  THorseCallbackProc = procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc);

{$IF DEFINED(FPC)}
  THorseCallbackRequest = procedure(AReq: THorseRequest);
  THorseCallbackResponse = procedure(ARes: THorseResponse);
  THorseCallbackRequestResponse = procedure(AReq: THorseRequest; ARes: THorseResponse);
  THorseCallback = Pointer;
  TCallNextPath = function(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean of object;
{$ELSE}
  THorseCallbackRequest = reference to procedure(AReq: THorseRequest);
  THorseCallbackResponse = reference to procedure(ARes: THorseResponse);
  THorseCallbackRequestResponse = reference to procedure(AReq: THorseRequest; ARes: THorseResponse);
  THorseCallback = reference to procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc);
  TCallNextPath = reference to function(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
{$ENDIF}

implementation

end.
