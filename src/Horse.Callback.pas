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
  THorseCallback = record
  private
    FValue: Pointer;
  public
    class operator Implicit(AValue: Pointer): THorseCallback; inline;
    class operator Implicit(AValue: THorseCallbackProc): THorseCallback; inline;
    class operator Implicit(AValue: THorseCallbackRequest): THorseCallback; inline;
    class operator Implicit(AValue: THorseCallbackResponse): THorseCallback; inline;
    class operator Implicit(AValue: THorseCallbackRequestResponse): THorseCallback; inline;
    class operator Implicit(AValue: THorseCallback): Pointer; inline;
    class operator Implicit(AValue: THorseCallback): THorseCallbackProc; inline;
  end;
  TCallNextPath = function(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean of object;
{$ELSE}
  THorseCallbackRequest = reference to procedure(AReq: THorseRequest);
  THorseCallbackResponse = reference to procedure(ARes: THorseResponse);
  THorseCallbackRequestResponse = reference to procedure(AReq: THorseRequest; ARes: THorseResponse);
  THorseCallback = reference to procedure(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc);
  TCallNextPath = reference to function(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; const AHTTPType: TMethodType; const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
{$ENDIF}

implementation

{$IF DEFINED(FPC)}
class operator THorseCallback.Implicit(AValue: Pointer): THorseCallback;
begin
  Result.FValue := AValue;
end;

class operator THorseCallback.Implicit(AValue: THorseCallbackProc): THorseCallback;
begin
  Result.FValue := @AValue;
end;

class operator THorseCallback.Implicit(AValue: THorseCallbackRequest): THorseCallback;
begin
  Result.FValue := @AValue;
end;

class operator THorseCallback.Implicit(AValue: THorseCallbackResponse): THorseCallback;
begin
  Result.FValue := @AValue;
end;

class operator THorseCallback.Implicit(AValue: THorseCallbackRequestResponse): THorseCallback;
begin
  Result.FValue := @AValue;
end;

class operator THorseCallback.Implicit(AValue: THorseCallback): Pointer;
begin
  Result := AValue.FValue;
end;

class operator THorseCallback.Implicit(AValue: THorseCallback): THorseCallbackProc;
begin
  Result := THorseCallbackProc(AValue.FValue);
end;
{$ENDIF}

end.
