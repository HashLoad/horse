unit Horse.Core.Param.Header;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
    SysUtils, Classes, Generics.Collections, fpHTTP, fphttpserver, HTTPDefs,
  {$ELSE}
    System.Classes, System.SysUtils, System.Generics.Collections,
    Web.HTTPApp, IdCustomHTTPServer, IdHeaderList, Horse.Rtti,
  {$ENDIF}
  Horse.Core.Param,
  Horse.Commons;

type
  THorseStrings = {$IF DEFINED(FPC)} TStrings {$ELSE} TIdHeaderList {$ENDIF};

  THorseCoreParamHeader = class
  private
    class function GetHeadersList(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}): THorseStrings;

  public
    class function GetHeaders(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}): THorseList;

  end;

implementation

{ THorseCoreParamHeader }

class function THorseCoreParamHeader.GetHeaders(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}): THorseList;
var
  I: Integer;
  LName: String;
  LValue: String;
  LHeaders: THorseStrings;
begin
  Result := THorseList.create;
  try
    LHeaders := GetHeadersList(AWebRequest);
    for I := 0 to Pred(LHeaders.Count) do
    begin
      LName := LHeaders.Names[I];
      LValue := LHeaders.Values[LName];
      Result.AddOrSetValue(LName, LValue);
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function THorseCoreParamHeader.GetHeadersList(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}): THorseStrings;
var
  LRequest: {$IF DEFINED(FPC)} TFPHTTPConnectionRequest {$ELSE} TIdHTTPRequestInfo {$ENDIF};
  {$IF NOT DEFINED(FPC)}
  LObject: TObject;
  {$ENDIF}
begin
  Result := nil;
  {$IF DEFINED(FPC)}
    if AWebRequest is TFPHTTPConnectionRequest then
    begin
      LRequest := TFPHTTPConnectionRequest(AWebRequest);
      Result := LRequest.CustomHeaders;
    end;
  {$ELSE}
    LObject := THorseRtti.GetInstance.GetType(AWebRequest.ClassType)
                  .FieldValueAsObject(AWebRequest, 'FRequestInfo');

    if (Assigned(LObject)) and (LObject is TIdHTTPRequestInfo) then
    begin
      LRequest := TIdHTTPRequestInfo(LObject);
      Result := LRequest.RawHeaders;
    end;
  {$ENDIF}
end;

end.

