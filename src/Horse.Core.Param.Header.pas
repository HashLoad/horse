unit Horse.Core.Param.Header;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Classes, Generics.Collections, fpHTTP, fphttpserver, httpprotocol, HTTPDefs,
{$ELSE}
  System.Classes, System.SysUtils, System.Generics.Collections,
  Web.HTTPApp,{$IF DEFINED(HORSE_ISAPI)}Web.Win.IsapiHTTP,{$ENDIF}
  IdCustomHTTPServer, IdHeaderList,
  Horse.Rtti,
{$ENDIF}
  Horse.Core.Param,
  Horse.Commons;

type
  THorseStrings = {$IF DEFINED(FPC)}TStrings{$ELSEIF DEFINED(HORSE_ISAPI)}TStringList{$ELSE}TIdHeaderList{$ENDIF};

  THorseCoreParamHeader = class
  private
    class function GetHeadersList(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSEIF DEFINED(HORSE_ISAPI)}TISAPIRequest{$ELSE}TWebRequest{$ENDIF}): THorseStrings;
  public
    class function GetHeaders(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSEIF DEFINED(HORSE_ISAPI)}TISAPIRequest{$ELSE}TWebRequest{$ENDIF}): THorseList;
  end;

implementation

{ THorseCoreParamHeader }

class function THorseCoreParamHeader.GetHeaders(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSEIF DEFINED(HORSE_ISAPI)}TISAPIRequest{$ELSE}TWebRequest{$ENDIF}): THorseList;
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
{$IF DEFINED(FPC)}
    for I := Integer(Low(THeader)) to Integer(High(THeader)) do
    begin
      LName := HTTPHeaderNames[THeader(I)];
      LValue := AWebRequest.GetHeader(THeader(I));
      Result.AddOrSetValue(LName, LValue);
    end;
{$ENDIF}
  except
    Result.Free;
    raise;
  end;
end;

class function THorseCoreParamHeader.GetHeadersList(AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSEIF DEFINED(HORSE_ISAPI)}TISAPIRequest{$ELSE}TWebRequest{$ENDIF}): THorseStrings;
{$IF NOT DEFINED(HORSE_ISAPI)}
var
  LRequest: {$IF DEFINED(FPC)}TFPHTTPConnectionRequest{$ELSE}TIdHTTPRequestInfo{$ENDIF};
{$IF NOT DEFINED(FPC)}
  LObject: TObject;
{$ENDIF}
{$ENDIF}
begin
{$IF DEFINED(FPC)}
  Result := nil;

  if AWebRequest is TFPHTTPConnectionRequest then
  begin
    LRequest := TFPHTTPConnectionRequest(AWebRequest);
    Result := LRequest.CustomHeaders;
  end;
{$ELSEIF DEFINED(HORSE_ISAPI)}
  Result := THorseStrings.Create;
  Result.NameValueSeparator := ':';
  Result.Text := AWebRequest.GetFieldByName('ALL_RAW');
{$ELSE}
  Result := nil;

  LObject := THorseRtti.GetInstance.GetType(AWebRequest.ClassType).FieldValueAsObject(AWebRequest, 'FRequestInfo');

  if (Assigned(LObject)) and (LObject is TIdHTTPRequestInfo) then
  begin
    LRequest := TIdHTTPRequestInfo(LObject);
    Result := LRequest.RawHeaders;
  end;
{$ENDIF}
end;

end.
