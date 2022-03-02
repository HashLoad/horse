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
  Web.HTTPApp, IdCustomHTTPServer, IdHeaderList, Horse.Rtti,
{$ENDIF}
  Horse.Core.Param, Horse.Commons, Horse.Rtti.Helper;

type
  THorseCoreParamHeader = class
  private
    class function GetHeadersList(const AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}): TStrings;
  public
    class function GetHeaders(const AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}): THorseList;
  end;

implementation

class function THorseCoreParamHeader.GetHeaders(const AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}): THorseList;
var
  I: Integer;
  LName, LValue: string;
  LHeaders: TStrings;
begin
  Result := THorseList.create;
  try
    LHeaders := GetHeadersList(AWebRequest);
    try
      for I := 0 to Pred(LHeaders.Count) do
      begin
        LName := LHeaders.Names[I];
        LValue := LHeaders.Values[LName];
        Result.AddOrSetValue(LName, Trim(LValue));
      end;
      {$IF DEFINED(FPC)}
      for I := Integer(Low(THeader)) to Integer(High(THeader)) do
      begin
        LName := HTTPHeaderNames[THeader(I)];
        LValue := AWebRequest.GetHeader(THeader(I));
        if not LValue.Trim.IsEmpty then
          Result.AddOrSetValue(LName, LValue);
      end;
      {$ENDIF}
    finally
      LHeaders.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function THorseCoreParamHeader.GetHeadersList(const AWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}): TStrings;
{$IF NOT DEFINED(HORSE_ISAPI)}
var
  LRequest: {$IF DEFINED(FPC)} TFPHTTPConnectionRequest {$ELSE} TIdHTTPRequestInfo {$ENDIF};
  {$IF NOT DEFINED(FPC)}
  LObject: TObject;
  {$ENDIF}
{$ENDIF}
begin
  Result := TStringList.Create;
  try
    Result.NameValueSeparator := ':';
    {$IF DEFINED(FPC)}
      if AWebRequest is TFPHTTPConnectionRequest then
      begin
        LRequest := TFPHTTPConnectionRequest(AWebRequest);
        Result.NameValueSeparator := '=';
        Result.Text := LRequest.CustomHeaders.Text;
      end;
    {$ELSEIF DEFINED(HORSE_ISAPI)}
      Result.Text := AWebRequest.GetFieldByName('ALL_RAW');
    {$ELSE}
      LObject := THorseRtti.GetInstance.GetType(AWebRequest.ClassType).FieldValueAsObject(AWebRequest, 'FRequestInfo');
      if (Assigned(LObject)) and (LObject is TIdHTTPRequestInfo) then
      begin
        LRequest := TIdHTTPRequestInfo(LObject);
        Result.Text := LRequest.RawHeaders.Text;
      end;
    {$ENDIF}
  except
    Result.Free;
    raise;
  end;
end;

end.
