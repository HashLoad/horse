unit Horse.Core.Param.Header;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Classes,
  fpHTTP,
  fphttpserver,
  httpprotocol,
  HTTPDefs,
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Web.HTTPApp,
  IdCustomHTTPServer,
  IdHeaderList,
  Horse.Rtti,
  Horse.Commons,
  Horse.Rtti.Helper,
  {$IF DEFINED(HORSE_APACHE)}
    Web.ApacheHTTP,
    Web.HTTPD24,
  {$ENDIF}
  {$IF DEFINED(HORSE_CGI)}
    Horse.EnvironmentVariables,
  {$ENDIF}
{$ENDIF}
  Horse.Core.Param;

type
  THorseCoreParamHeader = class
  private
{$IF DEFINED(FPC)}
    class function GetHeadersList(const AWebRequest: TRequest): TStrings;
{$ELSE}
    class function GetHeadersList(const AWebRequest: TWebRequest): TStrings;
{$ENDIF}
{$IF DEFINED(HORSE_CGI)}
    class function NormalizeEnvVarHeaderName(const AEnvVarHeaderName: string): string;
{$ENDIF}
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

{$IF DEFINED(FPC)}


class function THorseCoreParamHeader.GetHeadersList(const AWebRequest: TRequest): TStrings;
var
  LRequest: TFPHTTPConnectionRequest;
begin
  Result := TStringList.create;
  try
    if AWebRequest is TFPHTTPConnectionRequest then
    begin
      LRequest := TFPHTTPConnectionRequest(AWebRequest);
      Result.NameValueSeparator := '=';
      Result.Text := LRequest.CustomHeaders.Text;
    end;
  except
    Result.Free;
    raise;
  end;
end;
{$ELSE}


class function THorseCoreParamHeader.GetHeadersList(const AWebRequest: TWebRequest): TStrings;
{$IF DEFINED(HORSE_APACHE)}
type
  Papr_table_entry_t = ^apr_table_entry_t;

var
  LHeadersArray: papr_array_header_t;
  LHeadersEntry: Papr_table_entry_t;
  I: Integer;
{$ELSEIF DEFINED(HORSE_CGI)}
var
  LEnvironmentVariables: TStringList;
  LEnvVarIndex: Integer;
{$ELSEIF NOT DEFINED(HORSE_ISAPI)}
var
  LRequest: TIdHTTPRequestInfo;
  LObject: TObject;
{$ENDIF}
begin
  Result := TStringList.create;
  try
    Result.NameValueSeparator := ':';

{$IF DEFINED(HORSE_ISAPI)}
    Result.Text := AWebRequest.GetFieldByName('ALL_RAW');
{$ELSEIF DEFINED(HORSE_APACHE)}
    {$IF COMPILERVERSION <= 32}
      LHeadersArray := papr_array_header_t(Prequest_rec(TWebResponse(AWebRequest).HTTPRequest)^.headers_in);
      LHeadersEntry := Papr_table_entry_t(LHeadersArray^.elts);
    {$ELSE}
      LHeadersArray := papr_array_header_t(Prequest_rec(TApacheRequest(AWebRequest).HTTPDRequest)^.headers_in);
      LHeadersEntry := Papr_table_entry_t(LHeadersArray^.elts);
    {$IFEND}
    for I := 0 to Pred(LHeadersArray^.nelts) do
    begin
      Result.Add(string(LHeadersEntry^.key) + Result.NameValueSeparator + string(LHeadersEntry^.val));
      Inc(LHeadersEntry);
    end;
{$ELSEIF DEFINED(HORSE_CGI)}
    LEnvironmentVariables := THorseEnvironmentVariables.GetEnvironmentVariables;
    try
      for LEnvVarIndex := 0 to Pred(LEnvironmentVariables.Count) do
      begin
        if (LEnvironmentVariables.Strings[LEnvVarIndex].StartsWith('HTTP_')) then
          Result.AddPair(
            NormalizeEnvVarHeaderName(LEnvironmentVariables.KeyNames[LEnvVarIndex]),
            LEnvironmentVariables.ValueFromIndex[LEnvVarIndex]
          );
      end;
    finally
      LEnvironmentVariables.Free;
    end;
{$ELSE}
    LObject := THorseRtti.GetInstance.GetType(AWebRequest.ClassType).FieldValueAsObject(AWebRequest, 'FRequestInfo');
    if (Assigned(LObject)) and (LObject is TIdHTTPRequestInfo) then
    begin
      LRequest := TIdHTTPRequestInfo(LObject);
      Result.Text := LRequest.RawHeaders.Text;
    end
{$ENDIF}
  except
    Result.Free;
    raise;
  end;
end;

{$ENDIF}

{$IF DEFINED(HORSE_CGI)}
class function THorseCoreParamHeader.NormalizeEnvVarHeaderName(const AEnvVarHeaderName: string): string;
var
  LParts: TArray<string>;
  LNormalizedParts: TArray<string>;
  LSplitIndex: Integer;
begin
  Result := AEnvVarHeaderName.Replace('HTTP_', EmptyStr);
  LParts := Result.ToLower.Split(['_']);
  for LSplitIndex := Low(LParts) to High(LParts) do
    LNormalizedParts := LNormalizedParts + [UpperCase(LParts[LSplitIndex].Chars[0]) + LParts[LSplitIndex].Substring(1)];
  Result := Result.Join('-', LNormalizedParts);
end;
{$ENDIF}

end.
