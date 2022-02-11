unit Horse.Core.Param;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Classes, DateUtils, Generics.Collections, fpHTTP, fphttpserver, HTTPDefs,
{$ELSE}
  System.SysUtils, System.Classes, System.DateUtils, System.Generics.Collections,
{$ENDIF}
  Horse.Exception, Horse.Commons;

type
  THorseList = TDictionary<string, string>;

  THorseCoreParam = class
  private
    FParams: THorseList;
    FContent: TStrings;
    function GetItem(const AKey: string): string;
    function GetDictionary: THorseList;
    function GetCount: Integer;
    function GetContent: TStrings;
    function GetFormatSettings(const ADateFormat, ATimeFormat: string): TFormatSettings;
    procedure RaiseHorseException(const AMessage: string); overload;
    procedure RaiseHorseException(const AMessage: string; const Args: array of const); overload;
    function TryISO8601ToDate(const AISODate: string; out Value: TDateTime; AReturnUTC: Boolean = True): Boolean;
  public
    function AsBoolean(const AKey: string; ARequired: Boolean = True; ATrueValue: string = 'true'): Boolean;
    function AsCurrency(const AKey: string; ARequired: Boolean = True): Currency;
    function AsDate(const AKey: string; ARequired: Boolean = True; ADateFormat: string = 'yyyy-MM-dd'): TDateTime;
    function AsDateTime(const AKey: string; ARequired: Boolean = True; ADateFormat: string = 'yyyy-MM-dd'; ATimeFormat: string = 'hh:mm:ss'): TDateTime;
    function AsExtended(const AKey: string; ARequired: Boolean = True): Extended;
    function AsFloat(const AKey: string; ARequired: Boolean = True): Double;
    function AsInteger(const AKey: string; ARequired: Boolean = True): Integer;
    function AsInt64(const AKey: string; ARequired: Boolean = True): Int64;
    function AsISO8601DateTime(const AKey: string; ARequired: Boolean = True; AReturnUTC: Boolean = True): TDateTime;
    function AsString(const AKey: string; ARequired: Boolean = True): string;
    function AsTime(const AKey: string; ARequired: Boolean = True; ATimeFormat: string = 'hh:mm:ss'): TTime;
    function ContainsKey(const AKey: string): Boolean;
    function ContainsValue(const AValue: string): Boolean;
    function ToArray: TArray<TPair<string, string>>;
    function TryGetValue(const AKey: string; var AValue: string): Boolean;
    property Content: TStrings read GetContent;
    property Count: Integer read GetCount;
    property Items[const AKey: string]: string read GetItem; default;
    property Dictionary: THorseList read GetDictionary;
    constructor create(AParams: THorseList);
    destructor Destroy; override;
  end;

implementation

{ THorseCoreParam }

function THorseCoreParam.AsBoolean(const AKey: string; ARequired: Boolean; ATrueValue: string): Boolean;
var
  LStrParam: string;
begin
  Result := False;
  LStrParam := Asstring(AKey, ARequired);
  if LStrParam <> EmptyStr then
    result := LowerCase(LStrParam) = LowerCase(ATrueValue);
end;

function THorseCoreParam.AsCurrency(const AKey: string; ARequired: Boolean = True): Currency;
begin
  Result := AsFloat(AKey, ARequired);
end;

function THorseCoreParam.AsExtended(const AKey: string; ARequired: Boolean): Extended;
begin
  Result := AsFloat(AKey, ARequired);
end;

function THorseCoreParam.AsISO8601DateTime(const AKey: string; ARequired: Boolean = True; AReturnUTC: Boolean = True): TDateTime;
var
  LStrParam: string;
begin
  Result := 0;
  LStrParam := Asstring(AKey, ARequired);
  if LStrParam <> EmptyStr then
  begin
    if not TryISO8601ToDate(LStrParam, Result, AReturnUTC) then
      RaiseHorseException('The %s param ''%s'' is not valid a ISO8601 date.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsDate(const AKey: string; ARequired: Boolean; ADateFormat: string): TDateTime;
var
  LStrParam: string;
  LFormat: TFormatSettings;
begin
  Result := 0;
  LStrParam := Asstring(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat := GetFormatSettings(ADateFormat, EmptyStr);
      Result := StrToDate(Copy(LStrParam, 1, Length(ADateFormat)), LFormat);
    end;
  except
    on E: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a date type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsDateTime(const AKey: string; ARequired: Boolean; ADateFormat: string; ATimeFormat: string): TDateTime;
var
  LStrParam: string;
  LFormat: TFormatSettings;
begin
  Result := 0;
  LStrParam := Asstring(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat := GetFormatSettings(ADateFormat, ATimeFormat);
      Result := StrToDateTime(LStrParam, LFormat);
    end;
  except
    on E: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a date type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsFloat(const AKey: string; ARequired: Boolean = True): Double;
var
  LStrParam: string;
begin
  Result := 0;
  LStrParam := Asstring(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
    begin
      LStrParam := LStrParam.Replace(',', FormatSettings.DecimalSeparator).Replace('.', FormatSettings.DecimalSeparator);
      Result := StrToFloat(LStrParam);
    end;
  except
    on E: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a numeric type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsInt64(const AKey: string; ARequired: Boolean): Int64;
var
  LStrParam: string;
begin
  Result := 0;
  LStrParam := Asstring(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
      Result := StrToInt64(LStrParam);
  except
    on E: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a int64 type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsInteger(const AKey: string; ARequired: Boolean): Integer;
var
  LStrParam: string;
begin
  Result := 0;
  LStrParam := Asstring(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
      Result := StrToInt(LStrParam);
  except
    on E: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a integer type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsString(const AKey: string; ARequired: Boolean): string;
begin
  Result := EmptyStr;
  if ContainsKey(AKey) then
    Result := GetItem(AKey)
  else
  if ARequired then
    RaiseHorseException('The %s param is required.', [AKey]);
end;

function THorseCoreParam.AsTime(const AKey: string; ARequired: Boolean; ATimeFormat: string): TTime;
var
  LStrParam: string;
  LFormat: TFormatSettings;
begin
  Result := 0;
  LStrParam := Asstring(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat := GetFormatSettings(EmptyStr, ATimeFormat);
      Result := StrToTime(Copy(LStrParam, 1, Length(ATimeFormat)), LFormat);
    end;
  except
    on E: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a time type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.ContainsKey(const AKey: string): Boolean;
var
  LKey: string;
begin
  Result := False;
  for LKey in FParams.Keys do
  begin
    if AnsiCompareText(LKey, AKey) = 0 then
      Exit(True);
  end;
end;

function THorseCoreParam.ContainsValue(const AValue: string): Boolean;
begin
  Result := FParams.ContainsValue(AValue);
end;

constructor THorseCoreParam.create(AParams: THorseList);
begin
  FParams := AParams;
end;

destructor THorseCoreParam.Destroy;
begin
  FParams.Free;
  FContent.Free;
  inherited;
end;

function THorseCoreParam.GetContent: TStrings;
var
  LKey: string;
begin
  if not Assigned(FContent) then
  begin
    FContent := TstringList.Create;
    for LKey in FParams.Keys do
      FContent.Add(Format('%s=%s', [LKey, FParams[LKey]]));
  end;
  Result := FContent;
end;

function THorseCoreParam.GetCount: Integer;
begin
  Result := FParams.Count;
end;

function THorseCoreParam.GetFormatSettings(const ADateFormat, ATimeFormat: string): TFormatSettings;
begin
{$IF DEFINED(FPC)}
  Result := DefaultFormatSettings;
{$ELSE}
  Result := TFormatSettings.Create;
{$ENDIF}
  if ADateFormat.IndexOf('-') > 0 then
    Result.DateSeparator := '-';
  result.ShortDateFormat := ADateFormat;
  result.ShortTimeFormat := ATimeFormat;
end;

function THorseCoreParam.GetItem(const AKey: string): string;
var
  LKey: string;
begin
  for LKey in FParams.Keys do
  begin
    if AnsiCompareText(LKey, AKey) = 0 then
      Exit(FParams[LKey]);
  end;
  raise EListError.CreateFmt('Item %s not found', [AKey]);
end;

function THorseCoreParam.GetDictionary: THorseList;
begin
  Result := FParams;
end;

procedure THorseCoreParam.RaiseHorseException(const AMessage: string; const Args: array of const);
begin
  RaiseHorseException(Format(AMessage, Args));
end;

procedure THorseCoreParam.RaiseHorseException(const AMessage: string);
var
  LException: EHorseException;
begin
  LException := EHorseException.New.Status(THTTPStatus.BadRequest).Error(AMessage);
  LException.Message := AMessage;
  raise LException;
end;

function THorseCoreParam.ToArray: TArray<TPair<string, string>>;
begin
  Result := FParams.ToArray;
end;

function THorseCoreParam.TryGetValue(const AKey: string; var AValue: string): Boolean;
begin
  AValue := Asstring(AKey, False);
  Result := ContainsKey(AKey);
end;

function THorseCoreParam.TryISO8601ToDate(const AISODate: string; out Value: TDateTime; AReturnUTC: Boolean = True): Boolean;
begin
  Value := ISO8601ToDate(AISODate, AReturnUTC);
  Result := True
end;

end.
