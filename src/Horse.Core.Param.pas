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
  Horse.Exception,
  Horse.Commons;

type
  THorseList = TDictionary<string, string>;

  THorseCoreParam = class
  private
    FParams: THorseList;
    FContent: TStrings;

    function GetItem(const AKey: String): String;
    function GetList: THorseList;
    function GetCount: Integer;
    function GetContent: TStrings;
    function GetFormatSettings(const ADateFormat, ATimeFormat: String): TFormatSettings;

    procedure RaiseHorseException(const AMessage: String); overload;
    procedure RaiseHorseException(const AMessage: String; const Args: array of const); overload;

  public
    function AsBoolean(const AKey: String; ARequired: Boolean = True; ATrueValue: string = 'true'): Boolean;
    function AsCurrency(const AKey: String; ARequired: Boolean = True; ADecimalSeparator: String = ','): Currency;
    function AsDate(const AKey: string; ARequired: Boolean = True; ADateFormat: string = 'yyyy-MM-dd'): TDateTime;
    function AsDateTime(const AKey: string; ARequired: Boolean = True; ADateFormat: string = 'yyyy-MM-dd'; ATimeFormat: String = 'hh:mm:ss'): TDateTime;
    function AsExtended(const AKey: String; ARequired: Boolean = True): Extended;
    function AsFloat(const AKey: String; ARequired: Boolean = True; ADecimalSeparator: String = ','): Double;
    function AsInteger(const AKey: String; ARequired: Boolean = True): Integer;
    function AsInt64(const AKey: String; ARequired: Boolean = True): Int64;
    function AsISO8601DateTime(const AKey: string; ARequired: Boolean = True; AReturnUTC: Boolean = True): TDateTime;
    function AsString(const AKey: String; ARequired: Boolean = True): string;
    function AsTime(const AKey: string; ARequired: Boolean = True; ATimeFormat: string = 'hh:mm:ss'): TTime;

    function ContainsKey(const AKey: String): Boolean;
    function ContainsValue(const AValue: String): Boolean;
    function ToArray: TArray<TPair<String, String>>;
    function TryGetValue(const AKey: String; var AValue: String): Boolean;

    property Content: TStrings read GetContent;
    property Count: Integer read GetCount;
    property Items[const AKey: String]: String read GetItem; default;
    property List: THorseList read GetList;

    constructor create(AParams: THorseList);
    destructor Destroy; override;
end;

implementation

{ THorseCoreParam }

function THorseCoreParam.AsBoolean(const AKey: String; ARequired: Boolean; ATrueValue: String): Boolean;
var
  LStrParam: String;
begin
  result := False;
  LStrParam := AsString(AKey, ARequired);
  if LStrParam <> EmptyStr then
    result := LowerCase(LStrParam) = LowerCase(ATrueValue);
end;

function THorseCoreParam.AsCurrency(const AKey: String; ARequired: Boolean = True; ADecimalSeparator: String = ','): Currency;
begin
  result := AsFloat(AKey, ARequired);
end;

function THorseCoreParam.AsExtended(const AKey: String; ARequired: Boolean): Extended;
begin
  result := AsFloat(AKey, ARequired);
end;

function THorseCoreParam.AsISO8601DateTime(const AKey: string; ARequired: Boolean = True; AReturnUTC: Boolean = True): TDateTime;
var
  LStrParam: String;
begin
  Result := 0;
  LStrParam := AsString(AKey, ARequired);
  if LStrParam <> EmptyStr then
  begin
    if not TryISO8601ToDate(LStrParam, Result, AReturnUTC) then
      RaiseHorseException('The %s param ''%s'' is not valid a ISO8601 date.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsDate(const AKey: string; ARequired: Boolean; ADateFormat: string): TDateTime;
var
  LStrParam: String;
  LFormat: TFormatSettings;
begin
  result := 0;
  LStrParam := AsString(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat := GetFormatSettings(ADateFormat, EmptyStr);
      result := StrToDate(Copy(LStrParam, 1, Length(ADateFormat)), LFormat);
    end;
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a date type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsDateTime(const AKey: string; ARequired: Boolean; ADateFormat: string; ATimeFormat: String): TDateTime;
var
  LStrParam: String;
  LFormat: TFormatSettings;
begin
  result := 0;
  LStrParam := AsString(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat := GetFormatSettings(ADateFormat, ATimeFormat);
      result := StrToDateTime(LStrParam, LFormat);
    end;
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a date type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsFloat(const AKey: String; ARequired: Boolean = True; ADecimalSeparator: String = ','): Double;
var
  LStrParam: String;
begin
  result := 0;
  LStrParam := AsString(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
      result := StrToFloat(LStrParam.Replace(',', ADecimalSeparator).Replace('.', ADecimalSeparator));
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a numeric type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsInt64(const AKey: String; ARequired: Boolean): Int64;
var
  LStrParam: String;
begin
  result := 0;
  LStrParam := AsString(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
      result := StrToInt64(LStrParam);
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a int64 type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsInteger(const AKey: String; ARequired: Boolean): Integer;
var
  LStrParam: String;
begin
  result := 0;
  LStrParam := AsString(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
      result := StrToInt(LStrParam);
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a integer type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.AsString(const AKey: String; ARequired: Boolean): string;
begin
  result := EmptyStr;
  if ContainsKey(AKey) then
    result := FParams[AKey]
  else
  if ARequired then
    RaiseHorseException('The %s param is required.', [AKey]);
end;

function THorseCoreParam.AsTime(const AKey: string; ARequired: Boolean; ATimeFormat: string): TTime;
var
  LStrParam: String;
  LFormat: TFormatSettings;
begin
  result := 0;
  LStrParam := AsString(AKey, ARequired);
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat := GetFormatSettings(EmptyStr, ATimeFormat);
      result := StrToDateTime(Copy(LStrParam, 1, Length(ATimeFormat)), LFormat);
    end;
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a time type.', [AKey, LStrParam]);
  end;
end;

function THorseCoreParam.ContainsKey(const AKey: String): Boolean;
begin
  result := FParams.ContainsKey(AKey);
end;

function THorseCoreParam.ContainsValue(const AValue: String): Boolean;
begin
  result := FParams.ContainsValue(AValue);
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
    FContent := TStringList.Create;
    for LKey in FParams.Keys do
      FContent.Add(Format('%s=%s', [LKey, FParams[LKey]]));
  end;

  result := FContent;
end;

function THorseCoreParam.GetCount: Integer;
begin
  result := FParams.Count;
end;

function THorseCoreParam.GetFormatSettings(const ADateFormat, ATimeFormat: String): TFormatSettings;
begin
{$IF DEFINED(FPC)}
  result := DefaultFormatSettings;
  if ADateFormat.IndexOf('-') > 0 then
    result.DateSeparator := '-';
{$ELSE}
  result := TFormatSettings.Create;
{$ENDIF}
  result.ShortDateFormat := ADateFormat;
  result.ShortTimeFormat := ATimeFormat;
end;

function THorseCoreParam.GetItem(const AKey: String): String;
begin
  result := FParams[AKey];
end;

function THorseCoreParam.GetList: THorseList;
begin
  Result := FParams;
end;

procedure THorseCoreParam.RaiseHorseException(const AMessage: String; const Args: array of const);
begin
  RaiseHorseException(Format(AMessage, Args));
end;

procedure THorseCoreParam.RaiseHorseException(const AMessage: String);
var
  LException: EHorseException;
begin
  LException := EHorseException.Create(THTTPStatus.BadRequest, AMessage);
  LException.Message := AMessage;

  raise LException;
end;

function THorseCoreParam.ToArray: TArray<TPair<String, String>>;
begin
  result := FParams.ToArray;
end;

function THorseCoreParam.TryGetValue(const AKey: String; var AValue: String): Boolean;
begin
  result := FParams.TryGetValue(AKey, AValue);
end;

end.
