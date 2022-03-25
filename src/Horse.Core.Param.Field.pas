unit Horse.Core.Param.Field;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Classes, DateUtils, Generics.Collections,
{$ELSE}
  System.SysUtils, System.Classes, System.DateUtils, System.Generics.Collections,
{$ENDIF}
  Horse.Exception, Horse.Commons;

type

  { THorseCoreParamFieldLhsBrackets }

  THorseCoreParamFieldLhsBrackets = class
  private
    Feq: string; //Equal
    Fne: string; //NotEqual
    Flt: string; //LessThan
    Flte: string; //LessThanOrEqual
    Fgt: string; //GreaterThan
    Fgte: string; //GreaterThanOrEqual
    Frange: string; //Range
    Flike: string; //Like
    FTypes: TLhsBrackets;
  public
    property eq: string read Feq write Feq ;
    property ne: string read Fne write Fne;
    property lt: string read Flt write Flt;
    property lte: string read Flte;
    property gt: string read Fgt;
    property gte: string read Fgte;
    property range: string read Frange;
    property like: string read Flike;
    property Types: TLhsBrackets read FTypes write FTypes;

    procedure SetValue(AType: TLhsBracketsType; AValue: string);
    function GetValue(AType: TLhsBracketsType): string;
  end;

  { THorseCoreParamField }

  THorseCoreParamField = class
  private
    FContains: Boolean;
    FFieldName: string;
    FRequired: Boolean;
    FRequiredMessage: string;
    FInvalidFormatMessage: string;
    FDateFormat: string;
    FTimeFormat: string;
    FReturnUTC: Boolean;
    FTrueValue: string;
    FValue: string;
    FLhsBrackets: THorseCoreParamFieldLhsBrackets;

    function GetFormatSettings: TFormatSettings;
    procedure RaiseHorseException(const AMessage: string); overload;
    procedure RaiseHorseException(const AMessage: string; const Args: array of const); overload;
    function TryISO8601ToDate(const AValue: string; out Value: TDateTime): Boolean;
  public
    function DateFormat(const AValue: string): THorseCoreParamField;
    function InvalidFormatMessage(const AValue: string): THorseCoreParamField;
    function Required: THorseCoreParamField; overload;
    function Required(const AValue: Boolean): THorseCoreParamField; overload;
    function RequiredMessage(const AValue: string): THorseCoreParamField;
    function ReturnUTC(const AValue: Boolean): THorseCoreParamField;
    function TimeFormat(const AValue: string): THorseCoreParamField;
    function TrueValue(const AValue: string): THorseCoreParamField;

    function AsBoolean: Boolean;
    function AsCurrency: Currency;
    function AsDate: TDateTime;
    function AsDateTime: TDateTime;
    function AsExtended: Extended;
    function AsFloat: Double;
    function AsInteger: Integer;
    function AsInt64: Int64;
    function AsISO8601DateTime: TDateTime;
    function Asstring: string;
    function AsTime: TTime;

    property LhsBrackets:THorseCoreParamFieldLhsBrackets read FLhsBrackets;

    constructor Create(const AParams: TDictionary<string, string>; const AFieldName: string; ACheckLhsBrackets: Boolean);
    destructor Destroy; override;
  end;

implementation

{ THorseCoreParamFieldLhsBrackets }

procedure THorseCoreParamFieldLhsBrackets.SetValue(AType: TLhsBracketsType;
  AValue: string);
begin
  case AType of
    lbteq:
      Feq := AValue;
    lbtne:
      Fne := AValue;
    lbtlt:
      Flt := AValue;
    lbtlte:
      Flte := AValue;
    lbtgt:
      Fgt := AValue;
    lbtgte:
      Fgte := AValue;
    lbtrange:
      Frange := AValue;
    lbtlike:
      Flike := AValue;
  end;
end;

function THorseCoreParamFieldLhsBrackets.GetValue(AType: TLhsBracketsType
  ): string;
begin
  case AType of
    lbteq:
      Result := Feq;
    lbtne:
      Result := Fne;
    lbtlt:
      Result := Flt;
    lbtlte:
      Result := Flte;
    lbtgt:
      Result := Fgt;
    lbtgte:
      Result := Fgte;
    lbtrange:
      Result := Frange;
    lbtlike:
      Result := Flike;
  end;
end;

{ THorseCoreParamField }

function THorseCoreParamField.AsBoolean: Boolean;
var
  LStrParam: string;
begin
  Result := False;
  LStrParam := Asstring;
  if LStrParam <> EmptyStr then
    Result := LowerCase(LStrParam) = LowerCase(FTrueValue);
end;

function THorseCoreParamField.AsCurrency: Currency;
begin
  Result := AsFloat;
end;

function THorseCoreParamField.AsDate: TDateTime;
var
  LStrParam: string;
  LFormat: TFormatSettings;
begin
  Result := 0;
  LStrParam := Asstring;
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat := GetFormatSettings;
      Result := StrToDate(Copy(LStrParam, 1, Length(FDateFormat)), LFormat);
    end;
  except
    on E: EConvertError do
      RaiseHorseException(FInvalidFormatMessage, [FFieldName, LStrParam, 'date']);
  end;
end;

function THorseCoreParamField.AsDateTime: TDateTime;
var
  LStrParam: string;
  LFormat: TFormatSettings;
begin
  Result := 0;
  LStrParam := Asstring;
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat := GetFormatSettings;
      Result := StrToDateTime(LStrParam, LFormat);
    end;
  except
    on E: EConvertError do
      RaiseHorseException(FInvalidFormatMessage, [FFieldName, LStrParam, 'datetime']);
  end;
end;

function THorseCoreParamField.AsExtended: Extended;
begin
  Result := AsFloat;
end;

function THorseCoreParamField.AsFloat: Double;
var
  LStrParam: string;
begin
  Result := 0;
  LStrParam := Asstring;
  try
    if LStrParam <> EmptyStr then
    begin
      LStrParam := LStrParam.Replace(',', FormatSettings.DecimalSeparator).Replace('.', FormatSettings.DecimalSeparator);
      Result := StrToFloat(LStrParam);
    end;
  except
    on E: EConvertError do
      RaiseHorseException(FInvalidFormatMessage, [FFieldName, LStrParam, 'numeric']);
  end;
end;

function THorseCoreParamField.AsInt64: Int64;
var
  LStrParam: string;
begin
  Result := 0;
  LStrParam := Asstring;
  try
    if LStrParam <> EmptyStr then
      Result := StrToInt64(LStrParam);
  except
    on E: EConvertError do
      RaiseHorseException(FInvalidFormatMessage, [FFieldName, LStrParam, 'int64']);
  end;
end;

function THorseCoreParamField.AsInteger: Integer;
var
  LStrParam: string;
begin
  Result := 0;
  LStrParam := Asstring;
  try
    if LStrParam <> EmptyStr then
      Result := StrToInt(LStrParam);
  except
    on E: EConvertError do
      RaiseHorseException(FInvalidFormatMessage, [FFieldName, LStrParam, 'integer']);
  end;
end;

function THorseCoreParamField.AsISO8601DateTime: TDateTime;
var
  LStrParam: string;
begin
  Result := 0;
  LStrParam := Asstring;
  if LStrParam <> EmptyStr then
  begin
    if not TryISO8601ToDate(LStrParam, Result) then
      RaiseHorseException(FInvalidFormatMessage, [FFieldName, LStrParam, 'ISO8601 date']);
  end;
end;

function THorseCoreParamField.Asstring: string;
begin
  Result := EmptyStr;
  if FContains then
    Result := FValue
  else
  if FRequired then
    RaiseHorseException(FRequiredMessage, [FFieldName]);
end;

function THorseCoreParamField.AsTime: TTime;
var
  LStrParam: string;
  LFormat: TFormatSettings;
begin
  Result := 0;
  LStrParam := Asstring;
  try
    if LStrParam <> EmptyStr then
    begin
      LFormat := GetFormatSettings;
      Result := StrToTime(Copy(LStrParam, 1, Length(FTimeFormat)), LFormat);
    end;
  except
    on E: EConvertError do
      RaiseHorseException(FInvalidFormatMessage, [FFieldName, LStrParam, 'time']);
  end;
end;

constructor THorseCoreParamField.Create(const AParams: TDictionary<string,
  string>; const AFieldName: string; ACheckLhsBrackets: Boolean);
var
  LKey: string;
  LLhsBracketType: TLhsBracketsType;
begin
  FContains := False;
  FFieldName := AFieldName;
  FValue := EmptyStr;
  FRequired := False;
  FLhsBrackets := THorseCoreParamFieldLhsBrackets.Create;

  for LKey in AParams.Keys do
  begin
    if AnsiCompareText(LKey, FFieldName) = 0 then
    begin
      FContains := True;
      Break;
    end;
  end;

  if FContains then
    FValue := AParams.Items[LKey];

  if ACheckLhsBrackets then
  begin
    for LLhsBracketType in TLhsBracketsType do
    begin
      if AParams.ContainsKey(FFieldName+LLhsBracketType.ToString) then
      begin
        FLhsBrackets.SetValue(LLhsBracketType, AParams.Items[FFieldName+LLhsBracketType.ToString]);
        FLhsBrackets.Types := FLhsBrackets.Types + [LLhsBracketType];
      end;
    end;
  end;
end;

destructor THorseCoreParamField.Destroy;
begin
  FLhsBrackets.Free;
  inherited Destroy;
end;

function THorseCoreParamField.DateFormat(const AValue: string): THorseCoreParamField;
begin
  Result := Self;
  FDateFormat := AValue;
end;

function THorseCoreParamField.GetFormatSettings: TFormatSettings;
begin
{$IF DEFINED(FPC)}
  Result := DefaultFormatSettings;
{$ELSE}
  Result := TFormatSettings.Create;
{$ENDIF}
  if FDateFormat.IndexOf('-') > 0 then
    Result.DateSeparator := '-';
  Result.ShortDateFormat := FDateFormat;
  Result.ShortTimeFormat := FTimeFormat;
end;

function THorseCoreParamField.InvalidFormatMessage(const AValue: string): THorseCoreParamField;
begin
  Result := Self;
  FInvalidFormatMessage := AValue;
end;

procedure THorseCoreParamField.RaiseHorseException(const AMessage: string; const Args: array of const);
begin
  RaiseHorseException(Format(AMessage, Args));
end;

procedure THorseCoreParamField.RaiseHorseException(const AMessage: string);
var
  LException: EHorseException;
begin
  LException := EHorseException.New.Status(THTTPStatus.BadRequest).Error(AMessage);
  LException.Message := AMessage;
  raise LException;
end;

function THorseCoreParamField.Required(const AValue: Boolean): THorseCoreParamField;
begin
  Result := Self;
  FRequired := AValue;
end;

function THorseCoreParamField.Required: THorseCoreParamField;
begin
  Result := Self;
  FRequired := True;
end;

function THorseCoreParamField.RequiredMessage(const AValue: string): THorseCoreParamField;
begin
  Result := Self;
  FRequiredMessage := AValue;
end;

function THorseCoreParamField.ReturnUTC(const AValue: Boolean): THorseCoreParamField;
begin
  Result := Self;
  FReturnUTC := AValue;
end;

function THorseCoreParamField.TimeFormat(const AValue: string): THorseCoreParamField;
begin
  Result := Self;
  FTimeFormat := AValue;
end;

function THorseCoreParamField.TrueValue(const AValue: string): THorseCoreParamField;
begin
  Result := Self;
  FTrueValue := AValue;
end;

function THorseCoreParamField.TryISO8601ToDate(const AValue: string; out Value: TDateTime): Boolean;
begin
  Value := ISO8601ToDate(AValue, FReturnUTC);
  Result := True;
end;

end.
