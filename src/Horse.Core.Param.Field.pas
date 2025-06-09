unit Horse.Core.Param.Field;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Classes,
  DateUtils,
  Generics.Collections,
{$ELSE}
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.Generics.Collections,
  System.Rtti,
{$ENDIF}
  Horse.Exception,
  Horse.Commons,
  Horse.Core.Param.Field.Brackets,
  Horse.Core.Param.Config;

type

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
    FStream: TStream;
    FLhsBrackets: THorseCoreParamFieldLhsBrackets;

    procedure RaiseHorseException(const AMessage: string); overload;
    procedure RaiseHorseException(const AMessage: string; const Args: array of const); overload;
    function TryISO8601ToDate(const AValue: string; out Value: TDateTime): Boolean;
    procedure InitializeLhsBrackets(const AParams: TDictionary<string, string>; const AFieldName: string);
  public
    function DateFormat(const AValue: string): THorseCoreParamField;
    function InvalidFormatMessage(const AValue: string): THorseCoreParamField;
    function Required: THorseCoreParamField; overload;
    function Required(const AValue: Boolean): THorseCoreParamField; overload;
    function RequiredMessage(const AValue: string): THorseCoreParamField;
    function ReturnUTC(const AValue: Boolean): THorseCoreParamField;
    function TimeFormat(const AValue: string): THorseCoreParamField;
    function TrueValue(const AValue: string): THorseCoreParamField;
    procedure SaveToFile(const AFileName: String);
    function AsBoolean: Boolean;
    function AsCurrency: Currency;
    function AsDate: TDateTime;
    function AsDateTime: TDateTime;
    function AsExtended: Extended;
    function AsFloat: Double;
    function AsInteger: Integer;
    function AsInt64: Int64;
    function AsISO8601DateTime: TDateTime;
    function AsStream: TStream;
    function AsString: string;
    function AsTime: TTime;
    function AsList: TList<string>; overload;
   {$IF NOT DEFINED(FPC)}
    function AsList<T>: TList<T>; overload;
   {$ENDIF}
    property LhsBrackets:THorseCoreParamFieldLhsBrackets read FLhsBrackets;
    constructor Create(const AParams: TDictionary<string, string>; const AFieldName: string); overload;
    constructor Create(const AStream: TStream; const AFieldName: string); overload;
    destructor Destroy; override;
  end;

implementation

function THorseCoreParamField.AsBoolean: Boolean;
var
  LStrParam: string;
begin
  Result := False;
  LStrParam := Trim(AsString);
  if LStrParam <> EmptyStr then
    Result := LowerCase(LStrParam) = LowerCase(FTrueValue);
end;

function THorseCoreParamField.AsCurrency: Currency;
begin
  Result := AsFloat;
end;

function THorseCoreParamField.AsDate: TDateTime;
var  
  LDay: Word;
  LMonth: Word;
  LYear: Word;
  LStrParam: string;
  LDateTime: TDateTime;
begin
  Result := 0;
  LStrParam := Trim(AsString);
  try
    if LStrParam = EmptyStr then
      Exit;
    if not IsDateTime(LStrParam, LDateTime) then
      raise EConvertError.Create('');
    {$IF DEFINED(FPC)}
    DecodeDate(LDateTime, LYear, LMonth, LDay);
    Result := EncodeDate(LYear, LMonth, LDay);
    {$ELSE}
    Result.SetDate(LDateTime.Year, LDateTime.Month, LDateTime.Day);
    {$ENDIF}
  except
    on E: EConvertError do
      RaiseHorseException(FInvalidFormatMessage, [FFieldName, LStrParam, 'date']);
  end;
end;

function THorseCoreParamField.AsDateTime: TDateTime;
var
  LStrParam: string;
begin
  Result := 0;
  LStrParam := Trim(AsString);
  try
    if LStrParam = EmptyStr then
      Exit;
    if not IsDateTime(LStrParam, Result) then
      raise EConvertError.Create('');
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
  LStrParam := Trim(AsString);
  try
    if LStrParam = EmptyStr then
      Exit;
    LStrParam := LStrParam.Replace(',', FormatSettings.DecimalSeparator).Replace('.', FormatSettings.DecimalSeparator);
    Result := StrToFloat(LStrParam);
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
  LStrParam := Trim(AsString);
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
  LStrParam := Trim(AsString);
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
  LStrParam := Trim(AsString);
  if LStrParam = EmptyStr then
    Exit;
  if not TryISO8601ToDate(LStrParam, Result) then
    RaiseHorseException(FInvalidFormatMessage, [FFieldName, LStrParam, 'ISO8601 date']);
end;

function THorseCoreParamField.AsList: TList<string>;
var
  ValueArray : TArray<string>;
  i: Integer;
begin
  Result := TList<string>.Create;
  if FContains then
  begin
    ValueArray := FValue.Split([',']);
    for i := 0 to High(ValueArray) do
    begin
      Result.Add(ValueArray[i]);
    end;
  end
  else if FRequired then
    RaiseHorseException(FRequiredMessage, [FFieldName]);
end;

{$IF NOT DEFINED(FPC)}
function THorseCoreParamField.AsList<T>: TList<T>;
var
  ValueArray : TArray<string>;
  i: Integer;
begin
  Result := TList<T>.Create;
  if FContains then
  begin
    ValueArray := FValue.Split([',']);
    for i := 0 to High(ValueArray) do
    begin
      case GetTypeKind(T) of
        tkString, tkUString:
          Result.Add(TValue.From<string>(ValueArray[i]).AsType<T>);
        tkInteger:
          Result.Add(TValue.From<Integer>(StrToInt(ValueArray[i])).AsType<T>);
        tkInt64:
          Result.Add(TValue.From<Int64>(StrToInt64(ValueArray[i])).AsType<T>);
        tkFloat:
          Result.Add(TValue.From<Double>(StrToFloat(ValueArray[i])).AsType<T>);
        tkVariant:
          Result.Add(TValue.From<variant>(ValueArray[i]).AsType<T>);
      else
        // Specific handling for Datetime, Date, and Time, as they are not present in the TypeKind list.
        begin
          if TypeInfo(T) = TypeInfo(TDateTime) then
            Result.Add(TValue.From<TDateTime>(StrToDateTime(ValueArray[i])).AsType<T>)
          else if TypeInfo(T) = TypeInfo(TDate) then
            Result.Add(TValue.From<TDate>(StrToDate(ValueArray[i])).AsType<T>)
          else if TypeInfo(T) = TypeInfo(TTime) then
            Result.Add(TValue.From<TTime>(StrToTime(ValueArray[i])).AsType<T>)
          else
            raise EHorseException.New.Error('Unsupported type');
        end;
      end;
    end;
  end
  else if FRequired then
    RaiseHorseException(FRequiredMessage, [FFieldName]);
end;
{$ENDIF}

function THorseCoreParamField.AsStream: TStream;
begin
  Result := nil;
  if FContains then
  begin
    Result := FStream;
    if Assigned(Result) then
      Result.Position := 0;
  end
  else if FRequired then
    RaiseHorseException(FRequiredMessage, [FFieldName]);
end;

function THorseCoreParamField.AsString: string;
begin
  Result := EmptyStr;
  if FContains then
    Result := FValue
  else if FRequired then
    RaiseHorseException(FRequiredMessage, [FFieldName]);
end;

function THorseCoreParamField.AsTime: TTime;
var              
  LHour: Word;
  LMinute: Word;
  LSecond: Word;
  LMilliSecond: Word;
  LStrParam: string;
  LDateTime: TDateTime;
begin
  Result := 0;
  LStrParam := Trim(AsString);
  try
    if LStrParam = EmptyStr then
      Exit;
    if not IsDateTime(LStrParam, LDateTime) then
      raise EConvertError.Create('');
    {$IF DEFINED(FPC)}
    DecodeTime(LDateTime, LHour, LMinute, LSecond, LMilliSecond);
    Result := EncodeTime(LHour, LMinute, LSecond, LMilliSecond);
    {$ELSE}
    TDateTime(Result).SetTime(LDateTime.Hour, LDateTime.Minute, LDateTime.Second, 0);
    {$ENDIF}
  except
    on E: EConvertError do
      RaiseHorseException(FInvalidFormatMessage, [FFieldName, LStrParam, 'time']);
  end;
end;

constructor THorseCoreParamField.Create(const AStream: TStream; const AFieldName: string);
begin
  FContains := True;
  FFieldName := AFieldName;
  FValue := EmptyStr;
  FRequired := False;
  FStream := AStream;
end;

constructor THorseCoreParamField.Create(const AParams: TDictionary<string, string>; const AFieldName: string);
var
  LKey: string;
begin
  FContains := False;
  FFieldName := AFieldName;
  FValue := EmptyStr;
  FRequired := False;

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

  InitializeLhsBrackets(AParams, AFieldName);
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

procedure THorseCoreParamField.InitializeLhsBrackets(const AParams: TDictionary<string, string>; const AFieldName: string);
var
  LLhsBracketType: TLhsBracketsType;
begin
  FLhsBrackets := THorseCoreParamFieldLhsBrackets.Create;
  if THorseCoreParamConfig.GetInstance.CheckLhsBrackets then
  begin
    for LLhsBracketType := Low(TLhsBracketsType) to High(TLhsBracketsType) do
    begin
      if AParams.ContainsKey(FFieldName + LLhsBracketType.ToString) then
      begin
        FLhsBrackets.SetValue(LLhsBracketType, AParams.Items[FFieldName+LLhsBracketType.ToString]);
        FLhsBrackets.Types := FLhsBrackets.Types + [LLhsBracketType];
      end;
    end;
  end;
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

procedure THorseCoreParamField.SaveToFile(const AFileName: String);
var
  LMemoryStream: TMemoryStream;
begin
  if AsStream = nil then
    Exit;

  LMemoryStream := TMemoryStream.Create;
  try
    LMemoryStream.LoadFromStream(AsStream);
    LMemoryStream.Position := 0;
    LMemoryStream.SaveToFile(AFileName);
  finally
    LMemoryStream.Free;
  end;
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
