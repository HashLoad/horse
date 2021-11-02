unit Horse.Core.Param;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Classes, Generics.Collections, fpHTTP, fphttpserver, HTTPDefs,
{$ELSE}
  System.Classes, System.SysUtils, System.Generics.Collections,
{$ENDIF}
  Horse.Exception,
  Horse.Commons;

type
  THorseList = TDictionary<string, string>;

  THorseCoreParam = class
  private
    FParams: THorseList;
    FContent: TStrings;

    function GetItem(const Key: String): String;
    function GetCount: Integer;

    procedure RaiseHorseException(const AMessage: String); overload;
    procedure RaiseHorseException(const AMessage: String; const Args: array of const); overload;
    function GetContent: TStrings;

  public
    function AsBoolean(const Key: String; bRequired: Boolean = True; TrueValue: string = 'true'): Boolean;
    function AsString(const Key: String; bRequired: Boolean = True): string;
    function AsInteger(const Key: String; bRequired: Boolean = True): Integer;
    function AsFloat(const Key: String; bRequired: Boolean = True): Double;
    function AsCurrency(const Key: String; bRequired: Boolean = True): Currency;
    function AsDate(const Key: string; bRequired: Boolean = True; DateFormat: string = 'yyyy-MM-dd'): TDateTime;
    function AsDateTime(const Key: string; bRequired: Boolean = True; DateFormat: string = 'yyyy-MM-dd'; TimeFormat: String = 'hh:mm:ss'): TDateTime;
    function AsTime(const Key: string; bRequired: Boolean = True; TimeFormat: string = 'hh:mm:ss'): TTime;

    function TryGetValue(const Key: String; var Value: String): Boolean;
    function ContainsKey(const Key: String): Boolean;
    function ContainsValue(const Value: String): Boolean;
    function ToArray: TArray<TPair<String, String>>;

    property Content: TStrings read GetContent;
    property Count: Integer read GetCount;
    property Items[const Key: String]: String read GetItem; default;

    constructor create(AParams: THorseList);
    destructor Destroy; override;
end;

implementation

{ THorseCoreParam }

function THorseCoreParam.AsBoolean(const Key: String; bRequired: Boolean; TrueValue: String): Boolean;
var
  LStrParam: String;
begin
  result := False;
  LStrParam := AsString(Key, bRequired);
  try
    if LStrParam <> EmptyStr then
      result := LowerCase(LStrParam) = LowerCase(TrueValue);
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a numeric type.', [Key, LStrParam]);
  end;
end;

function THorseCoreParam.AsCurrency(const Key: String; bRequired: Boolean): Currency;
begin
  result := AsFloat(Key, bRequired);
end;

function THorseCoreParam.AsDate(const Key: string; bRequired: Boolean; DateFormat: string): TDateTime;
var
  LStrParam: String;
  LFormat: TFormatSettings;
begin
  result := 0;
  LStrParam := AsString(Key, bRequired);
  try
    if LStrParam <> EmptyStr then
    begin
      {$IF NOT DEFINED(FPC)}
      LFormat := TFormatSettings.Create;
      {$ENDIF}
      LFormat.ShortDateFormat := DateFormat;
      result := StrToDate(Copy(LStrParam, 1, Length(DateFormat)), LFormat);
    end;
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a date type.', [Key, LStrParam]);
  end;
end;

function THorseCoreParam.AsDateTime(const Key: string; bRequired: Boolean; DateFormat: string; TimeFormat: String): TDateTime;
var
  LStrParam: String;
  LFormat: TFormatSettings;
begin
  result := 0;
  LStrParam := AsString(Key, bRequired);
  try
    if LStrParam <> EmptyStr then
    begin
      {$IF NOT DEFINED(FPC)}
      LFormat := TFormatSettings.Create;
      {$ENDIF}
      LFormat.ShortDateFormat := DateFormat;
      LFormat.ShortTimeFormat := TimeFormat;
      result := StrToDateTime(LStrParam, LFormat);
    end;
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a date type.', [Key, LStrParam]);
  end;
end;

function THorseCoreParam.AsFloat(const Key: String; bRequired: Boolean): Double;
var
  LStrParam: String;
begin
  result := 0;
  LStrParam := AsString(Key, bRequired);
  try
    if LStrParam <> EmptyStr then
      result := StrToFloat(LStrParam);
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a numeric type.', [Key, LStrParam]);
  end;
end;

function THorseCoreParam.AsInteger(const Key: String; bRequired: Boolean): Integer;
var
  LStrParam: String;
begin
  result := 0;
  LStrParam := AsString(Key, bRequired);
  try
    if LStrParam <> EmptyStr then
      result := StrToInt(LStrParam);
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a integer type.', [Key, LStrParam]);
  end;
end;

function THorseCoreParam.AsString(const Key: String; bRequired: Boolean): string;
begin
  result := EmptyStr;
  if ContainsKey(Key) then
    result := FParams[Key]
  else
  if bRequired then
    RaiseHorseException('The %s param is required.', [Key]);
end;

function THorseCoreParam.AsTime(const Key: string; bRequired: Boolean; TimeFormat: string): TTime;
var
  LStrParam: String;
  LFormat: TFormatSettings;
begin
  result := 0;
  LStrParam := AsString(Key, bRequired);
  try
    if LStrParam <> EmptyStr then
    begin
      {$IF NOT DEFINED(FPC)}
      LFormat := TFormatSettings.Create;
      {$ENDIF}
      LFormat.ShortTimeFormat := TimeFormat;
      result := StrToDateTime(Copy(LStrParam, 1, Length(TimeFormat)), LFormat);
    end;
  except
    on e: EConvertError do
      RaiseHorseException('The %s param ''%s'' is not valid a time type.', [Key, LStrParam]);
  end;
end;

function THorseCoreParam.ContainsKey(const Key: String): Boolean;
begin
  result := FParams.ContainsKey(Key);
end;

function THorseCoreParam.ContainsValue(const Value: String): Boolean;
begin
  result := FParams.ContainsValue(Value);
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
    try
      for LKey in FParams.Keys do
        FContent.Add(Format('%s=%s', [LKey, FParams[LKey]]));
    except
      FreeAndNil(FContent);
      raise;
    end;
  end;

  result := FContent;
end;

function THorseCoreParam.GetCount: Integer;
begin
  result := FParams.Count;
end;

function THorseCoreParam.GetItem(const Key: String): String;
begin
  result := FParams[Key];
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

function THorseCoreParam.TryGetValue(const Key: String; var Value: String): Boolean;
begin
  result := FParams.TryGetValue(Key, Value);
end;

end.
