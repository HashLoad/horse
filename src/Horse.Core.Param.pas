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
  Horse.Exception, Horse.Commons, Horse.Core.Param.Field;

type
  THorseList = TDictionary<string, string>;

  THorseCoreParamConfig = class
  private
    class var FInstance: THorseCoreParamConfig;

    FRequiredMessage: string;
    FInvalidFormatMessage: string;
    FDateFormat: string;
    FTimeFormat: string;
    FReturnUTC: Boolean;
    FTrueValue: string;

    constructor Create;
  public
    function RequiredMessage(const AValue: string): THorseCoreParamConfig; overload;
    function RequiredMessage: string; overload;

    function InvalidFormatMessage(const AValue: string): THorseCoreParamConfig; overload;
    function InvalidFormatMessage: string; overload;

    function DateFormat(const AValue: string): THorseCoreParamConfig; overload;
    function DateFormat: string; overload;

    function TimeFormat(const AValue: string): THorseCoreParamConfig; overload;
    function TimeFormat: string; overload;

    function ReturnUTC(const AValue: Boolean): THorseCoreParamConfig; overload;
    function ReturnUTC: Boolean; overload;

    function TrueValue(const AValue: string): THorseCoreParamConfig; overload;
    function TrueValue: string; overload;

    destructor Destroy; override;

    class function GetInstance: THorseCoreParamConfig;
    class destructor UnInitialize;
  end;

  THorseCoreParam = class
  private
    FParams: THorseList;
    FFields: TDictionary<string, THorseCoreParamField>;
    FContent: TStrings;
    FRequired: Boolean;

    function GetItem(const AKey: string): string;
    function GetDictionary: THorseList;
    function GetCount: Integer;
    function GetContent: TStrings;

    procedure ClearFields;
    function AsString(const AKey: string): string;
  public
    function Required(const AValue: Boolean): THorseCoreParam;
    function Field(const AKey: string): THorseCoreParamField;
    function ContainsKey(const AKey: string): Boolean;
    function ContainsValue(const AValue: string): Boolean;
    function ToArray: TArray<TPair<string, string>>;
    function TryGetValue(const AKey: string; var AValue: string): Boolean;
    property Content: TStrings read GetContent;
    property Count: Integer read GetCount;
    property Items[const AKey: string]: string read GetItem; default;
    property Dictionary: THorseList read GetDictionary;
    constructor Create(AParams: THorseList);
    destructor Destroy; override;
  end;

implementation

{ THorseCoreParam }

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

constructor THorseCoreParam.Create(AParams: THorseList);
begin
  FParams := AParams;
  FRequired := False;
end;

destructor THorseCoreParam.Destroy;
begin
  FParams.Free;
  FContent.Free;
  ClearFields;
  inherited;
end;

function THorseCoreParam.Field(const AKey: string): THorseCoreParamField;
var
  LFieldName: string;
begin
  if not Assigned(FFields) then
    FFields := TDictionary<string, THorseCoreParamField>.Create;

  LFieldName := AKey.ToLower;
  if FFields.ContainsKey(LFieldName) then
    Exit( FFields.Items[LFieldName] );

  Result := THorseCoreParamField.create(FParams, AKey);
  try
    Result
      .Required(FRequired)
      .DateFormat(THorseCoreParamConfig.GetInstance.DateFormat)
      .InvalidFormatMessage(THorseCoreParamConfig.GetInstance.InvalidFormatMessage)
      .RequiredMessage(THorseCoreParamConfig.GetInstance.RequiredMessage)
      .ReturnUTC(THorseCoreParamConfig.GetInstance.ReturnUTC)
      .TimeFormat(THorseCoreParamConfig.GetInstance.TimeFormat)
      .TrueValue(THorseCoreParamConfig.GetInstance.TrueValue);

    FFields.AddOrSetValue(LFieldName, Result);
  except
    Result.Free;
    raise;
  end;
end;

function THorseCoreParam.AsString(const AKey: string): string;
var
  LKey: string;
begin
  Result := EmptyStr;
  for LKey in FParams.Keys do
  begin
    if AnsiCompareText(LKey, AKey) = 0 then
      Exit(FParams.Items[LKey]);
  end;
end;

procedure THorseCoreParam.ClearFields;
var
  LKey: string;
begin
  if Assigned(FFields) then
  begin
    for LKey in FFields.Keys do
      FFields.Items[LKey].Free;

    FFields.Free;
  end;
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

function THorseCoreParam.GetItem(const AKey: string): string;
var
  LKey: string;
begin
  for LKey in FParams.Keys do
  begin
    if AnsiCompareText(LKey, AKey) = 0 then
      Exit(FParams[LKey]);
  end;
  Result := EmptyStr;
end;

function THorseCoreParam.Required(const AValue: Boolean): THorseCoreParam;
begin
  Result := Self;
  FRequired := AValue;
end;

function THorseCoreParam.GetDictionary: THorseList;
begin
  Result := FParams;
end;

function THorseCoreParam.ToArray: TArray<TPair<string, string>>;
begin
  Result := FParams.ToArray;
end;

function THorseCoreParam.TryGetValue(const AKey: string; var AValue: string): Boolean;
begin
  Result := ContainsKey(AKey);
  if Result then
    AValue := AsString(AKey);
end;

{ THorseCoreParamConfig }

constructor THorseCoreParamConfig.Create;
begin
  FReturnUTC := True;
  FDateFormat := 'yyyy-MM-dd';
  FTimeFormat := 'hh:mm:ss';
  FTrueValue := 'true';
  FRequiredMessage := 'The %s param is required.';
  FInvalidFormatMessage := 'The %0:s param ''%1:s'' is not valid a %2:s type.';
end;

function THorseCoreParamConfig.DateFormat(const AValue: string): THorseCoreParamConfig;
begin
  Result := Self;
  FDateFormat := AValue;
end;

function THorseCoreParamConfig.DateFormat: string;
begin
  Result := FDateFormat;
end;

destructor THorseCoreParamConfig.Destroy;
begin

  inherited;
end;

class function THorseCoreParamConfig.GetInstance: THorseCoreParamConfig;
begin
  if not Assigned(FInstance) then
    FInstance := THorseCoreParamConfig.Create;
  Result := FInstance;
end;

function THorseCoreParamConfig.InvalidFormatMessage: string;
begin
  Result := FInvalidFormatMessage;
end;

function THorseCoreParamConfig.InvalidFormatMessage(const AValue: string): THorseCoreParamConfig;
begin
  Result := Self;
  FInvalidFormatMessage := AValue;
end;

function THorseCoreParamConfig.RequiredMessage(const AValue: string): THorseCoreParamConfig;
begin
  Result := Self;
  FRequiredMessage := AValue;
end;

function THorseCoreParamConfig.RequiredMessage: string;
begin
  Result := FRequiredMessage;
end;

function THorseCoreParamConfig.ReturnUTC(const AValue: Boolean): THorseCoreParamConfig;
begin
  Result := Self;
  FReturnUTC := AValue;
end;

function THorseCoreParamConfig.ReturnUTC: Boolean;
begin
  Result := FReturnUTC;
end;

function THorseCoreParamConfig.TimeFormat: string;
begin
  Result := FTimeFormat;
end;

function THorseCoreParamConfig.TimeFormat(const AValue: string): THorseCoreParamConfig;
begin
  Result := Self;
  FTimeFormat := AValue;
end;

function THorseCoreParamConfig.TrueValue(const AValue: string): THorseCoreParamConfig;
begin
  Result := Self;
  FTrueValue := AValue;
end;

function THorseCoreParamConfig.TrueValue: string;
begin
  Result := FTrueValue;
end;

class destructor THorseCoreParamConfig.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);
end;

end.
