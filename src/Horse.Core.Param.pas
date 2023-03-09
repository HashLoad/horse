unit Horse.Core.Param;

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
  fpHTTP,
  HTTPDefs,
{$ELSE}
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.Generics.Collections,
  Horse.Exception,
  Horse.Commons,
{$ENDIF}
  Horse.Core.Param.Field;

type
  THorseList = TDictionary<string, string>;

  THorseCoreParam = class
  private
    FParams: THorseList;
    FFiles: TDictionary<String, TStream>;
    FFields: TDictionary<string, THorseCoreParamField>;
    FContent: TStrings;
    FRequired: Boolean;
    function GetItem(const AKey: string): string;
    function GetDictionary: THorseList;
    function GetCount: Integer;
    function GetContent: TStrings;
    function AsString(const AKey: string): string;
    procedure ClearFields;

    function NewField(const AKey: String): THorseCoreParamField;
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
    function AddStream(const AKey: string; const AContent: TStream): THorseCoreParam;
    constructor Create(const AParams: THorseList);
    destructor Destroy; override;
  end;

implementation

uses
  Horse.Core.Param.Config;

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

constructor THorseCoreParam.Create(const AParams: THorseList);
begin
  FParams := AParams;
  FRequired := False;
end;

destructor THorseCoreParam.Destroy;
begin
  FParams.Free;
  FContent.Free;
  ClearFields;
  if Assigned(FFiles) then
    FFiles.Free;
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
    Exit(FFields.Items[LFieldName]);

  Result := NewField(AKey);
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

function THorseCoreParam.AddStream(const AKey: string; const AContent: TStream): THorseCoreParam;
begin
  Result := Self;
  if not Assigned(FFiles) then
    FFiles := TDictionary<String, TStream>.Create;

  FFiles.AddOrSetValue(AKey, AContent);
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

function THorseCoreParam.NewField(const AKey: String): THorseCoreParamField;
var
  LKey: String;
begin
  if Assigned(FFiles) then
  begin
    for LKey in FFiles.Keys do
    begin
      if AnsiSameText(LKey, AKey) then
      begin
        Result := THorseCoreParamField.Create(FFiles.Items[LKey], AKey);
        Exit;
      end;
    end;
  end;
  Result := THorseCoreParamField.Create(FParams, AKey);
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

end.
