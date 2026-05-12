unit Horse.Core.Param;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Classes,
  DateUtils,
  Generics.Collections,
  fpHTTP,
  HTTPDefs,
{$ELSE}
  System.Classes,
  System.Generics.Collections,
{$ENDIF}
  Horse.Core.Param.Field;

type
  THorseList = TDictionary<string, string>;

  THorseCoreParam = class
  private
    FParams: THorseList;
    FFiles: TDictionary<string, TStream>;
    FFields: TDictionary<string, THorseCoreParamField>;
    FContent: TStrings;
    FRequired: Boolean;

    function GetItem(const AKey: string): string;
    function GetDictionary: THorseList;
    function GetCount: Integer;
    function GetContent: TStrings;
    function NewField(const AKey: string): THorseCoreParamField;
    procedure ClearFields;
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
{$IF DEFINED(FPC)}
  SysUtils,
{$ELSE}
  System.SysUtils,
{$ENDIF}
  Horse.Core.Param.Config;

constructor THorseCoreParam.Create(const AParams: THorseList);
begin
  inherited Create;
  FParams := AParams;
  FRequired := False;
end;

destructor THorseCoreParam.Destroy;
begin
  FParams.Free;
  FContent.Free;
  ClearFields;
  FreeAndNil(FFiles);
  inherited;
end;

function THorseCoreParam.Required(const AValue: Boolean): THorseCoreParam;
begin
  Result := Self;
  FRequired := AValue;
end;

function THorseCoreParam.ContainsKey(const AKey: string): Boolean;
begin
  Result := FParams.ContainsKey(AKey);
end;

function THorseCoreParam.ContainsValue(const AValue: string): Boolean;
begin
  Result := FParams.ContainsValue(AValue);
end;

function THorseCoreParam.TryGetValue(const AKey: string; var AValue: string): Boolean;
begin
  Result := FParams.TryGetValue(AKey, AValue);
end;

function THorseCoreParam.GetItem(const AKey: string): string;
begin
  FParams.TryGetValue(AKey, Result);
end;

function THorseCoreParam.GetDictionary: THorseList;
begin
  Result := FParams;
end;

function THorseCoreParam.GetCount: Integer;
begin
  Result := FParams.Count;
end;

function THorseCoreParam.AddStream(const AKey: string; const AContent: TStream): THorseCoreParam;
begin
  Result := Self;
  if not Assigned(FFiles) then
    FFiles := TDictionary<string, TStream>.Create;
  FFiles.AddOrSetValue(AKey, AContent);
end;

function THorseCoreParam.NewField(const AKey: string): THorseCoreParamField;
var
  LStream: TStream;
begin
  if Assigned(FFiles) and FFiles.TryGetValue(AKey, LStream) then
    Exit(THorseCoreParamField.Create(LStream, AKey));
  Result := THorseCoreParamField.Create(FParams, AKey);
end;

function THorseCoreParam.Field(const AKey: string): THorseCoreParamField;
var
  LFieldName: string;
  LConfig: THorseCoreParamConfig;
begin
  if not Assigned(FFields) then
    FFields := TDictionary<string, THorseCoreParamField>.Create;

  LFieldName := AKey.ToLower;

  if FFields.TryGetValue(LFieldName, Result) then
    Exit;

  Result := NewField(AKey);

  try
    LConfig := THorseCoreParamConfig.GetInstance;

    Result
      .Required(FRequired)
      .DateFormat(LConfig.DateFormat)
      .InvalidFormatMessage(LConfig.InvalidFormatMessage)
      .RequiredMessage(LConfig.RequiredMessage)
      .ReturnUTC(LConfig.ReturnUTC)
      .TimeFormat(LConfig.TimeFormat)
      .TrueValue(LConfig.TrueValue);

    FFields.Add(LFieldName, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure THorseCoreParam.ClearFields;
var
  LField: THorseCoreParamField;
begin
  if Assigned(FFields) then
  begin
    for LField in FFields.Values do
      LField.Free;
    FreeAndNil(FFields);
  end;
end;

function THorseCoreParam.GetContent: TStrings;
var
  LPair: TPair<string, string>;
begin
  if not Assigned(FContent) then
  begin
    FContent := TStringList.Create;
    for LPair in FParams do
      FContent.Add(LPair.Key + '=' + LPair.Value);
  end;
  Result := FContent;
end;

function THorseCoreParam.ToArray: TArray<TPair<string, string>>;
begin
  Result := FParams.ToArray;
end;

end.
