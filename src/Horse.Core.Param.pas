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
    { PATCH-PARAM-1 (resolves FOLLOW-UP-MEM-1) — streams whose ownership was
      transferred to this param via AddStream(..., AOwnsStream=True).  FFiles is
      a non-owning lookup dictionary; FOwnedStreams owns the actual objects and
      frees them on Clear (pool recycle) and Destroy. }
    FOwnedStreams: TObjectList<TStream>;
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
    procedure Clear;
    function Field(const AKey: string): THorseCoreParamField;
    function ContainsKey(const AKey: string): Boolean;
    function ContainsValue(const AValue: string): Boolean;
    function ToArray: TArray<TPair<string, string>>;
    function TryGetValue(const AKey: string; var AValue: string): Boolean;

    property Content: TStrings read GetContent;
    property Count: Integer read GetCount;
    property Items[const AKey: string]: string read GetItem; default;
    property Dictionary: THorseList read GetDictionary;

    { AddStream — store a stream-backed field (e.g. a multipart file upload),
      retrievable via Field(AKey).AsStream.
        AOwnsStream = False (default / 2-arg overload): the stream is owned
          elsewhere (e.g. CrossSocket's THttpMultiPartFormData) — never freed
          here.  Preserves the historical behaviour for every existing caller.
        AOwnsStream = True: ownership is transferred to this param — Clear and
          Destroy free the stream.  Used by the mORMot bridge, which synthesises
          a TMemoryStream per file part with no other owner (PATCH-PARAM-1). }
    function AddStream(const AKey: string; const AContent: TStream): THorseCoreParam; overload;
    function AddStream(const AKey: string; const AContent: TStream; const AOwnsStream: Boolean): THorseCoreParam; overload;

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
  Horse.Utils,
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
  FreeAndNil(FFields);
  { PATCH-PARAM-1 — free owned streams (no-op when none were transferred).
    FFiles is non-owning, so order vs. FreeAndNil(FFiles) is irrelevant. }
  FreeAndNil(FOwnedStreams);
  FreeAndNil(FFiles);
  inherited;
end;

function THorseCoreParam.Required(const AValue: Boolean): THorseCoreParam;
begin
  Result := Self;
  FRequired := AValue;
end;

{-----------------------------------------------------------------------------
 Limpa valores e campos cacheados para reutilizar o objeto em outra requisicao.
 -----------------------------------------------------------------------------}
procedure THorseCoreParam.Clear;
begin
  FParams.Clear;

  if Assigned(FContent) then
    FreeAndNil(FContent);

  ClearFields;

  { PATCH-PARAM-1 — free owned streams BEFORE clearing the (non-owning) FFiles
    lookup dictionary, so a pooled context does not accumulate one leaked
    TMemoryStream per multipart upload. }
  if Assigned(FOwnedStreams) then
    FOwnedStreams.Clear;

  if Assigned(FFiles) then
    FFiles.Clear;
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
var
  LVal: string;
begin
  Result := FParams.TryGetValue(AKey, LVal);
  if Result then
  begin
    AValue := DecodeParam(LVal);
    if AValue <> LVal then
      FParams.AddOrSetValue(AKey, AValue);
  end;
end;

function THorseCoreParam.GetItem(const AKey: string): string;
var
  LVal: string;
begin
  if FParams.TryGetValue(AKey, LVal) then
  begin
    Result := DecodeParam(LVal);
    if Result <> LVal then
      FParams.AddOrSetValue(AKey, Result);
  end
  else
    Result := '';
end;

function THorseCoreParam.GetDictionary: THorseList;
begin
  Result := FParams;
end;

function THorseCoreParam.GetCount: Integer;
begin
  Result := Integer(FParams.Count);
end;

function THorseCoreParam.AddStream(const AKey: string; const AContent: TStream): THorseCoreParam;
begin
  { Backward-compatible overload — non-owning (historical behaviour). }
  Result := AddStream(AKey, AContent, False);
end;

function THorseCoreParam.AddStream(const AKey: string; const AContent: TStream; const AOwnsStream: Boolean): THorseCoreParam;
begin
  Result := Self;
  if not Assigned(FFiles) then
    FFiles := TDictionary<string, TStream>.Create;
  FFiles.AddOrSetValue(AKey, AContent);

  { PATCH-PARAM-1 — when ownership is transferred, track the stream so Clear and
    Destroy free it.  Guard against double-registration of the same instance. }
  if AOwnsStream and Assigned(AContent) then
  begin
    if not Assigned(FOwnedStreams) then
      FOwnedStreams := TObjectList<TStream>.Create(True {AOwnsObjects});
    if FOwnedStreams.IndexOf(AContent) < 0 then
      FOwnedStreams.Add(AContent);
  end;
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
    FFields.Clear;
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
      FContent.Add(LPair.Key + '=' + DecodeParam(LPair.Value));
  end;
  Result := FContent;
end;

function THorseCoreParam.ToArray: TArray<TPair<string, string>>;
var
  I: Integer;
begin
  Result := FParams.ToArray;
  for I := 0 to Length(Result) - 1 do
  begin
    Result[I].Value := DecodeParam(Result[I].Value);
    FParams.AddOrSetValue(Result[I].Key, Result[I].Value);
  end;
end;

end.
