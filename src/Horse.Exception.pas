unit Horse.Exception;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  fpjson,
  jsonparser,
  TypInfo,
{$ELSE}
  System.SysUtils,
  System.JSON,
  System.TypInfo,
{$ENDIF}
  Horse.Commons;

type
  EHorseException = class(Exception)
  strict private
    FError: string;
    FStatus: THTTPStatus;
    FType: TMessageType;
    FTitle: string;
    FCode: Integer;
    FHint: string;
    FUnit: string;
    FDetail: string;
  public
    constructor Create; reintroduce;
    function Error(const AValue: string): EHorseException; overload;
    function Error: string; overload;
    function Status(const AValue: THTTPStatus): EHorseException; overload;
    function Status: THTTPStatus; overload;
    function &Type(const AValue: TMessageType): EHorseException; overload;
    function &Type: TMessageType; overload;
    function Title(const AValue: string): EHorseException; overload;
    function Title: string; overload;
    function Code(const AValue: Integer): EHorseException; overload;
    function Code: Integer; overload;
    function Hint(const AValue: string): EHorseException; overload;
    function Hint: string; overload;
    function &Unit(const AValue: string): EHorseException; overload;
    function &Unit: string; overload;
    function Detail(const AValue: string): EHorseException; overload;
    function Detail: string; overload;
    function ToJSON: string; virtual;
    function ToJSONObject: TJSONObject; virtual;
    class function New: EHorseException;
  end;

implementation

constructor EHorseException.Create;
begin
  FError := EmptyStr;
  FStatus := THTTPStatus.InternalServerError;
  FCode := 0;
end;

class function EHorseException.New: EHorseException;
begin
  Result := EHorseException.Create;
end;

function EHorseException.Code(const AValue: Integer): EHorseException;
begin
  FCode := AValue;
  Result := Self;
end;

function EHorseException.&Type: TMessageType;
begin
  Result := FType;
end;

function EHorseException.&Type(const AValue: TMessageType): EHorseException;
begin
  FType := AValue;
  Result := Self;
end;

function EHorseException.Code: Integer;
begin
  Result := FCode;
end;

function EHorseException.&Unit: string;
begin
  Result := FUnit;
end;

function EHorseException.&Unit(const AValue: string): EHorseException;
begin
  FUnit := AValue;
  Result := Self;
end;

function EHorseException.Detail: string;
begin
  Result := FDetail;
end;

function EHorseException.Detail(const AValue: string): EHorseException;
begin
  FDetail := AValue;
  Result := Self;
end;

function EHorseException.Error: string;
begin
  Result := FError;
end;

function EHorseException.Error(const AValue: string): EHorseException;
begin
  FError := AValue;
  Self.Message := AValue;
  Result := Self;
end;

function EHorseException.Status: THTTPStatus;
begin
  Result := FStatus;
end;

function EHorseException.Status(const AValue: THTTPStatus): EHorseException;
begin
  FStatus := AValue;
  Result := Self;
end;

function EHorseException.Title(const AValue: string): EHorseException;
begin
  FTitle := AValue;
  Result := Self;
end;

function EHorseException.Title: string;
begin
  Result := FTitle;
end;

function EHorseException.Hint(const AValue: string): EHorseException;
begin
  FHint := AValue;
  Result := Self;
end;

function EHorseException.Hint: string;
begin
  Result := FHint;
end;

function EHorseException.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON := ToJSONObject;
  try
    Result := {$IF DEFINED(FPC)}LJSON.AsJSON{$ELSE}{$IF CompilerVersion > 27.0}LJSON.ToJSON{$ELSE}LJSON.ToString{$ENDIF}{$ENDIF};
  finally
    LJSON.Free;
  end;
end;

function EHorseException.ToJSONObject: TJsonObject;
begin
  Result := TJSONObject.Create;

  if FType <> TMessageType.Default then
    Result.{$IF DEFINED(FPC)}Add{$ELSE}AddPair{$ENDIF}('type', GetEnumName(TypeInfo(TMessageType), Integer(FType)));

  if not FTitle.Trim.IsEmpty then
    Result.{$IF DEFINED(FPC)}Add{$ELSE}AddPair{$ENDIF}('title', FTitle);

  if FCode <> 0 then
    Result.{$IF DEFINED(FPC)}Add{$ELSE}AddPair{$ENDIF}('code', {$IF DEFINED(FPC)}TJSONIntegerNumber{$ELSE}TJSONNumber{$ENDIF}.Create(FCode));

  Result.{$IF DEFINED(FPC)}Add{$ELSE}AddPair{$ENDIF}('error', FError);

  if not FHint.Trim.IsEmpty then
    Result.{$IF DEFINED(FPC)}Add{$ELSE}AddPair{$ENDIF}('hint', FHint);

  if not FUnit.Trim.IsEmpty then
    Result.{$IF DEFINED(FPC)}Add{$ELSE}AddPair{$ENDIF}('unit', FUnit);

  if not FDetail.Trim.IsEmpty then
    Result.{$IF DEFINED(FPC)}Add{$ELSE}AddPair{$ENDIF}('detail', FDetail);
end;

end.
