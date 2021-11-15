unit Horse.Rtti;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
    SysUtils, RTTI,
  {$ELSE}
    System.SysUtils, System.Rtti,
  {$ENDIF}
  Horse.Commons;

type
  THorseRtti = class
  private
    { private declarations }
    class var FHorseRtti: THorseRtti;

    FContext: TRttiContext;

  protected
    class function GetDefaultHorseRtti: THorseRtti;

  public
    function GetType (AClass: TClass): TRttiType;

    constructor Create; virtual;
    class destructor UnInitialize; {$IFNDEF FPC} virtual; {$ENDIF}
    class function GetInstance: THorseRtti;
  end;

  THorseRttiTypeHelper = class helper for TRttiType
  public
    {$IF NOT DEFINED(FPC)}
    function FieldValueAsObject(AInstance: Pointer; const AFieldName: String): TObject;
    {$ENDIF}
  end;

implementation

{ THorseRtti }

constructor THorseRtti.Create;
begin
  if FHorseRtti <> nil then
    raise Exception.Create('The Horse Rtti instance has already been created');

  FContext := TRttiContext.Create;
  FHorseRtti := Self;
end;

class function THorseRtti.GetDefaultHorseRtti: THorseRtti;
begin
  if FHorseRtti = nil then
    FHorseRtti := THorseRtti.Create;
  Result := FHorseRtti;
end;

class function THorseRtti.GetInstance: THorseRtti;
begin
  Result := GetDefaultHorseRtti;
end;

function THorseRtti.GetType(AClass: TClass): TRttiType;
begin
  result := FContext.GetType(AClass);
end;

class destructor THorseRtti.UnInitialize;
begin
  if FHorseRtti <> nil then
    FreeAndNil(FHorseRtti);
end;

{ THorseRttiTypeHelper }

{$IF NOT DEFINED(FPC)}
function THorseRttiTypeHelper.FieldValueAsObject(AInstance: Pointer; const AFieldName: String): TObject;
var
  LField: TRttiField;
begin
  result := nil;
  LField := GetField(AFieldName);
  if Assigned(LField) then
    result := LField.GetValue(AInstance).AsObject;
end;
{$ENDIF}

end.

