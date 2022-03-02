unit Horse.Rtti.Helper;

interface

uses
{$IF DEFINED(FPC)}
  RTTI;
{$ELSE}
  System.Rtti;
{$ENDIF}

type
  THorseRttiTypeHelper = class helper for TRttiType
  public
    {$IF NOT DEFINED(FPC)}
    function FieldValueAsObject(const AInstance: Pointer; const AFieldName: string): TObject;
    {$ENDIF}
  end;

implementation

{$IF NOT DEFINED(FPC)}
function THorseRttiTypeHelper.FieldValueAsObject(const AInstance: Pointer; const AFieldName: string): TObject;
var
  LField: TRttiField;
begin
  Result := nil;
  LField := GetField(AFieldName);
  if Assigned(LField) then
    Result := LField.GetValue(AInstance).AsObject;
end;
{$ENDIF}

end.
