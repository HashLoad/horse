unit Horse.Rtti;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  RTTI;
{$ELSE}
  System.Rtti;
{$ENDIF}
type
  THorseRtti = class
  private
    class var FHorseRtti: THorseRtti;
    FContext: TRttiContext;
  protected
    class function GetDefaultHorseRtti: THorseRtti;
  public
    function GetType(const AClass: TClass): TRttiType;
    constructor Create; virtual;
    class destructor UnInitialize; {$IFNDEF FPC}virtual;{$ENDIF}
    class function GetInstance: THorseRtti;
  end;

implementation

uses
{$IF DEFINED(FPC)}
  SysUtils;
{$ELSE}
  System.SysUtils;
{$ENDIF}

constructor THorseRtti.Create;
begin
  if Assigned(FHorseRtti) then
    raise Exception.Create('The Horse Rtti instance has already been created');
  FContext := TRttiContext.Create;
  FHorseRtti := Self;
end;

class function THorseRtti.GetDefaultHorseRtti: THorseRtti;
begin
  if not Assigned(FHorseRtti) then
    FHorseRtti := THorseRtti.Create;
  Result := FHorseRtti;
end;

class function THorseRtti.GetInstance: THorseRtti;
begin
  Result := GetDefaultHorseRtti;
end;

function THorseRtti.GetType(const AClass: TClass): TRttiType;
begin
  Result := FContext.GetType(AClass);
end;

class destructor THorseRtti.UnInitialize;
begin
  FreeAndNil(FHorseRtti);
end;

end.

