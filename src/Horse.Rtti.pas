unit Horse.Rtti;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, RTTI,
{$ELSE}
  System.SysUtils,
  System.Rtti,
{$ENDIF}
  Horse.Commons;

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

function THorseRtti.GetType(const AClass: TClass): TRttiType;
begin
  Result := FContext.GetType(AClass);
end;

class destructor THorseRtti.UnInitialize;
begin
  if FHorseRtti <> nil then
    FreeAndNil(FHorseRtti);
end;

end.

