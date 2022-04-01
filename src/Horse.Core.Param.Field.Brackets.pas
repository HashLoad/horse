unit Horse.Core.Param.Field.Brackets;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Classes,
{$ELSE}
  System.SysUtils, System.Classes,
{$ENDIF}
  Horse.Commons;

type
  THorseCoreParamFieldLhsBrackets = class
  private
    FEq: string;
    FNe: string;
    FLt: string;
    FLte: string;
    FGt: string;
    FGte: string;
    FRange: string;
    FLike: string;
    FTypes: TLhsBrackets;
  public
    property Eq: string read Feq;
    property Ne: string read Fne;
    property Lt: string read Flt;
    property Lte: string read Flte;
    property Gt: string read Fgt;
    property Gte: string read Fgte;
    property Range: string read Frange;
    property Like: string read Flike;
    property Types: TLhsBrackets read FTypes write FTypes;
    procedure SetValue(const AType: TLhsBracketsType; const AValue: string);
    function GetValue(const AType: TLhsBracketsType): string;
  end;

implementation

procedure THorseCoreParamFieldLhsBrackets.SetValue(const AType: TLhsBracketsType; const AValue: string);
begin
  case AType of
    TLhsBracketsType.Equal:
      FEq := AValue;
    TLhsBracketsType.NotEqual:
      FNe := AValue;
    TLhsBracketsType.LessThan:
      FLt := AValue;
    TLhsBracketsType.LessThanOrEqual:
      FLte := AValue;
    TLhsBracketsType.GreaterThan:
      FGt := AValue;
    TLhsBracketsType.GreaterThanOrEqual:
      FGte := AValue;
    TLhsBracketsType.Range:
      FRange := AValue;
    TLhsBracketsType.Like:
      FLike := AValue;
  end;
end;

function THorseCoreParamFieldLhsBrackets.GetValue(const AType: TLhsBracketsType): string;
begin
  case AType of
    TLhsBracketsType.Equal:
      Result := FEq;
    TLhsBracketsType.NotEqual:
      Result := FNe;
    TLhsBracketsType.LessThan:
      Result := FLt;
    TLhsBracketsType.LessThanOrEqual:
      Result := FLte;
    TLhsBracketsType.GreaterThan:
      Result := FGt;
    TLhsBracketsType.GreaterThanOrEqual:
      Result := FGte;
    TLhsBracketsType.Range:
      Result := FRange;
    TLhsBracketsType.Like:
      Result := FLike;
  end;
end;

end.

