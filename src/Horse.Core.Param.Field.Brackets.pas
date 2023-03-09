unit Horse.Core.Param.Field.Brackets;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Classes,
{$ELSE}
  System.SysUtils,
  System.Classes,
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
    property Eq: string read FEq;
    property Ne: string read FNe;
    property Lt: string read FLt;
    property Lte: string read FLte;
    property Gt: string read FGt;
    property Gte: string read FGte;
    property Range: string read FRange;
    property Like: string read FLike;
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
  else
    Result := FEq;
  end;
end;

end.
