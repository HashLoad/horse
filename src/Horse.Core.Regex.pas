unit Horse.Core.Regex;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
  RegExpr,
  SysUtils;
  {$ELSE}
  System.RegularExpressions,
  System.SysUtils;
  {$ENDIF}

type
  THorseRegex = class
  private
    {$IF DEFINED(FPC)}
    FRegExpr: TRegExpr;
    {$ELSE}
    FRegEx: TRegEx;
    {$ENDIF}
  public
    constructor Create(const APattern: string);
    destructor Destroy; override;
    function Match(const AInput: string): Boolean;
  end;

implementation

constructor THorseRegex.Create(const APattern: string);
begin
  {$IF DEFINED(FPC)}
  FRegExpr := TRegExpr.Create(APattern);
  FRegExpr.ModifierI := True;
  {$ELSE}
  FRegEx := TRegEx.Create(APattern, [roCompiled, roIgnoreCase]);
  {$ENDIF}
end;

destructor THorseRegex.Destroy;
begin
  {$IF DEFINED(FPC)}
  FRegExpr.Free;
  {$ENDIF}
  inherited;
end;

function THorseRegex.Match(const AInput: string): Boolean;
begin
  {$IF DEFINED(FPC)}
  Result := FRegExpr.Exec(AInput);
  {$ELSE}
  Result := FRegEx.IsMatch(AInput);
  {$ENDIF}
end;

end.
