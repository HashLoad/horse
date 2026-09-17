unit Horse.Utils;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

function DecodeParam(const AValue: string): string;

implementation

uses
{$IF DEFINED(FPC)}
  SysUtils,
  httpprotocol;
{$ELSE}
  System.SysUtils,
  System.NetEncoding;
{$ENDIF}

function IsHexDigit(const AValue: Char): Boolean;
begin
  Result := ((AValue >= '0') and (AValue <= '9')) or
    ((AValue >= 'A') and (AValue <= 'F')) or
    ((AValue >= 'a') and (AValue <= 'f'));
end;

function HasInvalidPercentEncoding(const AValue: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := 1;
  while I <= Length(AValue) do
  begin
    if AValue[I] = '%' then
    begin
      if (I + 2 > Length(AValue)) or not IsHexDigit(AValue[I + 1]) or
        not IsHexDigit(AValue[I + 2]) then
        Exit(True);
      Inc(I, 3);
    end
    else
      Inc(I);
  end;
end;

function DecodeParam(const AValue: string): string;
begin
  if Pos('%', AValue) = 0 then
  begin
    Exit(AValue);
  end;

  { A literal or incomplete percent sign is not a percent-encoded octet.
    Keep malformed input unchanged instead of letting Delphi's URL decoder
    raise EConvertError. Valid %XX sequences retain the existing behavior. }
  if HasInvalidPercentEncoding(AValue) then
    Exit(AValue);

  {$IF DEFINED(FPC)}
    Result := HTTPDecode(AValue);
  {$ELSE}
    Result := TNetEncoding.URL.Decode(AValue);
  {$ENDIF}
end;

end.
