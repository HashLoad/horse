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

function DecodeParam(const AValue: string): string;
begin
  if Pos('%', AValue) = 0 then
    Exit(AValue);
    
  {$IF DEFINED(FPC)}
  Result := HTTPDecode(AValue);
  {$ELSE}
  Result := TNetEncoding.URL.Decode(AValue);
  {$ENDIF}
end;

end.
