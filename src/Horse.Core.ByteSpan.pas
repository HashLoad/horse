unit Horse.Core.ByteSpan;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  SysUtils;

type
  PByteSpan = ^TByteSpan;
  TByteSpan = record
  private
    FOffset: Integer;
    FLength: Integer;
  public
    constructor Create(AOffset, ALength: Integer);
    property Offset: Integer read FOffset;
    property Length: Integer read FLength;
    function ToString(const ABuffer: TBytes): string;
    function IsEmpty: Boolean;
  end;

implementation

constructor TByteSpan.Create(AOffset, ALength: Integer);
begin
  FOffset := AOffset;
  FLength := ALength;
end;

function TByteSpan.IsEmpty: Boolean;
begin
  Result := FLength <= 0;
end;

function TByteSpan.ToString(const ABuffer: TBytes): string;
begin
  if IsEmpty or (ABuffer = nil) or (FOffset < 0) or (FOffset + FLength > System.Length(ABuffer)) then
    Result := ''
  else
    SetString(Result, PChar(@ABuffer[FOffset]), FLength);
end;

end.
