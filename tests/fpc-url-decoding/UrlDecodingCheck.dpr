program UrlDecodingCheck;

{$MODE DELPHI}{$H+}

uses
  Horse.Utils;

procedure Check(const AInput, AExpected: string);
var
  LActual: string;
begin
  LActual := DecodeParam(AInput);
  if LActual <> AExpected then
  begin
    Writeln('DecodeParam failed for "', AInput, '": expected "', AExpected,
      '", got "', LActual, '"');
    Halt(1);
  end;
end;

begin
  Check('100%', '100%');
  Check('value%2', 'value%2');
  Check('50%off', '50%off');
  Check('%41%', '%41%');
  Check('100%25', '100%');
  Check('hello%20world', 'hello world');
  Check('%2fapi', '/api');
  Check('a+b c', 'a+b c');
end.
