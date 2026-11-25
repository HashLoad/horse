program MimeTypeCheck;

{ MIME type by extension.

  GetExtType receives an extension, with or without the leading dot, or a
  file name. Before this fix it ran ExtractFileExt on the value it received:
  on FPC ExtractFileExt('.css') is '' (a hidden file without extension), and
  ExtractFileExt('css') is '' on every compiler, so both came back as
  application/octet-stream. GetFileType keeps its previous result for file
  names, hidden files and names without extension included. }

{$MODE DELPHI}{$H+}

uses
  Horse.Mime;

const
  OCTET_STREAM = 'application/octet-stream';

var
  GFailures: Integer;

procedure Check(const AKind, AInput, AActual, AExpected: string);
begin
  if AActual <> AExpected then
  begin
    Writeln(AKind, '("', AInput, '"): expected "', AExpected, '", got "', AActual, '"');
    Inc(GFailures);
  end;
end;

procedure CheckExt(const AInput, AExpected: string);
begin
  Check('GetExtType', AInput, THorseMimeTypes.GetExtType(AInput), AExpected);
end;

procedure CheckFile(const AInput, AExpected: string);
begin
  Check('GetFileType', AInput, THorseMimeTypes.GetFileType(AInput), AExpected);
end;

begin
  GFailures := 0;
  // Extension with the leading dot.
  CheckExt('.css', 'text/css');
  CheckExt('.js', 'application/javascript');
  // Extension without the dot, upper case, surrounding spaces.
  CheckExt('css', 'text/css');
  CheckExt('.CSS', 'text/css');
  CheckExt(' .CSS ', 'text/css');
  // File names keep working.
  CheckExt('style.min.js', 'application/javascript');
  CheckFile('/x/style.css', 'text/css');
  CheckFile('style.CSS', 'text/css');
  // GetFileType does not read a bare name as an extension.
  CheckFile('.css', OCTET_STREAM);
  CheckFile('pdf', OCTET_STREAM);
  // Unknown or empty.
  CheckExt('.naoexiste', OCTET_STREAM);
  CheckExt('', OCTET_STREAM);

  if GFailures > 0 then
  begin
    Writeln(GFailures, ' MIME check(s) failed.');
    Halt(1);
  end;
  Writeln('MIME checks passed.');
end.
