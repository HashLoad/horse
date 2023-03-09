unit Horse.Core.Files;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Classes;
{$ELSE}
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;
{$ENDIF}

type
  THorseCoreFile = class
  private
    FFileName: string;
    FName: string;
    FFileStream: TStream;
    FFreeContentStream: Boolean;
    FContentType: string;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function ContentType: string;
    function ContentStream: TStream;
    function Size: Int64;
    property Name: string read FName;
    property FreeContentStream: Boolean read FFreeContentStream write FFreeContentStream;
  end;

implementation

uses
  Horse.Mime;

constructor THorseCoreFile.Create(const AFileName: string);
begin
  if AFileName = EmptyStr then
    raise Exception.Create('Invalid FileName');

  if not FileExists(AFileName) then
    raise Exception.Create('File not exist');

  FFileName := AFileName;
  FName := ExtractFileName(FFileName);
  FFreeContentStream := True;
  FContentType := THorseMimeTypes.GetFileType(FFileName);
end;

destructor THorseCoreFile.Destroy;
begin
  if FFreeContentStream then
    FFileStream.Free;
  inherited;
end;

function THorseCoreFile.ContentType: string;
begin
  Result := FContentType;
end;

function THorseCoreFile.ContentStream: TStream;
begin
  if not Assigned(FFileStream) then
    FFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  Result := FFileStream;
end;

function THorseCoreFile.Size: Int64;
begin
  Result := ContentStream.Size;
end;

end.
