unit Horse.EnvironmentVariables;

interface

uses
  System.Classes;

type

  THorseEnvironmentVariables = class
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
    class function GetEnvironmentVariables: TStringList;
    class function GetEnvironmentVariable(const AName: string): string;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils;

{ THorseEnvironmentVariables }

class function THorseEnvironmentVariables.GetEnvironmentVariable(const AName: string): string;
begin
  Result := System.SysUtils.GetEnvironmentVariable(AName);
end;

{$IFDEF LINUX}
class function THorseEnvironmentVariables.GetEnvironmentVariables: TStringList;
var
  LEnvVar: PPChar;
begin
  Result := TStringList.Create;
  LEnvVar := System.envp;
  while (LEnvVar <> nil) and (LEnvVar^ <> nil) do
  begin
    Result.Add(LEnvVar^);
    Inc(LEnvVar);
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function THorseEnvironmentVariables.GetEnvironmentVariables: TStringList;
var
  LEnvBlock: PChar;
  LEnvVar: PChar;
begin
  Result := TStringList.Create;
  LEnvBlock := GetEnvironmentStrings;
  LEnvVar := LEnvBlock;
  while LEnvVar^ <> #0 do
  begin
    Result.Add(LEnvVar);
    LEnvVar := LEnvVar + StrLen(LEnvVar) + 1;
  end;
  FreeEnvironmentStrings(LEnvBlock);
end;
{$ENDIF}

end.
