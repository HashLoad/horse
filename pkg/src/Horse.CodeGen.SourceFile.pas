unit Horse.CodeGen.SourceFile;

interface

uses System.SysUtils, System.Classes, ToolsAPI;

type
  TSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    function GetSource: string;
    function GetAge: TDateTime;
    constructor Create(const ASource: string; const AArgs: array of const);
  end;

implementation

{ TSourceFile }

constructor TSourceFile.Create(const ASource: string; const AArgs: array of const);
begin
  FSource := Format(ASource, AArgs);
end;

function TSourceFile.GetAge: TDateTime;
begin
  Result := Now;
end;

function TSourceFile.GetSource: string;
begin
  Result := FSource;
end;

end.

