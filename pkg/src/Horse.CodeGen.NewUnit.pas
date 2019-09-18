unit Horse.CodeGen.NewUnit;

interface

uses ToolsAPI;

type
  TNewUnit = class(TNotifierObject, IOTACreator, IOTAModuleCreator)
  private
    FPersonality: string;
  protected
    FFormName: string;
    FImplFileName: string;
    FIntfFileName: string;
    FAncestorName: string;
    procedure SetAncestorName(const AValue: string); virtual;
    procedure SetFormName(const AValue: string); virtual;
    procedure SetImplFileName(const AValue: string); virtual;
    procedure SetIntfFileName(const AValue: string); virtual;
    { IOTACreator }
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    { IOTAModuleCreator }
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    procedure FormCreated(const FormEditor: IOTAFormEditor); virtual;
  public
    property FormName: string read GetFormName write SetFormName;
    property ImplFileName: string read GetImplFileName write SetImplFileName;
    property IntfFileName: string read GetIntfFileName write SetIntfFileName;
    property AncestorName: string read GetAncestorName write SetAncestorName;
    property Personality: string read FPersonality write FPersonality;
  end;

implementation

uses System.SysUtils;

{ TUnitCreator }

procedure TNewUnit.FormCreated(const FormEditor: IOTAFormEditor);
begin

end;

function TNewUnit.GetAncestorName: string;
begin
  Result := FAncestorName;
end;

function TNewUnit.GetCreatorType: string;
begin
  Result := sUnit;
end;

function TNewUnit.GetExisting: Boolean;
begin
  Result := False;
end;

function TNewUnit.GetFileSystem: string;
begin
  Result := EmptyStr;
end;

function TNewUnit.GetFormName: string;
begin
  Result := FFormName;
end;

function TNewUnit.GetImplFileName: string;
begin
  Result := FImplFileName;
end;

function TNewUnit.GetIntfFileName: string;
begin
  Result := FIntfFileName;
end;

function TNewUnit.GetMainForm: Boolean;
begin
  Result := False;
end;

function TNewUnit.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TNewUnit.GetShowForm: Boolean;
begin
  Result := False;
end;

function TNewUnit.GetShowSource: Boolean;
begin
  Result := True;
end;

function TNewUnit.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TNewUnit.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TNewUnit.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TNewUnit.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TNewUnit.SetAncestorName(const AValue: string);
begin
  FAncestorName := AValue;
end;

procedure TNewUnit.SetFormName(const AValue: string);
begin
  FFormName := AValue;
end;

procedure TNewUnit.SetImplFileName(const AValue: string);
begin
  FImplFileName := AValue;
end;

procedure TNewUnit.SetIntfFileName(const AValue: string);
begin
  FIntfFileName := AValue;
end;

end.
