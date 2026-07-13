unit FrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.IOUtils;

type
  TFrmMain = class(TForm)
    lblInput: TLabel;
    edtInput: TEdit;
    btnBrowseInput: TButton;
    lblOutput: TLabel;
    edtOutput: TEdit;
    btnBrowseOutput: TButton;
    chkRecursive: TCheckBox;
    btnCompile: TButton;
    memLog: TMemo;
    FolderDialog: TFileOpenDialog;
    procedure btnBrowseInputClick(Sender: TObject);
    procedure btnBrowseOutputClick(Sender: TObject);
    procedure btnCompileClick(Sender: TObject);
  private
    function SelectFolder(const APrompt: string; var AFolder: string): Boolean;
    procedure Log(const AMsg: string);
    procedure ProcessDirectory(const AInput, AOutput: string; ARecursive: Boolean);
  public
  end;

var
  FrmMainForm: TFrmMain;

implementation

{$R *.dfm}

uses
  Horse.Protobuf.Compiler.Engine;

procedure TFrmMain.Log(const AMsg: string);
begin
  memLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + AMsg);
end;

function TFrmMain.SelectFolder(const APrompt: string; var AFolder: string): Boolean;
begin
  FolderDialog.Title := APrompt;
  FolderDialog.Options := [fdoPickFolders, fdoPathMustExist];
  Result := FolderDialog.Execute;
  if Result then
    AFolder := FolderDialog.FileName;
end;

procedure TFrmMain.btnBrowseInputClick(Sender: TObject);
var
  Folder: string;
begin
  if SelectFolder('Selecione a pasta com os arquivos .proto', Folder) then
    edtInput.Text := Folder;
end;

procedure TFrmMain.btnBrowseOutputClick(Sender: TObject);
var
  Folder: string;
begin
  if SelectFolder('Selecione a pasta de destino para as units .pas', Folder) then
    edtOutput.Text := Folder;
end;

procedure TFrmMain.ProcessDirectory(const AInput, AOutput: string; ARecursive: Boolean);
var
  Files: TArray<string>;
  ProtoFile: string;
  DestFile: string;
  RelPath: string;
  SuccessCount: Integer;
  ErrorCount: Integer;
  SearchOption: TSearchOption;
begin
  SuccessCount := 0;
  ErrorCount := 0;
  
  if ARecursive then
    SearchOption := TSearchOption.soAllDirectories
  else
    SearchOption := TSearchOption.soTopDirectoryOnly;

  try
    Files := TDirectory.GetFiles(AInput, '*.proto', SearchOption);
  except
    on E: Exception do
    begin
      Log('Erro ao listar arquivos: ' + E.Message);
      Exit;
    end;
  end;

  Log(Format('Encontrado(s) %d arquivo(s) .proto para processamento...', [Length(Files)]));

  for ProtoFile in Files do
  begin
    RelPath := ExtractRelativePath(AInput, ProtoFile);
    DestFile := TPath.Combine(AOutput, ChangeFileExt(RelPath, '.pas'));
    
    try
      TDirectory.CreateDirectory(TPath.GetDirectoryName(DestFile));
      Log('Compilando: ' + ExtractFileName(ProtoFile) + ' -> ' + RelPath.Replace('.proto', '.pas'));
      
      CompileProto(ProtoFile, DestFile);
      
      Log('  [OK] Sucesso.');
      Inc(SuccessCount);
    except
      on E: Exception do
      begin
        Log('  [ERRO] Falha ao compilar ' + ExtractFileName(ProtoFile) + ': ' + E.Message);
        Inc(ErrorCount);
      end;
    end;
  end;

  Log(Format('Concluido! Sucesso: %d | Falhas: %d', [SuccessCount, ErrorCount]));
end;

procedure TFrmMain.btnCompileClick(Sender: TObject);
var
  InputFolder: string;
  OutputFolder: string;
begin
  InputFolder := Trim(edtInput.Text);
  OutputFolder := Trim(edtOutput.Text);

  if (InputFolder = '') or (OutputFolder = '') then
  begin
    ShowMessage('Por favor, preencha os diretorios de entrada e saida.');
    Exit;
  end;

  if not TDirectory.Exists(InputFolder) then
  begin
    ShowMessage('Diretorio de entrada nao existe.');
    Exit;
  end;

  memLog.Clear;
  Log('Iniciando processamento em lote...');
  btnCompile.Enabled := False;
  try
    ProcessDirectory(InputFolder, OutputFolder, chkRecursive.Checked);
  finally
    btnCompile.Enabled := True;
  end;
end;

end.
