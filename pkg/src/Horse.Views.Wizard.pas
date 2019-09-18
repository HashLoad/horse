unit Horse.Views.Wizard;

interface

uses WinAPI.Windows, WinAPI.Messages, WinAPI.ShellAPI, System.SysUtils, System.Variants, System.Classes, VCL.Graphics,
  VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.StdCtrls, VCL.Imaging.pngimage, VCL.ExtCtrls,
  dxGDIPlusClasses;

type
  TFrmNewProject = class(TForm)
    pnlContent: TPanel;
    chkAddToProjectGroup: TCheckBox;
    imgHeader: TImage;
    Image1: TImage;
    lblMiddlewares: TLabel;
    chkJhonson: TCheckBox;
    chkHorseCORS: TCheckBox;
    chkHorseOctetStream: TCheckBox;
    chkHorseJWT: TCheckBox;
    chkHorseBasicAuth: TCheckBox;
    chkHorseCompression: TCheckBox;
    chkHandleException: TCheckBox;
    Button1: TButton;
    Button2: TButton;
  private
    function GetAddToProjectGroup: Boolean;
  public
    property AddToProjectGroup: Boolean read GetAddToProjectGroup;
  end;

var
  FrmNewProject: TFrmNewProject;

implementation

uses Horse.CodeGen.Templates;

{$R *.dfm}

function TFrmNewProject.GetAddToProjectGroup: Boolean;
begin
  Result := chkAddToProjectGroup.Checked;
end;

end.
