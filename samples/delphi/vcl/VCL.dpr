program VCL;

uses
  Vcl.Forms,
  Main.Form in 'src\Main.Form.pas' {FrmVCL};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmVCL, FrmVCL);
  Application.Run;
end.
