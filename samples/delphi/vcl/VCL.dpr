program VCL;

uses
  Vcl.Forms,
  uVCL in 'uVCL.pas' {frmVCL};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmVCL, frmVCL);
  Application.Run;
end.
