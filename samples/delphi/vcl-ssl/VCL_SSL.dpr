program VCL_SSL;

uses
  Vcl.Forms,
  Main.Form in 'src\Main.Form.pas' {frmMain};

{$R *.res}

begin

{$IFDEF MSWINDOWS}
  IsConsole := False;
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;

end.
