program SSL;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain};

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
