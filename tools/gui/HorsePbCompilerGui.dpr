program HorsePbCompilerGui;

uses
  Vcl.Forms,
  FrmMain in 'FrmMain.pas' {FrmMain},
  Horse.Protobuf.Compiler.Engine in '..\compiler\Horse.Protobuf.Compiler.Engine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMainForm);
  Application.Run;
end.
