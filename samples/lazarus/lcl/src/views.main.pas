unit Views.Main;

{$MODE DELPHI}{$H+}

interface

uses Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, Horse;

type
  TFrmMain = class(TForm)
    btnStart: TBitBtn;
    btnStop: TBitBtn;
    edtPort: TEdit;
    Label1: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Status;
    procedure Start;
    procedure Stop;
  end;

var
  FrmMain: TFrmMain;

implementation

procedure DoPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

{$R *.lfm}

procedure TFrmMain.btnStartClick(Sender: TObject);
begin
  Start;
  Status;
end;

procedure TFrmMain.btnStopClick(Sender: TObject);
begin
  Stop;
  Status;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  THorse.Get('/ping', DoPing);
end;

procedure TFrmMain.Status;
begin
  btnStop.Enabled := THorse.IsRunning;
  btnStart.Enabled := not THorse.IsRunning;
  edtPort.Enabled := not THorse.IsRunning;
end;

procedure TFrmMain.Start;
begin
  // Need to set "HORSE_LCL" compilation directive
  THorse.Listen(StrToInt(edtPort.Text));
end;

procedure TFrmMain.Stop;
begin
  THorse.StopListen;
end;

end.

