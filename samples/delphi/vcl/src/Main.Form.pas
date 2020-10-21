unit Main.Form;

interface

uses Winapi.Windows, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Buttons, System.SysUtils;

type
  TFrmVCL = class(TForm)
    lbStatus: TLabel;
    lbPorta: TLabel;
    btnStop: TBitBtn;
    btnStart: TBitBtn;
    procedure btnStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnStartClick(Sender: TObject);
  private
    procedure Status;
    procedure Start;
    procedure Stop;
  end;

var
  FrmVCL: TFrmVCL;

implementation

uses Horse;

{$R *.dfm}

procedure TFrmVCL.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if THorse.IsRunning then
    Stop;
end;

procedure TFrmVCL.Start;
begin
  THorse.Get('ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end;

procedure TFrmVCL.Status;
begin
  btnStop.Enabled := THorse.IsRunning;
  btnStart.Enabled := not THorse.IsRunning;
  if THorse.IsRunning then
  begin
    lbStatus.Caption := 'Status: Online';
    lbPorta.Caption := 'Port: ' + IntToStr(THorse.Port);
  end
  else
  begin
    lbStatus.Caption := 'Status: Offline';
    lbPorta.Caption := 'Port: ';
  end;
end;

procedure TFrmVCL.Stop;
begin
  THorse.StopListen;
end;

procedure TFrmVCL.btnStartClick(Sender: TObject);
begin
  Start;
  Status;
end;

procedure TFrmVCL.btnStopClick(Sender: TObject);
begin
  Stop;
  Status;
end;

end.
