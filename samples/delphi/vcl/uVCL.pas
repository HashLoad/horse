unit uVCL;

interface

uses
  Winapi.Windows, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, System.SysUtils;

type
  TfrmVCL = class(TForm)
    lbStatus: TLabel;
    lbPorta: TLabel;
    btnStartStop: TBitBtn;
    procedure btnStartStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FRunning: Boolean;
    procedure Start;
    procedure Stop;
  public
    { Public declarations }
  end;

var
  frmVCL: TfrmVCL;

implementation

uses
  Horse;

{$R *.dfm}

{ TfrmVCL }

procedure TfrmVCL.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (FRunning) then
    Stop;
end;

procedure TfrmVCL.Start;
begin
  THorse.Get('ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
  FRunning := True;
end;

procedure TfrmVCL.Stop;
begin
  THorse.StopListen;
  FRunning := False;
end;

procedure TfrmVCL.btnStartStopClick(Sender: TObject);
begin
  FRunning := (not FRunning);

  if (FRunning) then
  begin
    Start;
    btnStartStop.Caption := 'Stop';
    lbStatus.Caption := 'Status: Online';
    lbPorta.Caption := 'Port: ' + IntToStr(THorse.Port);
  end
  else
  begin
    Stop;
    btnStartStop.Caption := 'Start';
    lbStatus.Caption := 'Status: Offline';
    lbPorta.Caption := 'Port: ';
  end;
end;

end.
