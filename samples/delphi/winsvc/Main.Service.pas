unit Main.Service;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs;

type
  TMainService = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  MainService: TMainService;

implementation

uses

  Horse;

{$R *.dfm}


procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  MainService.Controller(CtrlCode);
end;

function TMainService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TMainService.ServiceCreate(Sender: TObject);
begin
  THorse.Get('ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);
end;

procedure TMainService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  THorse.Listen(7000);
  Started := true;
end;

procedure TMainService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  THorse.StopListen;
  Stopped := true;
end;

end.
