unit WinSvc.Service;

interface

uses Horse, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs;

type
  TWinService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceShutdown(Sender: TService);
  public
    function GetServiceController: TServiceController; override;
  end;

var
  WinService: TWinService;
  App: THorse;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  WinService.Controller(CtrlCode);
end;

function TWinService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TWinService.ServiceCreate(Sender: TObject);
begin
  App := THorse.Create(9000);

  App.Get('ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);
end;

procedure TWinService.ServiceDestroy(Sender: TObject);
begin
  App.Destroy;
end;

procedure TWinService.ServiceShutdown(Sender: TService);
begin
  App.Destroy;
end;

procedure TWinService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  App.Start;
  Started := True;
end;

end.
