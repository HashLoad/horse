unit TesteHorse.Servico;

interface

uses
  Horse, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs;

type
  TService1 = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceShutdown(Sender: TService);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  Service1: TService1;
  App: THorse;

implementation

{$R *.dfm}


procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  Service1.Controller(CtrlCode);
end;

function TService1.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TService1.ServiceCreate(Sender: TObject);
begin
  App := THorse.Create(9000);

  App.Get('ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

end;

procedure TService1.ServiceDestroy(Sender: TObject);
begin
   App.Destroy;
end;

procedure TService1.ServiceShutdown(Sender: TService);
begin
   App.Destroy;
end;

procedure TService1.ServiceStart(Sender: TService; var Started: Boolean);
begin
  App.Start;
  Started := True;
end;

end.


