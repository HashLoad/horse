unit DaemonMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type

  { TDaemon_Main }

  TDaemon_Main = class(TDaemon)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleShutDown(Sender: TCustomDaemon);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
  private

  public
  end;

var
  Daemon_Main: TDaemon_Main;

implementation

uses
  Horse;

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TDaemon_Main)
end;

procedure RunHorse;
begin
  THorse.Listen(9000);
end;

procedure Ping(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('pong');
end;

{$R *.lfm}

{ TDaemon_Main }

procedure TDaemon_Main.DataModuleCreate(Sender: TObject);
begin
  THorse.Get('ping', @Ping);
end;

procedure TDaemon_Main.DataModuleShutDown(Sender: TCustomDaemon);
begin
  THorse.StopListen;
end;

procedure TDaemon_Main.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
begin
  TThread.CreateAnonymousThread(@RunHorse).Start;
end;

initialization
  RegisterDaemon;
end.

