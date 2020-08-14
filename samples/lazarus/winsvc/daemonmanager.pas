unit DaemonManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type

  { TDaemon_Manager }

  TDaemon_Manager = class(TDaemonMapper)
    procedure Daemon_ManagerCreate(Sender: TObject);
  private

  public

  end;

var
  Daemon_Manager: TDaemon_Manager;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TDaemon_Manager)
end;

{$R *.lfm}

{ TDaemon_Manager }

procedure TDaemon_Manager.Daemon_ManagerCreate(Sender: TObject);
begin

end;


initialization
  RegisterMapper;
end.

