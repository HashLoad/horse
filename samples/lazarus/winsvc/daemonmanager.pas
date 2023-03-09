unit DaemonManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type
  TDaemon_Manager = class(TDaemonMapper)
  end;

var
  Daemon_Manager: TDaemon_Manager;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TDaemon_Manager)
end;

{$R *.lfm}

initialization
  RegisterMapper;

end.
