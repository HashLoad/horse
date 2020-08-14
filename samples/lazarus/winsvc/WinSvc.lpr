Program WinSvc;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, DaemonManager, DaemonMain
  { add your units here };

begin
  Application.Title := 'Daemon application';
  Application.Initialize;
  Application.Run;
end.
