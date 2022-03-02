Program WinSvc;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, DaemonManager, DaemonMain;

begin
  Application.Initialize;
  Application.Run;
end.
