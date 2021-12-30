program Console;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Horse, System.SysUtils;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000,
    procedure(Horse: THorse)
    begin
      Writeln(Format('Server is runing on %s:%d', [Horse.Host, Horse.Port]));
      {$IFDEF DEBUG}
      ReportMemoryLeaksOnShutdown := True;
      System.IsConsole := False;
      while THorse.IsRunning do
      begin
        Writeln('Type "stop" to terminate the server');
        Readln(LReadLn);
        if LReadLn = 'stop' then
          THorse.StopListen;
      end;
      {$ENDIF}
    end);
end.
