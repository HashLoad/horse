program Console;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}
{$IFDEF MSWINDOWS}
{$R *.res}
{$ENDIF}

uses
  {$IFDEF UNIX}
    cthreads,
  {$ENDIF}
  Horse,
  {$IFDEF FPC}
    SysUtils;
  {$ELSE}
    System.SysUtils;
  {$ENDIF}

procedure DoPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Ping');
end;

procedure DoListen;
begin
  Writeln(Format('Server is running on port %d...', [THorse.Port]));
end;

begin
  {$IFDEF MSWINDOWS}
    IsConsole := True;
    ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  THorse.ListenQueue := 4096;

  THorse.Get('/ping', DoPing);

  THorse.Listen(9000, DoListen);

  while THorse.IsRunning do
    Sleep(1000);
end.
