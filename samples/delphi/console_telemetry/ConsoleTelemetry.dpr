program ConsoleTelemetry;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}

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
  Sleep(150); // Simula processamento
  Res.Send('pong');
end;

procedure DoListen;
begin
  Writeln(Format('Telemetry Demo server is running on http://localhost:%d...', [THorse.Port]));
end;

begin
  {$IFDEF MSWINDOWS}
    IsConsole := True;
    ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  // Registrar gancho nativo de telemetria
  THorse.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      Writeln(Format('[Telemetry Log] %s %s - Status HTTP: %d - Latency: %.2f ms', 
        [Req.Method, Req.PathInfo, Res.Status, ExecutionTimeMS]));
    end);

  THorse.Get('/ping', DoPing);

  THorse.Listen(9000, DoListen);

  while THorse.IsRunning do
    Sleep(500);
end.
