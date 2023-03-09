program daemon;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

procedure OnListen;
begin
  if THorse.IsRunning then
    Writeln(Format('Server is runing on %s:%d', [THorse.Host, THorse.Port]));
  if not THorse.IsRunning then
    Writeln('Server stopped');
end;

var
  sCMD: string;
  bTerminated: Boolean;
begin
  // Need to set "HORSE_DAEMON" compilation directive
  THorse.Get('/ping', @GetPing);
  bTerminated := False;
  WriteLn('COMMANDS: START, STOP, TERMINATE');
  while not bTerminated do
  begin
    ReadLn(sCMD);
    case sCMD.ToUpper() of
      'START': THorse.Listen(9000, @OnListen);
      'STOP': THorse.StopListen;
      'TERMINATE' : bTerminated := True;
    end;
  end;
end.
