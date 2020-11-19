program daemon;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, Horse
  { you can add units after this };
  { add your program here }

procedure GetPing(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('pong');
end;

procedure OnListen(Horse: THorse);
begin
  if THorse.IsRunning then
    Writeln(Format('Server is runing on %s:%d', [Horse.Host, Horse.Port]));
  if not THorse.IsRunning then
    Writeln('Server stopped');
end;

var
  sCMD : String;
  bTerminated : Boolean;
begin
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

