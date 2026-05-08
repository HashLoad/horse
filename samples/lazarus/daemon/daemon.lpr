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
  WriteLn(Format('Server is running on %s:%d', [THorse.Host, THorse.Port]));
end;

var
  sCMD: string;
  bTerminated: Boolean;
begin
  THorse.Get('/ping', @GetPing);

  bTerminated := False;
  WriteLn('COMMANDS: START, STOP, TERMINATE');

  while not bTerminated do
  begin
    ReadLn(sCMD);
    sCMD := UpperCase(Trim(sCMD));

    case sCMD of
      'START':
        THorse.Listen(9000, @OnListen);
      'STOP':
        THorse.StopListen;
      'TERMINATE':
        begin
          THorse.StopListen;
          bTerminated := True;
        end;
    end;
  end;
end.
