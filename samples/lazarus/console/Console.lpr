program Console;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Horse, SysUtils, DateUtils;

procedure GetPing(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  LDate: TDateTime;
begin
  LDate := Req.Headers.AsISO8601DateTime('data');
  Res.Send(FormatDateTime('yyyy-MM-dd hh:mm:ss', LDate));
end;

procedure OnListen(Horse: THorse);
begin
  Writeln(Format('Server is runing on %s:%d', [Horse.Host, Horse.Port]));
end;

begin
  THorse.Get('/ping', GetPing);

  THorse.Listen(9000, OnListen);
end.
