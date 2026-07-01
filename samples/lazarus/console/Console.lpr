program Console;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Ping');
end;

procedure QueryPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Query: ' + Req.Body);
end;

begin
  THorse.Get('/ping', GetPing);
  THorse.Query('/ping', QueryPing);
  THorse.Listen(9000);
end.
