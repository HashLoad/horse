program Console;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.AddHeader('Teste', 'Alexandre').Send('Ping');
end;

begin
  THorse.Get('/ping', GetPing);
  THorse.Listen(9000);
end.
