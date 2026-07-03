program HorseBench;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cmem,
  cthreads,
  {$ENDIF}
  SysUtils,
  Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  try
    THorse.UseRadixRouter;
    THorse.Get('/ping', GetPing);
    Writeln('Servidor FPC/Horse (epoll) rodando na porta 9090...');
    THorse.Listen(9090);

    // Mantém o contêiner ativo impedindo que o programa console finalize,
    // já que o listener epoll roda de forma assíncrona em background.
    while True do
      Sleep(1000);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
