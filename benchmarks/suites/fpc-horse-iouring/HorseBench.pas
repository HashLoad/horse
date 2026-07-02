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

procedure EmptyCallback;
begin
  // Apenas callback vazio para compatibilidade com o Listen do io_uring no FPC
end;

begin
  try
    THorse.UseRadixRouter;
    THorse.Get('/ping', GetPing);
    Writeln('Servidor FPC/Horse (io_uring) rodando na porta 9090...');
    THorse.Listen(9090, EmptyCallback, EmptyCallback);

    while True do
      Sleep(1000);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
