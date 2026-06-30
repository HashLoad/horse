program HorseBench;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse,
  Horse.Provider.HttpSys;

begin
  try
    THorse.Get('/ping',
      procedure(Req: THorseRequest; Res: THorseResponse)
      begin
        Res.Send('pong');
      end);

    Writeln('Servidor Horse (HTTP.sys) rodando na porta 9090...');
    THorse.Listen(9090, '127.0.0.1');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
