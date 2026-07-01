program HorseBench;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse,
  Horse.Core.Router.Radix;

begin
  try
    if (ParamCount > 0) and SameText(ParamStr(1), '--radix') then
      THorse.Routes := THorseRadixRouter.Create;

    THorse.Get('/ping',
      procedure(Req: THorseRequest; Res: THorseResponse)
      begin
        Res.Send('pong');
      end);

    Writeln('Servidor Horse (Delphi Indy Linux) rodando na porta 9090...');
    THorse.Listen(9090);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
