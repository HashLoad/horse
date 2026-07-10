program HorseBench;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse
  {$IF DEFINED(HORSE_PROVIDER_IOCP)}
  , Horse.Provider.IOCP
  {$ELSEIF DEFINED(HORSE_PROVIDER_HTTPSYS)}
  , Horse.Provider.HttpSys
  {$ENDIF}
  ;

begin
  try
    if FindCmdLineSwitch('radix') then
      THorse.UseRadixRouter;

    THorse.Get('/ping',
      procedure(Req: THorseRequest; Res: THorseResponse)
      begin
        Res.Send('pong');
      end);

    // Inicia o servidor na porta 9090 no localhost 127.0.0.1
    THorse.Listen(9090, '127.0.0.1');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
