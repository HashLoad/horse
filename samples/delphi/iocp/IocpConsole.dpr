program IocpConsole;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}

// Habilita o novo provider IOCP nativo de Windows
{$DEFINE HORSE_PROVIDER_IOCP}

uses
  Horse,
  {$IFDEF FPC}
    SysUtils;
  {$ELSE}
    System.SysUtils;
  {$ENDIF}

procedure DoPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

procedure DoListen;
begin
  Writeln('--------------------------------------------------');
  Writeln(' Servidor Horse IOCP Iniciado (Windows)');
  Writeln(Format(' Escutando em: http://%s:%d/', [THorse.Host, THorse.Port]));
  Writeln(' Roteador: ' + {$IFDEF HORSE_RADIX_ROUTER}'Radix'{$ELSE}'Classic'{$ENDIF});
  Writeln('--------------------------------------------------');
  Writeln(' Pressione Ctrl+C para encerrar.');
end;

begin
  ReportMemoryLeaksOnShutdown := True;

  {$IFDEF HORSE_RADIX_ROUTER}
  THorse.UseRadixRouter;
  {$ENDIF}

  THorse.Get('/ping', DoPing);

  THorse.Listen(9000, '0.0.0.0', DoListen);
end.
