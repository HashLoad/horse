program ConsoleGracefulShutdown;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Horse, SysUtils, Classes, Horse.Commons;

begin
  // Endpoint que simula um processo demorado (ex: geração de um relatório pesado)
  THorse.Get('/slow-process',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln(' -> Iniciando processamento demorado (2 segundos)...');
      TThread.Sleep(2000);
      Res.Send('Processamento demorado concluído com sucesso!');
      Writeln(' -> Processamento demorado finalizado.');
    end);

  // Endpoint de Health Check (Readiness / Liveness Probe)
  THorse.Get('/health',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      if THorse.IsShuttingDown then
      begin
        // Retorna 503 para informar ao orquestrador (Kubernetes) para retirar este nó da rota
        Res.Send('Servidor entrando em desligamento suave.').Status(THTTPStatus.ServiceUnavailable);
      end
      else
      begin
        // Retorna 200 informando o volume de conexões ativas concorrentes
        Res.Send(Format('Servidor Saudável. Conexões ativas: %d', [THorse.ActiveRequests]));
      end;
    end);

  THorse.OnListen :=
    procedure
    begin
      Writeln('Servidor Horse executando com sucesso na porta ', THorse.Port);
      Writeln('Experimente abrir http://localhost:9000/health');
      Writeln('Pressione [ENTER] a qualquer momento para acionar o Graceful Shutdown...');
    end;

  // Inicia a escuta na porta 9000
  THorse.Listen(9000);

  // Aguarda o ENTER no console
  Readln;

  // Aciona o desligamento suave dando até 5 segundos para que as conexões ativas terminem
  Writeln('Desligando de forma suave (aguardando requests em andamento por até 5s)...');
  THorse.StopListenGraceful(5000);
  Writeln('Servidor Horse finalizado com sucesso!');
end.
