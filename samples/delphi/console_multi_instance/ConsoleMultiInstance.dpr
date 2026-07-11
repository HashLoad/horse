program ConsoleMultiInstance;

{$IFNDEF MSWINDOWS}
  {$ERROR Este exemplo usa recursos especificos do Windows. Para Linux/FPC use o exemplo do Lazarus.}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  Horse;

var
  FInstance1: THorseInstance;
  FInstance2: THorseInstance;

procedure DoListen1;
begin
  Writeln('Instancia 1 (Porta 9001): Pronta para receber conexoes em http://localhost:9001/api/v1/ping');
end;

procedure DoListen2;
begin
  Writeln('Instancia 2 (Porta 9002): Pronta para receber conexoes em http://localhost:9002/admin/ping');
end;

begin
  // NOTA DIDÁTICA SOBRE CONSOLE / THREADS:
  // O método Listen() do Horse bloqueia a thread chamadora apenas se 'IsConsole = True' 
  // (mecanismo usado para evitar que programas de console finalizem imediatamente).
  //
  // Para rodar múltiplos servidores paralelos sem criar threads manuais (TThread):
  // Definimos 'IsConsole := False'. Isso faz com que os métodos Listen() ativem os sockets
  // em background (usando as threads de socket nativas dos provedores) e retornem a execução
  // da Thread Principal imediatamente, permitindo que controlemos a parada com um Readln.
  // 
  // Em aplicações GUI (VCL/LCL) ou Serviços (Daemons), 'IsConsole' é False por padrão,
  // portanto o Listen() nunca bloqueia e threads manuais não são necessárias de forma alguma.
  IsConsole := False;
  ReportMemoryLeaksOnShutdown := True;

  Writeln('====================================================');
  Writeln('      EXEMPLO HORSE MULTI-INSTANCE (DELPHI)         ');
  Writeln('====================================================');

  // 1. Configurando a Instancia 1 (Ex: API Publica na porta 9001)
  FInstance1 := THorseInstance.Create;
  FInstance1.Get('/api/v1/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('Pong da API Publica (Instancia 1)');
    end);

  // 2. Configurando a Instancia 2 (Ex: Area de Admin/Metrics na porta 9002)
  FInstance2 := THorseInstance.Create;
  FInstance2.Get('/admin/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('Pong da Area Admin (Instancia 2)');
    end);

  // 3. Inicializando as escutas físicas (ambas ativam e retornam instantaneamente)
  FInstance1.Listen(9001, '127.0.0.1', DoListen1);
  FInstance2.Listen(9002, '127.0.0.1', DoListen2);

  Writeln('Servidores inicializados.');
  
  // Detecção de switch para testes automatizados ponta a ponta em background
  if (ParamCount > 0) and ((ParamStr(1) = '--delay') or (ParamStr(1) = '-delay')) then
  begin
    Writeln('Execucao em modo automatizado detectada. Aguardando 10 segundos antes de encerrar...');
    Sleep(10000);
  end
  else
  begin
    Writeln('Pressione [ENTER] para interromper a execucao e sair...');
    Readln;
  end;

  Writeln('Interrompendo a escuta das portas...');
  FInstance1.StopListen;
  FInstance2.StopListen;

  Writeln('Liberando recursos das instancias...');
  FInstance1.Free;
  FInstance2.Free;

  Writeln('Finalizado com sucesso.');
end.
