program ConsoleMultiInstance;

{$IFNDEF MSWINDOWS}
  {$ERROR Este exemplo usa recursos especificos do Windows. Para Linux/FPC use o exemplo do Lazarus.}
{$ENDIF}

{$APPTYPE CONSOLE}
{$R *.res}

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
  IsConsole := True;
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

  // 3. Inicializando os servidores em Threads separadas
  TThread.CreateAnonymousThread(
    procedure
    begin
      FInstance1.Listen(9001, DoListen1);
    end).Start;

  TThread.CreateAnonymousThread(
    procedure
    begin
      FInstance2.Listen(9002, DoListen2);
    end).Start;

  Writeln('Servidores inicializados.');
  Writeln('Pressione [ENTER] para interromper a execucao e sair...');
  Readln;

  Writeln('Interrompendo a escuta das portas...');
  FInstance1.StopListen;
  FInstance2.StopListen;

  Writeln('Liberando recursos das instancias...');
  FInstance1.Free;
  FInstance2.Free;

  Writeln('Finalizado com sucesso.');
end.
