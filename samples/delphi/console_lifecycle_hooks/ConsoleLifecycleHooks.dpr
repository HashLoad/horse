program ConsoleLifecycleHooks;

{$APPTYPE CONSOLE}

uses
  Horse,
  System.SysUtils;

begin
  Writeln('=== Horse Lifecycle Hooks Audit Sample ===');

  // === Ganchos de Ciclo de Vida do Servidor (Server Lifecycle Hooks) ===

  THorse.AddBeforeListen(
    procedure(APort: Integer)
    begin
      Writeln('[Server Hook] 1. BeforeListen: O servidor sera iniciado na porta ' + APort.ToString);
    end);

  THorse.AddAfterListen(
    procedure(APort: Integer)
    begin
      Writeln('[Server Hook] 2. AfterListen: O servidor foi iniciado fisicamente e esta escutando.');
    end);

  THorse.AddBeforeStop(
    procedure(APort: Integer)
    begin
      Writeln('[Server Hook] 3. BeforeStop: O encerramento do servidor foi solicitado para a porta ' + APort.ToString);
    end);

  THorse.AddAfterStop(
    procedure(APort: Integer)
    begin
      Writeln('[Server Hook] 4. AfterStop: O servidor foi desligado fisicamente e a porta liberada.');
    end);

  // === Ganchos de Ciclo de Vida da Requisição (Request Lifecycle Hooks) ===

  // 1. onRequest
  THorse.AddOnRequest(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('[Request Hook] 1. onRequest executado para: ' + Req.PathInfo);
      Next;
    end);

  // 2. preParsing
  THorse.AddPreParsing(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('[Request Hook] 2. preParsing executado.');
      Next;
    end);

  // 3. preValidation
  THorse.AddPreValidation(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('[Request Hook] 3. preValidation executado para a rota ativa.');
      Next;
    end);

  // 4. onSend (String)
  THorse.AddOnSend(
    procedure(const Req: THorseRequest; const Res: THorseResponse; var AContent: string)
    begin
      Writeln('[Request Hook] 4. onSend (String) interceptado. Payload original: ' + AContent);
      AContent := AContent + ' (Assinado pelo onSend)';
    end);

  // 5. onResponse
  THorse.AddOnResponse(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('[Request Hook] 5. onResponse finalizado com status: ' + Res.Status.ToString);
      Writeln('--------------------------------------------------');
      Next;
    end);

  // Rota de teste
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000,
    procedure
    begin
      Writeln('Servidor ativo na porta: ' + THorse.Port.ToString);
      Writeln('Envie uma requisicao para http://localhost:9000/ping para auditar o ciclo de vida.');
      Writeln('Pressione enter para encerrar...');
    end);
  Readln;
end.
