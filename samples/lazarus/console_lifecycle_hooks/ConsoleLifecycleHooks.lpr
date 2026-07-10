program ConsoleLifecycleHooks;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Horse,
  SysUtils;

begin
  Writeln('=== Horse Lifecycle Hooks Audit Sample ===');

  // 1. onRequest
  THorse.AddOnRequest(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('[Hook] 1. onRequest executado para: ' + Req.PathInfo);
      Next;
    end);

  // 2. preParsing
  THorse.AddPreParsing(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('[Hook] 2. preParsing executado.');
      Next;
    end);

  // 3. preValidation
  THorse.AddPreValidation(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('[Hook] 3. preValidation executado para a rota ativa.');
      Next;
    end);

  // 4. onSend (String)
  THorse.AddOnSend(
    procedure(const Req: THorseRequest; const Res: THorseResponse; var AContent: string)
    begin
      Writeln('[Hook] 4. onSend (String) interceptado. Payload original: ' + AContent);
      AContent := AContent + ' (Assinado pelo onSend)';
    end);

  // 5. onResponse
  THorse.AddOnResponse(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('[Hook] 5. onResponse finalizado com status: ' + IntToStr(Res.Status));
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
      Writeln('Servidor ativo na porta: ' + IntToStr(THorse.Port));
      Writeln('Envie uma requisicao para http://localhost:9000/ping para auditar o ciclo de vida.');
      Writeln('Pressione enter para encerrar...');
    end);
  Readln;
end.
