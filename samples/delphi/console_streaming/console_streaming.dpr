program console_streaming;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  Horse,
  Horse.Response;

begin
  // Rota principal: Serve o index.html
  THorse.Get('/',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.ContentType('text/html; charset=utf-8').SendFile('www/index.html');
    end);

  // Rota NDJSON (Web Streams)
  THorse.Get('/ndjson',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.ContentType('application/x-ndjson; charset=utf-8');
      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        var
          I: Integer;
          LJSON: string;
        begin
          for I := 1 to 10 do
          begin
            if not AWriter.IsConnected then Break;
            LJSON := Format('{"id": %d, "message": "Registro %d de 10", "timestamp": "%s"}'#10,
              [I, I, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
            AWriter.Write(LJSON);
            Sleep(500);
          end;
        end);
    end);

  // Rota SSE (Server-Sent Events)
  THorse.Get('/sse',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.ContentType('text/event-stream; charset=utf-8');
      Res.AddHeader('Cache-Control', 'no-cache');
      Res.AddHeader('Connection', 'keep-alive');

      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        var
          I: Integer;
          LPayload: string;
        begin
          for I := 1 to 10 do
          begin
            if not AWriter.IsConnected then Break;
            LPayload := Format('event: message'#10'data: {"id": %d, "count": %d}'#10#10, [I, I]);
            AWriter.Write(LPayload);
            Sleep(1000);
          end;
        end);
    end);

  // Rota de IA Chat simulado palavra por palavra
  THorse.Get('/ia-chat',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.ContentType('text/event-stream; charset=utf-8');
      Res.AddHeader('Cache-Control', 'no-cache');
      Res.AddHeader('Connection', 'keep-alive');

      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        const
          WORDS: array[0..36] of string = (
            'O', 'framework', 'Horse', 'agora', 'suporta', 'nativamente', 'a',
            'transmissao', 'de', 'dados', 'via', 'Web', 'Streams', 'e', 'Server-Sent',
            'Events', '(SSE).', 'Esta', 'funcionalidade', 'permite', 'construir',
            'APIs', 'modernas', 'de', 'IA', 'Generativa,', 'telemetria', 'em', 'tempo',
            'real', 'e', 'transmissoes', 'de', 'alta', 'performance', 'com', 'facilidade!'
          );
        var
          I: Integer;
        begin
          for I := Low(WORDS) to High(WORDS) do
          begin
            if not AWriter.IsConnected then Break;
            AWriter.Write('data: ' + WORDS[I] + ' '#10#10);
            Sleep(85);
          end;
        end);
    end);

  THorse.Listen(9000,
    procedure
    begin
      Writeln('Servidor executando em http://localhost:9000');
    end);
end.
