# Streaming de Dados (Web Streams & Server-Sent Events)

O Horse possui suporte nativo a transmissões de dados em tempo real (streaming) através de ganchos integrados aos provedores físicos, permitindo dois formatos principais de entrega incremental:
1.  **Web Streams (Chunked HTTP/1.1)**: Transmissão incremental de dados divididos em pedaços (*chunks*), comumente usado para APIs de IA/NDJSON.
2.  **Server-Sent Events (SSE)**: Canal unidirecional mantido aberto para envio contínuo de eventos estruturados do servidor para o cliente (`text/event-stream`).

---

## 🚀 Como Utilizar no Servidor

A inicialização do streaming é feita através do método `Res.SendStream`, que aceita um callback recebendo uma instância de `IHorseStreamWriter`.

### 1. Exemplo de Web Stream (NDJSON)
Neste exemplo, enviamos múltiplos objetos JSON de forma incremental linha a linha (usando quebra de linha `\n` como delimitador):

```delphi
uses Horse, Horse.Response, System.SysUtils;

begin
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
            // Verifica se o cliente ainda está conectado antes de escrever
            if not AWriter.IsConnected then Break;
            
            LJSON := Format('{"id": %d, "timestamp": "%s"}'#10, [I, FormatDateTime('hh:nn:ss', Now)]);
            AWriter.Write(LJSON);
            Sleep(500); // Simula processamento assíncrono
          end;
        end);
    end);

  THorse.Listen(9000);
end.
```

### 2. Exemplo de Server-Sent Events (SSE)
O SSE requer cabeçalhos específicos (`text/event-stream`, `Cache-Control: no-cache` e `Connection: keep-alive`) e o corpo dos eventos deve seguir o padrão `event: <nome>\ndata: <conteúdo>\n\n`:

```delphi
uses Horse, Horse.Response, System.SysUtils;

begin
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

  THorse.Listen(9000);
end.
```

---

## 📡 Como Consumir o Stream (Cliente Nativo Delphi)

Você pode consumir qualquer um dos endpoints acima de forma incremental utilizando o `THTTPClient` moderno do Delphi herdando de `TStream` para interceptar os dados:

```delphi
uses System.SysUtils, System.Classes, System.Net.HttpClient;

type
  TStreamingReceiveStream = class(TStream)
  private
    FOnChunkReceived: TProc<string>;
  public
    constructor Create(const AOnChunkReceived: TProc<string>);
    function Write(const Buffer; Count: Longint): Longint; override;
    // Métodos Read, Seek e SetSize devem ser sobrescritos apenas retornando 0 / vazios
  end;

constructor TStreamingReceiveStream.Create(const AOnChunkReceived: TProc<string>);
begin
  inherited Create;
  FOnChunkReceived := AOnChunkReceived;
end;

function TStreamingReceiveStream.Write(const Buffer; Count: Longint): Longint;
var
  LText: string;
  LBytes: TBytes;
begin
  Result := Count;
  if Count <= 0 then Exit;
  SetLength(LBytes, Count);
  Move(Buffer, LBytes[0], Count);
  LText := TEncoding.UTF8.GetString(LBytes);
  if Assigned(FOnChunkReceived) then
    FOnChunkReceived(LText);
end;

// Exemplo de consumo:
var
  LClient: THTTPClient;
  LStream: TStreamingReceiveStream;
begin
  LClient := THTTPClient.Create;
  LStream := TStreamingReceiveStream.Create(
    procedure(AChunk: string)
    begin
      Write(AChunk); // Imprime na tela o chunk recebido em tempo real
    end);
  try
    LClient.Get('http://localhost:9000/ndjson', LStream);
  finally
    LStream.Free;
    LClient.Free;
  end;
end.
```

---

## ⚠️ Diretrizes e Boas Práticas

*   **Evite Anonymous Methods Inline no Lazarus/FPC**: Devido a restrições do compilador FPC, prefira declarar procedimentos tradicionais ou métodos de classe/objeto para os callbacks das rotas e streams no Lazarus.
*   **Detecção de Desconexão**: Sempre verifique `AWriter.IsConnected` antes de cada escrita para liberar recursos imediatamente se o usuário fechar a aba ou interromper o download.
*   **Bypass de Middlewares**: Qualquer middleware de compressão global (gzip/deflate) ou de logging concorrente que manipule buffers de resposta assíncronos é automaticamente ignorado quando `Res.IsStreaming` for `True` para evitar retenção de dados no buffer e corrupção na rede.
