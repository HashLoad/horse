---
name: horse-streaming
description: Diretrizes e workflows para desenvolvimento e manutenção de transmissões de dados em tempo real (Web Streams e Server-Sent Events - SSE) no framework Horse.
---

# Horse Streaming Development Guide

Esta skill guia o desenvolvimento, depuração e manutenção de fluxos de streaming de dados (Web Streams / SSE) no framework Horse.

## 🟢 Conceitos Fundamentais
*   **Web Streams (NDJSON)**: Envio de dados formatados como múltiplos JSONs delimitados por quebras de linha (`\n`), utilizando o cabeçalho `Transfer-Encoding: chunked`.
*   **Server-Sent Events (SSE)**: Fluxo unidirecional persistente onde o servidor envia eventos estruturados sob o formato `event: message\ndata: ...\n\n` com o Content-Type `text/event-stream`.

## 🟢 Como usar nas Rotas do Horse
Para enviar dados via streaming, o manipulador da rota deve definir o `ContentType` adequado e invocar o `Res.SendStream`:

```delphi
  THorse.Get('/stream',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.ContentType('application/x-ndjson; charset=utf-8');
      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        begin
          if AWriter.IsConnected then
            AWriter.Write('{"data": "teste"}'#10);
        end);
    end);
```

## 🟢 Implementação dos Provedores Físicos
Ao criar ou estender novos provedores lógicos de transporte no Horse:
1.  Deve-se herdar uma classe especializada de `THorseStreamWriterBase` e implementar os métodos `Write` (escrita física), `IsConnected` (validação física de conectividade do socket) e `Close` (fim de transmissão/descarregamento de buffers).
2.  Registrar a fábrica através de `THorseResponse.RegisterStreamWriterFactory(Factory)`.

## 🟢 Peculiaridades por Provedor

### Indy (`TIdHTTPAppResponse`)
*   **Conexão**: A thread física é acessada via o campo `FThread` da Friend Class.
*   **Envio de Headers**: Invocar `MoveCookiesAndCustomHeaders` do `TIdHTTPAppResponse` para carregar headers customizados e cookies antes do `WriteHeader`.
*   **Flag de Envio**: Marcar `FSent := True` no `TIdHTTPAppResponse` para impedir que o WebBroker dispare novamente os headers no fim do dispatcher.
*   **Bypass de HTML do Indy**: Preencher `ContentStream` com um `TMemoryStream` vazio, `FreeContentStream := True` e `ContentLength := 0` para desativar a injeção do HTML default de 200 OK de 39 bytes pelo Indy.
