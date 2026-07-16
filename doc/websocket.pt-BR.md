# WebSockets no Horse

O Horse possui suporte nativo e de alto desempenho a conexões WebSocket bidirecionais (em total conformidade com a especificação RFC 6455). A funcionalidade é integrada diretamente no core do framework, operando sem dependências de pacotes externos, fornecendo uma API fluida para upgrade de conexões, gerenciamento thread-safe de sessões e broadcasts.

---

## 🚀 Como Funciona

Ao contrário de abordagens tradicionais que exigem servidores separados ou portas adicionais, o suporte a WebSocket do Horse adota o padrão de **Porta Única (Port Sharing)**. O handshake de upgrade do protocolo ocorre no mesmo socket TCP e porta HTTP principal da aplicação, garantindo facilidade no gerenciamento de firewalls e balanceadores de carga.

### Provedores Compatíveis
O suporte a WebSockets está disponível para todos os provedores baseados em sockets físicos controlados pela aplicação:
* **Indy** (Console, VCL, Daemon)
* **IOCP** (Windows)
* **Epoll** (Linux)
* **FPC** (`fphttpserver`)

### Restrições e Comportamento Fail-Fast
Alguns provedores, por sua natureza arquitetural, **não** suportam WebSocket direto:
* **HttpSys**: O driver de modo kernel (`http.sys`) do Windows impede o sequestro do socket bruto, exigindo APIs específicas do SO.
* **Apache / ISAPI / CGI / FastCGI**: Servidores web externos gerenciam o ciclo de vida das conexões, não permitindo manter sockets abertos indefinidamente na thread de execução do Horse.

Para estes casos, o Horse implementa uma política de **Fail-Fast**. Caso um cliente tente realizar o handshake WebSocket nesses ambientes, a API detecta a ausência do mecanismo de upgrade no provedor, define o status HTTP `501 Not Implemented` ou `400 Bad Request` de forma transparente no cliente, e interrompe o pipeline de execução imediatamente através da exceção `EHorseCallbackInterrupted`, evitando falhas silenciosas ou vazamento de recursos.

---

## 🛠️ API e Uso Básico

A ativação do WebSocket é feita chamando o método `UpgradeToWebSocket` no objeto `THorseResponse`.

```delphi
uses
  Horse,
  Horse.Core.WebSocket;

begin
  THorse.Get('/ws',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.UpgradeToWebSocket(
        procedure(const AConn: IHorseWebSocketConnection)
        begin
          // Evento de Conexão Estabelecida
          Writeln('Conectado: ', AConn.ClientIP);

          // Evento de Recebimento de Mensagem
          AConn.OnMessage :=
            procedure(const AConnection: IHorseWebSocketConnection; const AText: string)
            begin
              Writeln('Mensagem: ', AText);
              // Envia de volta para o mesmo cliente (Echo)
              AConnection.SendText('Eco: ' + AText);
            end;

          // Evento de Desconexão
          AConn.OnDisconnect :=
            procedure(const AConnection: IHorseWebSocketConnection)
            begin
              Writeln('Desconectado: ', AConnection.ClientIP);
            end;

          // Evento de Erro
          AConn.OnError :=
            procedure(const AConnection: IHorseWebSocketConnection; const AException: Exception)
            begin
              Writeln('Erro: ', AException.Message);
            end;
        end);
    end);

  THorse.Listen(9000);
end.
```

---

## 📡 Gerenciamento de Conexões e Broadcasts

O Horse possui a classe global thread-safe `THorseWebSocketManager` que gerencia automaticamente as conexões ativas associadas a cada rota.

### Broadcast Simples
Para enviar uma mensagem a todos os clientes conectados em uma rota específica:

```delphi
// Envia para todos na rota /ws
THorseWebSocketManager.Broadcast('/ws', 'Olá a todos!');
```

### Broadcast Excluindo o Remetente
É muito útil em sistemas de chat não ecoar a mensagem de volta para o cliente que a originou:

```delphi
AConn.OnMessage :=
  procedure(const AConnection: IHorseWebSocketConnection; const AText: string)
  begin
    // Envia a mensagem a todos no canal, exceto para o remetente (AConnection)
    THorseWebSocketManager.Broadcast('/ws', AText, AConnection);
  end;
```
---

## 🎯 Unicast: Enviando para um Cliente Específico (Mensagem Direta)

O Horse permite enviar mensagens para clientes individuais de duas formas principais:

### 1. Resposta Direta no Evento `OnMessage`
Ao receber uma mensagem, o callback do evento `OnMessage` fornece a referência direta da conexão ativa que disparou o evento (`AConnection: IHorseWebSocketConnection`). Para responder apenas a esse cliente, basta usar os métodos `SendText` ou `SendBinary` dessa instância:

```delphi
AConn.OnMessage :=
  procedure(const AConnection: IHorseWebSocketConnection; const AText: string)
  begin
    // Responde exclusivamente para o cliente emissor
    AConnection.SendText('Recebido: ' + AText);
  end;
```

### 2. Notificação Direcionada a partir de Eventos Externos (Workaround de Sessão)
Quando é necessário enviar mensagens para um cliente específico a partir de outros contextos da aplicação (como em um endpoint REST `/notificar` ou em uma thread de background), deve-se implementar um gerenciador de conexões thread-safe.

Você pode criar uma classe gerenciadora utilizando um dicionário sincronizado por uma seção crítica:

```delphi
unit MyWSManager;

interface

uses
  System.SysUtils, System.Generics.Collections, System.SyncObjs,
  Horse.Core.WebSocket;

type
  TMyWSManager = class
  private
    class var FSync: TCriticalSection;
    class var FClients: TDictionary<string, IHorseWebSocketConnection>;
    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterClient(const AClientId: string; const AConn: IHorseWebSocketConnection);
    class procedure UnregisterClient(const AClientId: string);
    class function SendToClient(const AClientId: string; const AMessage: string): Boolean;
  end;

implementation

class constructor TMyWSManager.Create;
begin
  FSync := TCriticalSection.Create;
  FClients := TDictionary<string, IHorseWebSocketConnection>.Create;
end;

class destructor TMyWSManager.Destroy;
begin
  FClients.Free;
  FSync.Free;
end;

class procedure TMyWSManager.RegisterClient(const AClientId: string; const AConn: IHorseWebSocketConnection);
begin
  FSync.Enter;
  try
    FClients.AddOrSetValue(AClientId, AConn);
  finally
    FSync.Leave;
  end;
end;

class procedure TMyWSManager.UnregisterClient(const AClientId: string);
begin
  FSync.Enter;
  try
    FClients.Remove(AClientId);
  finally
    FSync.Leave;
  end;
end;

class function TMyWSManager.SendToClient(const AClientId: string; const AMessage: string): Boolean;
var
  LConn: IHorseWebSocketConnection;
begin
  Result := False;
  FSync.Enter;
  try
    if FClients.TryGetValue(AClientId, LConn) then
    begin
      if LConn.IsConnected then
      begin
        LConn.SendText(AMessage);
        Result := True;
      end;
    end;
  finally
    FSync.Leave;
  end;
end;

end.
```

E na definição das rotas do seu servidor Horse:

```delphi
uses Horse, Horse.Core.WebSocket, MyWSManager;

begin
  THorse.Get('/ws',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LClientId: string;
    begin
      LClientId := Req.Query.Items['clientId']; // Ex: /ws?clientId=ClienteX
      
      if LClientId.IsEmpty then
      begin
        Res.Send('clientId is required').Status(400);
        Exit;
      end;

      Res.UpgradeToWebSocket(
        procedure(const AConn: IHorseWebSocketConnection)
        begin
          // Registra o cliente e sua conexão WebSocket correspondente
          TMyWSManager.RegisterClient(LClientId, AConn);

          AConn.OnDisconnect :=
            procedure(const AConnection: IHorseWebSocketConnection)
            begin
              // Remove o cliente da lista ao desconectar
              TMyWSManager.UnregisterClient(LClientId);
            end;
        end);
    end);
```

---

## 💓 Heartbeat e Keep-Alive

Para evitar que conexões inativas sejam encerradas prematuramente por proxies, firewalls ou balanceadores de carga, o Horse possui um mecanismo nativo de **Heartbeat** periódico.

Por padrão, ao fazer o upgrade da conexão, o Horse inicia uma thread de monitoramento em segundo plano que envia frames `Ping` (Opcode `$09`) ao cliente em um intervalo determinado (o padrão é 30 segundos). Se o cliente não responder com um frame `Pong` ou se a conexão cair, a conexão é encerrada localmente liberando a memória imediatamente de forma segura.

O desenvolvedor pode customizar o intervalo de Heartbeat (em segundos) passando o parâmetro correspondente no Upgrader se necessário.

---

## 🔒 Thread-Safety e Boas Práticas

1. **Exclusão Mútua de Escrita**: O método `SendText` / `SendBinary` é thread-safe. A escrita no socket físico é protegida internamente por um `TCriticalSection` (`FSyncWrite`), permitindo que múltiplas threads chamem `Send` concorrentemente sem risco de corrupção ou interleaving de bytes no socket.
2. **Ciclo de Vida do Request**: Não tente reter variáveis de `THorseRequest` ou `THorseResponse` dentro das closures do WebSocket. O ciclo de vida da requisição HTTP é encerrado no momento em que `UpgradeToWebSocket` faz o upgrade da conexão. A partir desse momento, utilize apenas a interface `IHorseWebSocketConnection`.
3. **Exceções em Callbacks**: Sempre trate exceções dentro dos eventos do WebSocket (`OnMessage`, etc.). Falhas não tratadas dentro desses callbacks podem forçar o encerramento abrupto do socket do cliente correspondente.
