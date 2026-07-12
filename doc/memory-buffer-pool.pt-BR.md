# Pool de Buffers de Memória (Memory Buffer Pooling)

O Pool de Buffers de Memória no Horse é uma otimização de alta performance projetada para minimizar o overhead de alocação de memória (heap churn) e a contenção de locks no Gerenciador de Memória sob cargas pesadas de requisições concorrentes.

## 💡 O Problema

Normalmente, frameworks web alocam novas estruturas de memória para cada payload de requisição e resposta HTTP (usando `TMemoryStream` ou `TBytesStream`). Em aplicações web altamente concorrentes em Delphi/FPC, a alocação e liberação contínua (`GetMem`/`FreeMem`) leva a:
1. **Fragmentação de Heap:** Divisão da memória física em blocos muito pequenos.
2. **Contenção de Lock:** Servidores multithread travando o mutex global do gerenciador de memória do sistema operacional ao alocar memória, causando pequenos travamentos (stalls) e reduzindo a eficiência da CPU.

## 🚀 A Solução

O Horse implementa um pool de buffers thread-safe baseado em pilha (`THorseMemoryBufferPool`) e uma implementação de stream reciclável (`THorsePooledStream` herdando de `TStream`).

Em vez de alocar nova memória:
1. **Request Body:** Provedores de alta performance (como HttpSys e Epoll) adquirem um buffer de stream pré-alocado do pool para ler e processar os dados vindos do socket.
2. **Response Body:** Handlers enviando bytes ou respostas vazias adquirem streams do pool.
3. **Reciclagem Automática:** Quando o processamento termina e o método `.Free` é acionado no stream, o buffer de `TBytes` subjacente é automaticamente devolvido ao pool global para a próxima requisição.

Isso resulta em um **mapeamento zero-allocation de payloads de requisição e resposta** para a vasta maioria das operações HTTP.

## 🛠️ Uso Transparente

Para desenvolvedores que apenas utilizam o framework, esta otimização é **100% transparente**. Você não precisa reescrever nenhuma rota ou middleware para usufruir dos ganhos. O código típico permanece exatamente igual:

```delphi
THorse.Get('/ping',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    Res.Send('pong'); // Utiliza o Pool de Buffers de forma automática por baixo dos panos
  end);
```

## 🔌 Uso Avançado (para Middlewares e I/O Customizado)

Se você está desenvolvendo middlewares customizados ou rotas que necessitam de streaming pesado ou manipulação manual de arquivos, você pode voluntariamente aproveitar o pool de buffers para evitar alocações de heap:

```delphi
uses
  Horse,
  Horse.Core.MemoryBufferPool,
  System.Classes;

begin
  THorse.Get('/relatorio',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LStream: TStream;
    begin
      // Adquire um stream pré-alocado do pool global de forma thread-safe
      LStream := THorseMemoryBufferPool.DefaultPool.AcquireStream;
      try
        // Grava o conteúdo no stream reciclado
        LStream.WriteBuffer(PChar('Dados do Relatório...')^, 21);
        
        // Retorna o stream. O Horse enviará os dados e chamará LStream.Free,
        // devolvendo o buffer de bytes automaticamente para o pool.
        Res.SendFile(LStream, 'relatorio.txt');
      except
        LStream.Free; // Garante a devolução ao pool em caso de exceção
        raise;
      end;
    end);

  THorse.Listen(9000);
end.
```

## ⚙️ Configurações Internas do Pool

Por padrão, o pool global inicializa com as seguintes diretrizes:
* **Tamanho Padrão do Buffer:** `65.536 bytes (64 KB)`.
* **Capacidade Máxima do Pool:** `1.024` buffers inativos mantidos em RAM.
* **Tamanho Máximo Reciclável:** `2.097.152 bytes (2 MB)`. 

> [!NOTE]
> Se um stream crescer dinamicamente além de `2 MB` (ex: processando um upload muito grande), ele é descartado no momento da destruição e um novo buffer padrão de `64 KB` é reinserido no pool para prevenir vazamento e manter o consumo de RAM estável.
