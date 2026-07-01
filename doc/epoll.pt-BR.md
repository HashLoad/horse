# Provider epoll para o Horse

O provedor `epoll` é uma camada de transporte assíncrona e não-bloqueante nativa para Linux, utilizando a API `epoll` do kernel (via `epoll_wait`). Assim como o HTTP.sys, ele é nativo e vem integrado diretamente no core do Horse — nenhum pacote externo é necessário. Após as otimizações da Phase 2, ele conta com Thread-Local Buffer Pools para eliminar disputa por locks globais de memória, parseador de cabeçalhos sem alocação dinâmica de objetos na heap, escrita vetorizada do sistema (`writev`) e transferência de arquivos com cópia zero via kernel (`sendfile`).

Nos Defines de Compilação do seu projeto: `HORSE_PROVIDER_EPOLL`.

---

## ⚙️ Propriedades e Arquitetura

- **Exclusivo para Linux:** Event loop nativo para distribuições Linux (ideal para Ubuntu, Debian, Alpine e containers Docker).
- **Event Loop Assíncrono:** Multiplexa milhares de conexões TCP abertas usando um pool estático e fixo de threads de E/S e workers, superando o gargalo de threads bloqueantes do Indy (*thread-per-connection*).
- **Thread-Local Buffer Pool (`threadvar`):** Cada thread worker recupera buffers de pools locais, eliminando gargalos de concorrência global por alocação de memória na heap.
- **Parseador Zero-Allocation:** Cabeçalhos HTTP são processados e mapeados baseando-se em offsets de bytes sobre o buffer cru do socket, eliminando conversões custosas e instanciação de strings.
- **Escrita Vetorizada (`writev`):** Agrupa os buffers de cabeçalho e corpo da resposta para reduzir chamadas de sistema e alternâncias de contexto (context-switch).
- **Transferência Cópia Zero (`sendfile`):** Envia streams de arquivo (`TFileStream`) diretamente do cache de página do kernel para o socket de rede, eliminando cópias em User-Space.
- **Timeout Ativo de Keep-Alive:** Gerencia conexões inativas com eficiência, liberando sockets ociosos por meio de um timeout automático de 5 segundos.
- **Proteção contra Slowloris:** Monitora e derruba conexões lentas ou com fluxos de requisição incompletos.
- **Auto-Elevação de Sockets:** Eleva automaticamente os limites de File Descriptors (`RLIMIT_NOFILE`) do processo para 65535 durante a inicialização, garantindo suporte a volumes massivos de clientes concorrentes.
- **Desempenho:** Processou até **29.209 req/s** com zero falhas sob estresse de 1.000 conexões Keep-Alive simultâneas em benchmarks comparativos.

---

## 🐧 Configurações de Limites do Linux

Para suportar milhares de conexões concorrentes sob o Linux, certifique-se de que os limites de descritores de arquivo do sistema operacional estejam configurados corretamente.

Embora o provedor epoll tente elevar esses limites em tempo de execução automaticamente, você pode inspecionar e definir os limites manualmente na sessão do shell:

```bash
# Verifica o limite de descritores de arquivo da sessao atual
ulimit -n

# Define temporariamente o limite para 65535 descritores
ulimit -n 65535
```

---

## ⚡ Início Rápido (Quick Start)

1. Adicione `HORSE_PROVIDER_EPOLL` aos Defines Condicionais do seu projeto (**Project Options → Delphi Compiler → Conditional defines**).
2. Escreva o código do seu servidor normalmente:

```delphi
program MyServer;

{$APPTYPE CONSOLE}

uses
  Horse;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  // Inicia o reactor do epoll na porta 9095
  THorse.Listen(9095);
end.
```

O framework resolverá automaticamente a chamada de `THorse.Listen` para o reactor nativo epoll quando executado em ambientes Linux.
