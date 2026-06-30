# Relatório de Benchmark - Horse vs Outras Tecnologias

Este relatório apresenta os resultados finais consolidados do benchmark de performance de alta carga, comparando o **Horse** (rodando em Delphi e FPC/Lazarus) contra os principais frameworks web do mercado (**.NET Minimal API**, **Go Fiber**, **Java Spring Boot** e **Node.js Express**).

O cenário avaliou o throughput (RPS) e latência do endpoint `/ping` (plain text) sob concorrências de **128**, **512** e **1024** conexões simultâneas. No Linux (Docker), todos os contêineres foram limitados a **2 CPUs e 512MB RAM** para garantir igualdade de condições.

---

## 🛠️ Metodologia de Teste

Para assegurar a imparcialidade e a precisão científica dos dados coletados, foi adotada a seguinte metodologia sistemática:

### 1. Ambiente e Isolamento de Recursos
* **Host do Delphi**: O servidor Delphi rodou nativamente no sistema operacional Windows Host para avaliar a performance nativa do provider `HTTP.sys`.
* **Hipervisor Docker**: Os servidores Linux (.NET, Java, Go, Node, FPC Default e FPC epoll) rodaram em contêineres Docker independentes orquestrados via `docker-compose`.
* **Restrição de Hardware**: Cada contêiner Linux teve seus recursos limitados rigidamente no Docker Engine para:
  * **Processamento**: Limite estrito de **2.0 CPUs** (`cpus: 2.0`).
  * **Memória RAM**: Limite físico de **512 MB** (`memory: 512m`).
  * **Swap**: Desabilitado para evitar mascaramento de gargalos de memória.

### 2. Protocolo de Teste de Carga
* **Ferramenta de Benchmark**: Foi utilizada a ferramenta **`bombardier`** (escrita em Go, altamente eficiente para geração de concorrência HTTP).
* **Endpoint e Payload**: Rota `/ping` respondendo com o payload `pong` (MIME: `text/plain`). Esta rota simples foca a medição estritamente no throughput do parser HTTP, roteador interno e latência da pilha TCP/IP do framework.
* **Ciclo de Carga por Tecnologia**:
  1. **Inicialização & Cooldown Inicial**: Inicialização do servidor com um intervalo de espera de **5 segundos** antes do início do tráfego.
  2. **Fase de Aquecimento (Warm-up)**: Carga contínua com a concorrência correspondente durante **10 segundos**. Esta fase é vital para o compilador JIT (Java JVM e .NET CLR) e para o compilador dinâmico V8 (NodeJS) aquecerem e otimizarem os caminhos de código.
  3. **Fase de Medição Oficial**: Teste de carga ativa de **30 segundos** por concorrência.
  4. **Cooldown de Socket**: Pausa de **2 segundos** entre as baterias para permitir a limpeza segura dos sockets TCP no estado `TIME_WAIT` do sistema operacional.

### 3. Coleta de Métricas
* **Vazão (RPS) e Latências**: Coletadas diretamente dos relatórios de distribuição de percentis estruturados do `bombardier`.
* **Recursos (CPU e RAM)**: Capturados em tempo real durante a execução da carga através de chamadas instantâneas à API `docker stats --no-stream` do Docker Engine, calculando a média real consumida pelo contêiner.

---

## 📊 Tabela Consolidada de Resultados

### ⚡ Cenário 1: Concorrência de 128 Conexões Simultâneas
| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | Latência Média | Latência p99 | CPU Média | Memória Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **C# (.NET 8)** | **ASP.NET Core (Minimal API)** | Linux (Docker) | **35.019,13** | 3,65ms | 8,20ms | 71,91% | 27,34 MiB |
| **Free Pascal (FPC)** | **Horse (epoll)** | Linux (Docker) | **30.875,28** | 4,16ms | 15,89ms | 124,09% | 6,79 MiB |
| Go 1.21 | Fiber | Linux (Docker) | 25.174,51 | 5,10ms | 46,48ms | 122,99% | 10,84 MiB |
| Java 17 | Spring Boot | Linux (Docker) | 14.559,41 | 8,95ms | 76,45ms | 122,31% | 146,50 MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 9.317,00 | 13,74ms | 27,55ms | 67,93% | 14,18 MiB |
| Delphi 11 (Alexandria) | Horse (HTTP.sys) | Windows (Host) | 5.212,53 | 24,58ms | 292,47ms | 0,00%* | 11,62 MiB |
| Free Pascal (FPC) | Horse (fphttpserver) | Linux (Docker) | 2.165,10 | 59,56ms | 1,07s | 123,11% | 4,08 MiB |

---

### ⚡ Cenário 2: Concorrência de 512 Conexões Simultâneas
| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | Latência Média | Latência p99 | CPU Média | Memória Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **C# (.NET 8)** | **ASP.NET Core (Minimal API)** | Linux (Docker) | **40.115,97** | 12,79ms | 24,56ms | 86,88% | 34,49 MiB |
| **Free Pascal (FPC)** | **Horse (epoll)** | Linux (Docker) | **34.570,21** | 14,64ms | 51,05ms | 123,47% | 10,92 MiB |
| Go 1.21 | Fiber | Linux (Docker) | 27.050,87 | 19,08ms | 73,05ms | 121,02% | 17,08 MiB |
| Java 17 | Spring Boot | Linux (Docker) | 23.233,14 | 23,02ms | 75,33ms | 121,24% | 173,80 MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 8.843,27 | 58,11ms | 59,25ms | 69,93% | 17,79 MiB |
| Delphi 11 (Alexandria) | Horse (HTTP.sys) | Windows (Host) | 5.301,45 | 96,85ms | 618,33ms | 0,00%* | 15,38 MiB |
| Free Pascal (FPC) | Horse (fphttpserver) | Linux (Docker) | 1.847,61 | 288,74ms | 3,06s | 123,15% | 4,05 MiB |

---

### ⚡ Cenário 3: Concorrência de 1024 Conexões Simultâneas
| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | Latência Média | Latência p99 | CPU Média | Memória Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **C# (.NET 8)** | **ASP.NET Core (Minimal API)** | Linux (Docker) | **38.563,97** | 26,69ms | 27,26ms | 85,22% | 59,45 MiB |
| **Free Pascal (FPC)** | **Horse (epoll)** | Linux (Docker) | **33.159,34** | 32,96ms | 77,75ms | 122,86% | 15,03 MiB |
| Go 1.21 | Fiber | Linux (Docker) | 28.719,56 | 34,78ms | 92,87ms | 124,27% | 26,68 MiB |
| Java 17 | Spring Boot | Linux (Docker) | 19.311,16 | 55,51ms | 652,81ms | 105,81% | 191,60 MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 8.420,98 | 122,61ms | 6,02s | 73.74% | 29,18 MiB |
| Delphi 11 (Alexandria) | Horse (HTTP.sys) | Windows (Host) | 5.952,49 | 172,22ms | 753,54ms | 0,00%* | 38,11 MiB |
| Free Pascal (FPC) | Horse (fphttpserver) | Linux (Docker) | 1.699,85 | 642,32ms | 6,03s | 125,19% | 4,43 MiB |

> \* *Nota: O consumo de CPU do Delphi HTTP.sys é listado as 0% porque ele rodou diretamente no Host Windows (fora da coleta do subsistema Docker stats).*

---

## 📈 Estabilidade e Distribuição de Latência (Concorrência de 1024 Conexões)

Apenas olhar as latências médias mascara o comportamento real do servidor sob estresse extremo. A distribuição de percentis a seguir detalha a latência na concorrência limite (1024):

| Linguagem / Framework | S.O. | p50 (Mediana) | p90 | p99 (Cauda Longa) | Comportamento sob Carga |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **C# (.NET 8) - ASP.NET Core** | Linux | **12,84ms** | 16,85ms | **27,26ms** | **Excepcional**: Desvio mínimo entre mediana e p99. |
| **Free Pascal (FPC) - Horse (epoll)** | Linux | **23,44ms** | 65,59ms | **77,75ms** | **Excelente**: Altamente estável, abaixo de 80ms sob concorrência máxima. |
| **Go 1.21 - Fiber** | Linux | **21,84ms** | 75,11ms | **92,87ms** | **Ótimo**: Muito estável, mantendo a cauda longa em double-digits. |
| **Java 17 - Spring Boot** | Linux | **29,72ms** | 81,50ms | **652,81ms** | **Instável**: p99 sobe 8x devido a contenção do Tomcat/GC JVM. |
| **Delphi 11 - Horse (HTTP.sys)** | Windows | **0,62ms (620µs)** | 500,98ms | **753,54ms** | **Alta Variabilidade**: Mediana ultrarrápida no Host, mas p99 alto. |
| **JavaScript (Node.js 20) - Express** | Linux | **17,42ms** | 23,18ms | **6,02s** | **Crítico**: Event loop bloqueado, enfileiramento severo de segundos. |
| **Free Pascal (FPC) - fphttpserver** | Linux | **72,46ms** | 2,07s | **6,03s** | **Crítico**: Saturação total do modelo síncrono bloqueante. |

---

## 🧠 Análise Arquitetural Sênior

### 1. FPC/Lazarus (epoll) vs FPC/Lazarus (Default)
O resultado mais marcante do benchmark é a disparidade entre as duas camadas de transporte do Pascal:
* O driver padrão do FPC (`fphttpserver`) é baseado em sockets síncronos bloqueantes que exigem a alocação de threads ativas ou sofrem contenção na fila sob concorrência. Ele registrou apenas **1.699 RPS** sob 1024 conexões simultâneas, com uma latência média de **642ms** e p99 que travou nos **6,03 segundos**.
* Ao mudarmos para o provider **`HORSE_PROVIDER_EPOLL`** (o event loop assíncrono nativo do Linux), o Throughput saltou para incríveis **33.159 RPS** (um ganho de **mais de 19x** de performance bruta) e o p99 de latência ficou contido em excelentes **77,75ms**! Isso comprova a eficácia da arquitetura reativa assíncrona para I/O de rede massivo.

### 2. .NET (Kestrel) e FPC (epoll)
O **.NET Minimal API (Kestrel)** liderou o throughput em todos os cenários, registrando picos de **40.115 RPS**. A Microsoft fez otimizações brilhantes no pool de sockets do Kestrel nos últimos anos.
No entanto, o **FPC/Lazarus (epoll)** performou logo em seguida (na casa dos **33k a 34.5k RPS**), superando o **Go Fiber** (~28k RPS). 
O grande diferencial aqui é a **eficiência de recursos**:
* Sob 1024 conexões, o contêiner do **.NET** consumiu **59,4 MB** de memória RAM.
* O contêiner do **FPC epoll** utilizou apenas **15,0 MB** de RAM (menos de um quarto do consumo do .NET!).

### 3. Java (Spring Boot) e a JVM
O Java com Spring Boot entregou uma performance robusta (~23k RPS de pico), mas sofreu com a latência de p99 e foi de longe o maior consumidor de hardware, precisando de **191,6 MB de RAM** (mais de **12 vezes** o consumo do FPC epoll) devido ao peso do runtime da JVM e à inicialização do ecossistema do Spring Boot.

### 4. Node.js (Express) e o Gargalo de Thread Única
O Node.js com Express teve boa performance e latência em cargas moderadas (~9.3k RPS). No entanto, sob 1024 conexões, o fato de o Event Loop rodar em uma única thread gerou um grande enfileiramento de requisições HTTP, fazendo com que a cauda longa (p99) de latência saltasse para **6,02 segundos**. Isso mostra que o Node.js Express atinge o seu limite de estabilidade de latência em cenários de saturação extrema de sockets.

### 5. Delphi (HTTP.sys)
O Delphi rodando no Windows Host via driver de kernel HTTP.sys manteve estabilidade (~5.2k a 5.9k RPS). Ele operou com baixo consumo de memória (~38MB), mas seu throughput absoluto foi menor que as soluções Linux rodando em Epoll/Kestrel. Isso ocorre porque o HTTP.sys, por operar em nível de kernel, realiza muitas transições de contexto (user-mode <-> kernel-mode) para processar cada pacote individual HTTP, o que gera overhead em rotas simples com payloads curtos (como `/ping`).

---

## 🏁 Conclusão
O **Horse** é perfeitamente viável para microsserviços de altíssima concorrência e throughput massivo, **desde que compilado em FPC no Linux utilizando o provider `HORSE_PROVIDER_EPOLL`**. Nesta configuração, ele fornece performance equivalente à das stacks mais rápidas do mercado (.NET e Go), utilizando uma quantidade minúscula de memória RAM (menos de 15MB sob concorrência máxima de 1024 conexões), oferecendo um Retorno sobre Investimento (ROI) de infraestrutura formidável.
