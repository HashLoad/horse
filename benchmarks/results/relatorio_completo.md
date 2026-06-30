# Relatório de Benchmark - Horse vs Outras Tecnologias

Este relatório apresenta os resultados finais consolidados do benchmark de performance de alta carga, comparando o **Horse** (rodando em Delphi e FPC/Lazarus) contra os principais frameworks web do mercado (**.NET Minimal API**, **Go Fiber**, **Java Spring Boot** e **Node.js Express**).

O cenário avaliou o throughput (RPS) e latência do endpoint `/ping` (plain text) sob concorrências de **128**, **512** e **1024** conexões simultâneas. No Linux (Docker), todos os contêineres foram limitados a **2 CPUs e 512MB RAM** para garantir igualdade de condições.

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

> \* *Nota: O consumo de CPU do Delphi HTTP.sys é listado como 0% porque ele rodou diretamente no Host Windows (fora da coleta do subsistema Docker stats).*

---

## 🧠 Análise Arquitetural Sênior

### 1. FPC/Lazarus (epoll) vs FPC/Lazarus (Default)
O resultado mais marcante do benchmark é a disparidade entre as duas camadas de transporte do Pascal:
* O driver padrão do FPC (`fphttpserver`) é baseado em sockets síncronos bloqueantes que exigem a alocação de threads ativas ou sofrem contenção na fila sob concorrência. Ele registrou apenas **1.699 RPS** sob 1024 conexões simultâneas, com uma latência média pesada de **642ms**.
* Ao mudarmos para o provider **`HORSE_PROVIDER_EPOLL`** (o event loop assíncrono nativo do Linux), o Throughput saltou para incríveis **33.159 RPS** (um ganho de **mais de 19x** de performance bruta) e a latência despencou para apenas **32ms**! Isso comprova a eficácia da arquitetura reativa assíncrona para I/O de rede massivo.

### 2. .NET (Kestrel) e FPC (epoll)
O **.NET Minimal API (Kestrel)** liderou o throughput em todos os cenários, registrando picos de **40.115 RPS**. A Microsoft fez otimizações brilhantes no pool de sockets do Kestrel nos últimos anos.
No entanto, o **FPC/Lazarus (epoll)** performou logo em seguida (na casa dos **33k a 34.5k RPS**), superando o **Go Fiber** (~28k RPS). 
O grande diferencial aqui é a **eficiência de recursos**:
* Sob 1024 conexões, o contêiner do **.NET** consumiu **59,4 MB** de memória RAM.
* O contêiner do **FPC epoll** utilizou apenas **15,0 MB** de RAM (menos de um quarto do consumo do .NET!).

### 3. Java (Spring Boot) e a JVM
O Java com Spring Boot entregou uma performance robusta (~23k RPS de pico), mas sofreu com a latência de p99 e foi de longe o maior consumidor de hardware, precisando de **191,6 MB de RAM** (mais de **12 vezes** o consumo do FPC epoll) devido ao peso do runtime da JVM e à inicialização do ecossistema do Spring Boot.

### 4. Node.js (Express)
Ficou no meio da tabela (~8.4k a 9.3k RPS). O loop de eventos single-thread do Node.js sofre de gargalos quando o middleware de processamento do Express é empilhado, e o consumo de memória sob carga alta aumentou moderadamente para ~29 MB.

### 5. Delphi (HTTP.sys)
O Delphi rodando no Windows Host via driver de kernel HTTP.sys manteve estabilidade (~5.2k a 5.9k RPS). Ele operou com baixo consumo de memória (~38MB), mas seu throughput absoluto foi menor que as soluções Linux rodando em Epoll/Kestrel. Isso ocorre porque o HTTP.sys, por operar em nível de kernel, realiza muitas transições de contexto (user-mode <-> kernel-mode) para processar cada pacote individual HTTP, o que gera overhead em rotas simples com payloads curtos (como `/ping`).

---

## 🏁 Conclusão
O **Horse** é perfeitamente viável para microsserviços de altíssima concorrência e throughput massivo, **desde que compilado em FPC no Linux utilizando o provider `HORSE_PROVIDER_EPOLL`**. Nesta configuração, ele fornece performance equivalente à das stacks mais rápidas do mercado (.NET e Go), utilizando uma quantidade minúscula de memória RAM (menos de 15MB sob concorrência máxima de 1024 conexões), oferecendo um Retorno sobre Investimento (ROI) de infraestrutura formidável.
