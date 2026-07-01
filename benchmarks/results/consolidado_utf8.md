# Resultados do Benchmark (RPS e Latência)

Teste executado em: 01/07/2026 15:34:02
Duração dos Testes: 30s por cenário
Ambiente de Testes: Windows Host para HTTP.sys / Linux Docker para demais
Limites Docker: 2 CPUs, 512MB RAM

## ⚡ Concorrência: 128 Conexões Simultâneas

| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | Latência Média | Latência p99 | CPU Média | Memória Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| Rust 1.75 | Actix-web | Linux (Docker) | 37028.65 | 3.46ms | 8.04ms | 55.77% | 4.156MiB |
| C# (.NET 8) | ASP.NET Core (Minimal API) | Linux (Docker) | 35156.97 | 3.64ms | 8.22ms | 73.43% | 28.89MiB |
| Go 1.21 | Fiber | Linux (Docker) | 27562.24 | 4.70ms | 45.68ms | 124.64% | 10.57MiB |
| Java 17 | Spring Boot | Linux (Docker) | 16993.54 | 7.90ms | 74.01ms | 122.88% | 172.4MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 9584.20 | 13.36ms | 26.85ms | 67.6% | 13.82MiB |
| Delphi 13 (Florence) (HTTP.sys) | Horse (HTTP.sys) | Windows (Host) | 4735.54 | 27.06ms | 306.70ms | 0% | 12.81MiB |
| Delphi 13 (Florence) (HTTP.sys + Radix) | Horse (HTTP.sys + Radix) | Windows (Host) | 4601.44 | 27.85ms | 291.71ms | 0% | 12.86MiB |
| Free Pascal (FPC) | Horse (fphttpserver) | Linux (Docker) | 1865.87 | 71.17ms | 1.09s | 123.83% | 3.824MiB |
| Delphi 13 (Florence) (Indy) | Horse (Indy) | Linux (Docker) | 1194.17 | 107.34ms | 386.31ms | 122.48% | 21.27MiB |
| Delphi 13 (Florence) (Indy + Radix) | Horse (Indy + Radix) | Linux (Docker) | 1188.53 | 115.45ms | 397.54ms | 124.8% | 21.44MiB |
| Free Pascal (FPC) (epoll) | Horse (epoll) | Linux (Docker) | 62.18 | 2.00s | 8.06s | 1.75% | 44.5MiB |

## ⚡ Concorrência: 512 Conexões Simultâneas

| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | Latência Média | Latência p99 | CPU Média | Memória Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| Rust 1.75 | Actix-web | Linux (Docker) | 40517.38 | 12.67ms | 26.14ms | 65.93% | 4.223MiB |
| C# (.NET 8) | ASP.NET Core (Minimal API) | Linux (Docker) | 39898.08 | 12.87ms | 25.75ms | 86.93% | 34.89MiB |
| Go 1.21 | Fiber | Linux (Docker) | 34579.74 | 16.42ms | 65.66ms | 122.72% | 17.8MiB |
| Java 17 | Spring Boot | Linux (Docker) | 21723.53 | 23.27ms | 75.19ms | 122.26% | 199.1MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 8651.14 | 59.30ms | 59.74ms | 69.91% | 19.47MiB |
| Delphi 13 (Florence) (HTTP.sys) | Horse (HTTP.sys) | Windows (Host) | 5550.81 | 92.43ms | 597.61ms | 0% | 15.97MiB |
| Delphi 13 (Florence) (HTTP.sys + Radix) | Horse (HTTP.sys + Radix) | Windows (Host) | 4676.23 | 109.89ms | 623.57ms | 0% | 16.26MiB |
| Free Pascal (FPC) | Horse (fphttpserver) | Linux (Docker) | 1596.54 | 322.92ms | 3.07s | 124.42% | 3.789MiB |
| Delphi 13 (Florence) (Indy + Radix) | Horse (Indy + Radix) | Linux (Docker) | 1406.97 | 401.08ms | 1.38s | 126.19% | 40.2MiB |
| Delphi 13 (Florence) (Indy) | Horse (Indy) | Linux (Docker) | 1291.05 | 449.02ms | 1.40s | 129.79% | 40.65MiB |
| Free Pascal (FPC) (epoll) | Horse (epoll) | Linux (Docker) | 257.65 | 2.05s | 6.04s | 5.02% | 160.7MiB |

## ⚡ Concorrência: 1024 Conexões Simultâneas

| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | Latência Média | Latência p99 | CPU Média | Memória Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| Rust 1.75 | Actix-web | Linux (Docker) | 41996.28 | 24.39ms | 28.59ms | 63.86% | 4.273MiB |
| C# (.NET 8) | ASP.NET Core (Minimal API) | Linux (Docker) | 38850.16 | 26.99ms | 28.97ms | 84.67% | 58.21MiB |
| Go 1.21 | Fiber | Linux (Docker) | 31117.45 | 35.34ms | 93.38ms | 121.02% | 26.6MiB |
| Java 17 | Spring Boot | Linux (Docker) | 24306.73 | 44.23ms | 100.55ms | 122.39% | 218.2MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 7319.48 | 140.91ms | 9.15s | 73.57% | 29.11MiB |
| Delphi 13 (Florence) (HTTP.sys) | Horse (HTTP.sys) | Windows (Host) | 6614.23 | 155.03ms | 786.82ms | 0% | 21.12MiB |
| Delphi 13 (Florence) (HTTP.sys + Radix) | Horse (HTTP.sys + Radix) | Windows (Host) | 5292.51 | 193.89ms | 1.28s | 0% | 19.16MiB |
| Delphi 13 (Florence) (Indy + Radix) | Horse (Indy + Radix) | Linux (Docker) | 1603.95 | 588.31ms | 1.80s | 132.22% | 48.66MiB |
| Delphi 13 (Florence) (Indy) | Horse (Indy) | Linux (Docker) | 1601.81 | 587.51ms | 1.81s | 128.94% | 48.28MiB |
| Free Pascal (FPC) | Horse (fphttpserver) | Linux (Docker) | 1440.66 | 698.73ms | 6.04s | 129.98% | 4.762MiB |
| Free Pascal (FPC) (epoll) | Horse (epoll) | Linux (Docker) | 934.75 | 1.05s | 4.12s | 2.44% | 218.6MiB |
