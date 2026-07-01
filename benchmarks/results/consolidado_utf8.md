# Resultados do Benchmark (RPS e LatÃªncia)

Teste executado em: 01/07/2026 17:46:54
DuraÃ§Ã£o dos Testes: 30s por cenÃ¡rio
Ambiente de Testes: Windows Host para HTTP.sys / Linux Docker para demais
Limites Docker: 2 CPUs, 512MB RAM

## âš¡ ConcorrÃªncia: 128 ConexÃµes SimultÃ¢neas

| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | LatÃªncia MÃ©dia | LatÃªncia p99 | CPU MÃ©dia | MemÃ³ria Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| Rust 1.75 | Actix-web | Linux (Docker) | 39016.15 | 3.28ms | 7.72ms | 55.18% | 3.496MiB |
| C# (.NET 8) | ASP.NET Core (Minimal API) | Linux (Docker) | 36261.76 | 3.53ms | 7.85ms | 73.18% | 27.64MiB |
| Go 1.21 | Fiber | Linux (Docker) | 27012.29 | 5.02ms | 46.91ms | 124.24% | 10.64MiB |
| Java 17 | Spring Boot | Linux (Docker) | 20458.41 | 6.69ms | 64.04ms | 122.7% | 160.3MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 10150.45 | 12.61ms | 25.33ms | 67.62% | 14.32MiB |
| Delphi 13 (Florence) (HTTP.sys) | Horse (HTTP.sys) | Windows (Host) | 5635.71 | 22.74ms | 260.83ms | 0% | 12.65MiB |
| Delphi 13 (Florence) (HTTP.sys + Radix) | Horse (HTTP.sys + Radix) | Windows (Host) | 5448.11 | 23.53ms | 275.99ms | 0% | 12.91MiB |
| Delphi 13 (Florence) (Indy + Radix) | Horse (Indy + Radix) | Linux (Docker) | 1036.14 | 121.42ms | 392.27ms | 121.49% | 21.12MiB |
| Delphi 13 (Florence) (Indy) | Horse (Indy) | Linux (Docker) | 931.36 | 141.72ms | 416.38ms | 123.33% | 22.11MiB |

## âš¡ ConcorrÃªncia: 512 ConexÃµes SimultÃ¢neas

| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | LatÃªncia MÃ©dia | LatÃªncia p99 | CPU MÃ©dia | MemÃ³ria Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| Rust 1.75 | Actix-web | Linux (Docker) | 41724.01 | 12.29ms | 25.17ms | 64.73% | 4.164MiB |
| C# (.NET 8) | ASP.NET Core (Minimal API) | Linux (Docker) | 40386.44 | 12.70ms | 25.48ms | 83.19% | 34.39MiB |
| Go 1.21 | Fiber | Linux (Docker) | 31902.52 | 17.65ms | 71.02ms | 124.73% | 17.9MiB |
| Java 17 | Spring Boot | Linux (Docker) | 24741.71 | 21.30ms | 73.72ms | 121.01% | 189.8MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 8703.22 | 58.98ms | 56.05ms | 69.53% | 19.41MiB |
| Delphi 13 (Florence) (HTTP.sys + Radix) | Horse (HTTP.sys + Radix) | Windows (Host) | 6049.06 | 85.02ms | 596.94ms | 0% | 16.18MiB |
| Delphi 13 (Florence) (HTTP.sys) | Horse (HTTP.sys) | Windows (Host) | 5462.80 | 93.84ms | 596.68ms | 0% | 16.04MiB |
| Delphi 13 (Florence) (Indy + Radix) | Horse (Indy + Radix) | Linux (Docker) | 1303.05 | 440.12ms | 1.40s | 128.17% | 39.91MiB |
| Delphi 13 (Florence) (Indy) | Horse (Indy) | Linux (Docker) | 1216.09 | 420.81ms | 1.40s | 128.44% | 40.68MiB |

## âš¡ ConcorrÃªncia: 1024 ConexÃµes SimultÃ¢neas

| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | LatÃªncia MÃ©dia | LatÃªncia p99 | CPU MÃ©dia | MemÃ³ria Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| Rust 1.75 | Actix-web | Linux (Docker) | 40321.76 | 25.55ms | 27.00ms | 65.89% | 4.031MiB |
| C# (.NET 8) | ASP.NET Core (Minimal API) | Linux (Docker) | 38737.40 | 26.58ms | 29.12ms | 85.3% | 57.11MiB |
| Go 1.21 | Fiber | Linux (Docker) | 27683.99 | 37.31ms | 99.84ms | 123.69% | 24.48MiB |
| Java 17 | Spring Boot | Linux (Docker) | 13459.66 | 79.47ms | 1.40s | 57.26% | 206.8MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 8121.83 | 126.57ms | 2.57s | 71.89% | 30.5MiB |
| Delphi 13 (Florence) (HTTP.sys + Radix) | Horse (HTTP.sys + Radix) | Windows (Host) | 6425.99 | 159.62ms | 749.85ms | 0% | 18.49MiB |
| Delphi 13 (Florence) (HTTP.sys) | Horse (HTTP.sys) | Windows (Host) | 5916.16 | 173.12ms | 765.58ms | 0% | 19.57MiB |
| Delphi 13 (Florence) (Indy + Radix) | Horse (Indy + Radix) | Linux (Docker) | 1671.30 | 673.65ms | 2.85s | 128.61% | 48.31MiB |
| Delphi 13 (Florence) (Indy) | Horse (Indy) | Linux (Docker) | 1660.56 | 600.46ms | 1.91s | 133.1% | 49.43MiB |


