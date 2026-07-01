# Resultados do Benchmark (RPS e LatÃªncia)

Teste executado em: 01/07/2026 17:07:09
DuraÃ§Ã£o dos Testes: 30s por cenÃ¡rio
Ambiente de Testes: Windows Host para HTTP.sys / Linux Docker para demais
Limites Docker: 2 CPUs, 512MB RAM

## âš¡ ConcorrÃªncia: 128 ConexÃµes SimultÃ¢neas

| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | LatÃªncia MÃ©dia | LatÃªncia p99 | CPU MÃ©dia | MemÃ³ria Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| Rust 1.75 | Actix-web | Linux (Docker) | 41676.13 | 3.07ms | 7.42ms | 53.59% | 3.387MiB |
| C# (.NET 8) | ASP.NET Core (Minimal API) | Linux (Docker) | 35003.13 | 3.65ms | 8.51ms | 71.41% | 29.05MiB |
| Delphi 13 (Florence) (Indy) | Horse (Indy) | Linux (Docker) | 34724.38 | 3.76ms | 20.53ms | 0% | 0B |
| Free Pascal (FPC) | Horse (fphttpserver) | Linux (Docker) | 32388.77 | 3.95ms | 21.45ms | 0% | 0B |
| Free Pascal (FPC) (epoll) | Horse (epoll) | Linux (Docker) | 30762.01 | 4.28ms | 23.01ms | 124.37% | 7.457MiB |
| Go 1.21 | Fiber | Linux (Docker) | 29345.15 | 4.57ms | 45.34ms | 125.24% | 10.27MiB |
| Delphi 13 (Florence) (Indy + Radix) | Horse (Indy + Radix) | Linux (Docker) | 26570.76 | 4.91ms | 24.70ms | 0% | 0B |
| Java 17 | Spring Boot | Linux (Docker) | 18165.88 | 7.29ms | 63.14ms | 120.56% | 154.3MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 10431.54 | 12.29ms | 25.00ms | 67.13% | 14.72MiB |
| Delphi 13 (Florence) (HTTP.sys) | Horse (HTTP.sys) | Windows (Host) | 5064.56 | 25.34ms | 306.25ms | 0% | 12.85MiB |
| Delphi 13 (Florence) (HTTP.sys + Radix) | Horse (HTTP.sys + Radix) | Windows (Host) | 2816.58 | 45.50ms | 444.47ms | 0% | 12.77MiB |

## âš¡ ConcorrÃªncia: 512 ConexÃµes SimultÃ¢neas

| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | LatÃªncia MÃ©dia | LatÃªncia p99 | CPU MÃ©dia | MemÃ³ria Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| Rust 1.75 | Actix-web | Linux (Docker) | 45847.95 | 11.18ms | 22.80ms | 63.09% | 2.773MiB |
| C# (.NET 8) | ASP.NET Core (Minimal API) | Linux (Docker) | 43970.86 | 11.66ms | 23.20ms | 86.35% | 35.45MiB |
| Delphi 13 (Florence) (Indy) | Horse (Indy) | Linux (Docker) | 35087.46 | 15.23ms | 51.11ms | 0% | 0B |
| Free Pascal (FPC) | Horse (fphttpserver) | Linux (Docker) | 34812.30 | 14.78ms | 50.49ms | 0% | 0B |
| Go 1.21 | Fiber | Linux (Docker) | 34499.15 | 16.25ms | 65.46ms | 124.69% | 18MiB |
| Free Pascal (FPC) (epoll) | Horse (epoll) | Linux (Docker) | 34261.68 | 15.62ms | 52.67ms | 123.42% | 11.91MiB |
| Delphi 13 (Florence) (Indy + Radix) | Horse (Indy + Radix) | Linux (Docker) | 29457.40 | 17.45ms | 57.18ms | 0% | 0B |
| Java 17 | Spring Boot | Linux (Docker) | 26129.39 | 20.96ms | 70.71ms | 120.43% | 183MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 9469.01 | 54.33ms | 55.66ms | 69.58% | 16.27MiB |
| Delphi 13 (Florence) (HTTP.sys + Radix) | Horse (HTTP.sys + Radix) | Windows (Host) | 4982.79 | 103.02ms | 642.08ms | 0% | 16.25MiB |
| Delphi 13 (Florence) (HTTP.sys) | Horse (HTTP.sys) | Windows (Host) | 4784.68 | 107.12ms | 619.77ms | 0% | 16.17MiB |

## âš¡ ConcorrÃªncia: 1024 ConexÃµes SimultÃ¢neas

| Linguagem / Compilador | Framework / Driver | S.O. / Ambiente | RPS (Throughput) | LatÃªncia MÃ©dia | LatÃªncia p99 | CPU MÃ©dia | MemÃ³ria Final |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| Rust 1.75 | Actix-web | Linux (Docker) | 44244.56 | 23.20ms | 27.27ms | 61.76% | 3.371MiB |
| C# (.NET 8) | ASP.NET Core (Minimal API) | Linux (Docker) | 42619.61 | 24.08ms | 34.99ms | 90.39% | 56.53MiB |
| Delphi 13 (Florence) (Indy + Radix) | Horse (Indy + Radix) | Linux (Docker) | 33254.75 | 31.23ms | 75.70ms | 0% | 0B |
| Free Pascal (FPC) (epoll) | Horse (epoll) | Linux (Docker) | 32037.61 | 31.79ms | 76.38ms | 123.04% | 15.89MiB |
| Free Pascal (FPC) | Horse (fphttpserver) | Linux (Docker) | 31149.34 | 34.10ms | 78.39ms | 0% | 0B |
| Go 1.21 | Fiber | Linux (Docker) | 28835.71 | 35.54ms | 94.80ms | 123.3% | 26.11MiB |
| Delphi 13 (Florence) (Indy) | Horse (Indy) | Linux (Docker) | 25280.85 | 40.87ms | 88.06ms | 0% | 0B |
| Java 17 | Spring Boot | Linux (Docker) | 22552.11 | 48.08ms | 105.33ms | 121.67% | 200MiB |
| JavaScript (Node.js 20) | Express | Linux (Docker) | 8650.31 | 118.97ms | 100.24ms | 73.35% | 30.26MiB |
| Delphi 13 (Florence) (HTTP.sys + Radix) | Horse (HTTP.sys + Radix) | Windows (Host) | 6560.95 | 156.20ms | 759.68ms | 0% | 18.48MiB |
| Delphi 13 (Florence) (HTTP.sys) | Horse (HTTP.sys) | Windows (Host) | 5737.47 | 178.38ms | 754.78ms | 0% | 19.71MiB |


