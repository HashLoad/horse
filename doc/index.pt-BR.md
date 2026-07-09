# Documentação do Horse

*Leia em [English](./index.md) ou [Português (BR)](./index.pt-BR.md).*

Bem-vindo. Este é o índice da documentação do [Horse](https://github.com/HashLoad/horse) — um framework web para Delphi e Lazarus inspirado no Express.

Se você é novo por aqui, comece por [Primeiros passos](./getting-started.pt-BR.md). Se já tem um servidor rodando e quer fazer uma alteração específica, vá direto ao tópico relevante abaixo.

---

## Ordem de leitura para iniciantes

1. **[Primeiros passos](./getting-started.pt-BR.md)** — instalar, escrever um servidor hello-world e executá-lo.
2. **[Roteamento](./routing.pt-BR.md)** — declarar endpoints, parâmetros de caminho, grupos de rotas, query strings.
3. **[Request e Response](./request-response.pt-BR.md)** — ler a requisição, escrever a resposta, headers, cookies, sessões, upload/download de arquivos.
4. **[Middleware](./middleware.pt-BR.md)** — encadear handlers, ordem de registro, criar o seu. Para publicar middleware reutilizável, veja **[Criando um Middleware](./writing-middleware.pt-BR.md)**.
5. **[Providers e Tipos de aplicação](./providers.pt-BR.md)** — escolher o Provider de transporte (Indy padrão; CrossSocket, mORMot2 e ICS opcionais; HttpSys, epoll e IOCP embutidos) e o Tipo de aplicação (Console padrão, VCL, Daemon, LCL, HTTPApplication) — ou um tipo de aplicação host-managed (Apache, ISAPI, CGI, FastCGI).

## Referência

| Documento | O que você encontra |
|---|---|
| [Primeiros passos](./getting-started.pt-BR.md) | Instalação via Boss; exemplos mínimos em Delphi e Lazarus; convenções de estrutura de projeto. |
| [Roteamento](./routing.pt-BR.md) | `THorse.Get` / `Post` / `Put` / `Delete` / `Patch` / `Head` / `Use`; parâmetros de caminho; grupos de rotas; wildcards; enum de método HTTP. |
| [Request e Response](./request-response.pt-BR.md) | `THorseRequest` (body, params, query, headers, cookies, sessions, multipart). `THorseResponse` (`Send`, `Status`, `ContentType`, `AddHeader`, `RedirectTo`, `SendFile`, `Download`, `RawWebResponse`). |
| [Middleware](./middleware.pt-BR.md) | O modelo `Next` proc; built-in vs custom; ordem de registro; por-rota vs global. |
| [Criando um Middleware](./writing-middleware.pt-BR.md) | Criando um middleware de qualidade de produção: esqueleto, padrões de configuração, thread safety, código neutro a Provider, armadilhas entre compiladores, matriz de testes, empacotamento Boss, publicação. |
| [Providers e Tipos de aplicação](./providers.pt-BR.md) | O modelo de dois eixos: **Provider** (transporte — Indy padrão; CrossSocket, mORMot2, ICS opcionais; HttpSys, epoll e IOCP embutidos) × **Tipo de aplicação** (Console / VCL / Daemon / LCL / HTTPApplication, mais host-managed Apache / ISAPI / CGI / FCGI). Matriz de compatibilidade e guia de escolha. |
| [Ecossistema de Middlewares](./middleware-ecosystem.pt-BR.md) | Pacotes oficiais `HashLoad/*` e a lista mantida pela comunidade. |
| [Observabilidade e Telemetria](./telemetry.pt-BR.md) | Configuração de rastreamento distribuído (OpenTelemetry) e coleta de métricas (Prometheus). |
| [Suporte de Compilador](./compiler-support.pt-BR.md) | Versões testadas do Delphi, versões do FPC, plataformas-alvo, guards de versão de compilador. |
| [Cheatsheet de Deploy](./deployment.pt-BR.md) | Referência de uma página pra entregar um binário CrossSocket ou mORMot2 como qualquer um dos sete formatos de Aplicação (Console / VCL / Daemon / Serviço Windows / daemon FPC / LCL / HTTPApplication FPC). |
| [Testes de Integridade](./integrity-testing.pt-BR.md) | Testes de integração automatizados, resiliência (Access Violation) e limites de Stack. |

## Como a documentação está organizada

```
README.md                      ← página inicial; introdução curta e links
doc/
├── index.md / index.pt-BR.md  ← este arquivo
├── getting-started.*.md       ← primeiros 30 minutos com o Horse
├── routing.*.md               ← ligação URL → handler
├── request-response.*.md      ← API de THorseRequest e THorseResponse
├── middleware.*.md            ← encadeamento de handlers
├── providers.*.md             ← escolha de transporte
├── iocp.*.md                  ← portas de conclusão assíncronas (Windows)
├── epoll.*.md                 ← laço de eventos assíncronos (Linux)
├── telemetry.*.md             ← integração com OpenTelemetry e Prometheus
├── middleware-ecosystem.*.md  ← catálogo de pacotes
├── integrity-testing.*.md     ← testes de integridade e resiliência
└── compiler-support.*.md      ← versões / plataformas
```

Cada documento é independente e referencia os outros quando relevante. Não há ordem obrigatória além do fluxo para iniciantes acima.

## Contribuindo com a documentação

Edições são bem-vindas — abra um PR contra `master` modificando o arquivo relevante em `doc/`. Mantenha cada página focada em um tópico; se uma seção crescer além de algumas centenas de linhas, divida em um documento irmão e referencie a partir do `index.md`.

## Travou em algo?

- Canal no Telegram: [@hashload](https://t.me/hashload)
- GitHub Issues: [`HashLoad/horse/issues`](https://github.com/HashLoad/horse/issues)
- O código-fonte é curto e legível — em caso de dúvida, `Horse.pas`, `Horse.Request.pas` e `Horse.Response.pas` juntos somam menos de 2 000 linhas.
