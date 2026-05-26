# Suporte de Compilador

*Leia em [English](./compiler-support.md) ou [Português (BR)](./compiler-support.pt-BR.md).*

O Horse mira uma faixa ampla de versões de Delphi e Free Pascal. Esta página lista o que é suportado, o que é testado e as considerações por Provider / por Tipo de aplicação.

Para o modelo de dois eixos (Provider — Indy / fphttpserver / CrossSocket — × Tipo de aplicação — Console / VCL / Daemon / Apache / ISAPI / CGI / FCGI / LCL / HTTPApplication), veja [Providers e Tipos de aplicação](./providers.pt-BR.md).

---

## Delphi

| Versão | Versão do compilador | Status |
|---|---|---|
| Delphi 13 Florence | 38.0 | Suportado |
| Delphi 12 Athens | 36.0 | **Recomendado** |
| Delphi 11 Alexandria | 35.0 | Suportado |
| Delphi 10.4 Sydney | 34.0 | Suportado |
| Delphi 10.3 Rio | 33.0 | Suportado |
| Delphi 10.2 Tokyo | 32.0 | Suportado |
| Delphi 10.1 Berlin | 31.0 | Suportado |
| Delphi 10 Seattle | 30.0 | Suportado |
| Delphi XE8 | 29.0 | Suportado |
| Delphi XE7 | 28.0 | Mínimo |

O CI roda no Delphi 11 e 12 contra a suíte `tests/` oficial. Qualquer coisa abaixo de 10.4 recebe consideração de compatibilidade de código-fonte mas não é exercitada ativamente.

## Free Pascal / Lazarus

| Versão | Status |
|---|---|
| FPC 3.2.2 + Lazarus 2.2 | **Recomendado** |
| FPC 3.2.0 + Lazarus 2.0 | Suportado |
| FPC 3.3.1 (trunk) | Suportado com `{$MODE DELPHI}{$H+}` |

## Plataformas-alvo

O conjunto de plataformas depende de qual Provider é selecionado e qual Tipo de aplicação está sendo construído. As duas tabelas abaixo correspondem ao [modelo de dois eixos](./providers.pt-BR.md).

### Providers self-hosted × Plataforma

O Provider que é dono do socket. Indy é só-Delphi; `fphttpserver` é só-FPC; CrossSocket atende ambos.

| Plataforma | Indy _(padrão Delphi)_ | `fphttpserver` _(padrão FPC)_ | CrossSocket _(`HORSE_PROVIDER_CROSSSOCKET`; alias legado `HORSE_CROSSSOCKET`)_ |
|---|:---:|:---:|:---:|
| Windows x86 / x64 | ✔ | ✔ | ✔ |
| Linux x64 | ✔ | ✔ | ✔ _(alvo primário)_ |
| macOS Intel / ARM64 | ✔ | ✔ | ✔ |
| FreeBSD | — | ✔ | ✔ _(via kqueue)_ |
| Android / iOS | — | — | — |

### Tipos de aplicação host-managed × Plataforma

Estes não usam um Provider self-hosted — o processo host é dono do socket. Cobertura depende de onde o host roda.

| Plataforma | Módulo Apache | ISAPI _(IIS)_ | CGI | FastCGI |
|---|:---:|:---:|:---:|:---:|
| Windows x86 / x64 | ✔ | ✔ | ✔ | ✔ |
| Linux x64 | ✔ | — | ✔ | ✔ |
| macOS Intel / ARM64 | — | — | ✔ | — |
| FreeBSD | — | — | ✔ | — |
| Android / iOS | — | — | — | — |

(Apenas alvos server-side — para código HTTP **cliente**, as bibliotecas HTTP do Delphi suportam plataformas móveis mais amplas.)

## Notas por Provider (self-hosted)

### Indy — padrão Delphi

- Funciona em todas as versões Delphi listadas acima. **Não disponível no FPC** — builds FPC usam `fphttpserver`.
- Provider unit: `Horse.Provider.Console` / `Horse.Provider.VCL` / `Horse.Provider.Daemon`. Cada uma `uses IdHTTPWebBrokerBridge, IdContext, IdSSLOpenSSL, …`.
- DLLs do OpenSSL (`libeay32.dll` / `ssleay32.dll` ou `libssl-1_1-x64.dll` / `libcrypto-1_1-x64.dll`) precisam acompanhar o binário para HTTPS.

### `fphttpserver` — padrão FPC

- Funciona em todas as versões FPC listadas acima. **Não disponível no Delphi** — builds Delphi usam Indy.
- Provider unit: `Horse.Provider.FPC.Daemon` / `Horse.Provider.FPC.HTTPApplication` / `Horse.Provider.FPC.LCL`. Cada uma `uses fphttpserver, fpHTTP, httpdefs`.
- SSL via OpenSSL através dos handlers HTTP padrão do FPC — sem dependência do Indy.

### CrossSocket — opcional, entre compiladores

- Exige **Delphi 10.2 Tokyo ou mais recente** (a biblioteca `Delphi-Cross-Socket` usa `var` inline e tipos `System.Net` não presentes em versões anteriores).
- Exige **FPC 3.2.0 ou mais recente** para builds FPC.
- Plataformas testadas: Windows x64, Linux x64, macOS ARM64.
- Substitui Indy (Delphi) e `fphttpserver` (FPC) por um único transporte assíncrono para os dois compiladores.
- Veja [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket#readme) para a matriz atual de teste por versão.

## Tipos de aplicação host-managed

Apache / ISAPI / CGI / FastCGI **não usam um Provider self-hosted** — nem Indy, nem `fphttpserver`, nem CrossSocket está envolvido. O processo host (Apache httpd, IIS, o webserver) é dono do socket e entrega a requisição ao Horse via subclasses de `Web.HTTPApp` (Delphi) ou `fpFCGI` / `fpHTTP` (FPC).

- **Módulo Apache** — Delphi (`Web.HTTPD24Impl`, `Web.ApacheApp`). Construa o `.so` / `.dll` com a arquitetura correspondente à do Apache.
- **Extensão ISAPI** — só Delphi (`Web.Win.ISAPIApp`). Windows + IIS. Combine a arquitetura com o pool de aplicações do IIS (pool 32-bit → build Win32).
- **CGI** — Delphi (`Web.CGIApp`) e FPC. Multiplataforma; um processo por requisição.
- **FastCGI** — FPC (`fpFCGI`). Processo persistente; conversa com o webserver via Unix socket ou TCP. FCGI no Delphi requer biblioteca de terceiros e não faz parte dos providers do Horse.

## Guards de versão de compilador no código-fonte

O Horse usa alguns guards defensivos. Se você contribui patches:

- `{$IF DEFINED(FPC)}` — código somente-FPC (RTL diferente, sintaxe de generics diferente).
- `{$IF CompilerVersion >= 32.0}` — Delphi 10.2 Tokyo introduziu retorno `Int64` em `TWebRequest.GetIntegerVariable` / `TWebResponse.SetIntegerVariable`. Qualquer override desses precisa do guard.
- `{$IF CompilerVersion >= 33.0}` — Delphi 10.3 Rio introduziu `var` inline. O core do Horse evita `var` inline para continuar compilando no XE7.

A cada alteração, compile mentalmente contra `dcc32` (Delphi) e `fpc` (FPC). Procedimentos anônimos, generics e tipos Web/HTTPApp são os pontos mais comuns de tropeço entre compiladores.

## Reportando um bug específico de versão

Ao abrir uma issue:

1. Inclua a **versão exata** de Delphi / FPC e a **plataforma-alvo** (Win64, Linux64, …).
2. Inclua o(s) **define(s) do provider** ativo(s) no projeto.
3. Diga se o bug é reproduzível no sample do provider correspondente em `samples/`.

A triagem é normalmente rápida quando plataforma e provider estão definidos.

## Veja também

- [Primeiros passos](./getting-started.pt-BR.md) — caminhos de instalação por IDE.
- [Providers e Tipos de aplicação](./providers.pt-BR.md) — o modelo de dois eixos: quais combinações Provider × Tipo-de-aplicação existem, e quais units cada uma usa.
