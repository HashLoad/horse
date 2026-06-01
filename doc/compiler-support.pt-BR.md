# Suporte de Compilador

*Leia em [English](./compiler-support.md) ou [Português (BR)](./compiler-support.pt-BR.md).*

O Horse abrange uma ampla gama de versões do Delphi e do Free Pascal. Esta página lista o que é suportado, o que é testado e as considerações por Provider / por Tipo de aplicação.

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

O Provider que é dono do socket. Indy é só-Delphi; `fphttpserver` é só-FPC; CrossSocket e mORMot2 atendem ambos.

| Plataforma | Indy _(padrão Delphi)_ | `fphttpserver` _(padrão FPC)_ | CrossSocket _(`HORSE_PROVIDER_CROSSSOCKET`)_ | mORMot2 _(`HORSE_PROVIDER_MORMOT`)_ |
|---|:---:|:---:|:---:|:---:|
| Windows x86 / x64 | ✔ | ✔ | ✔ _(IOCP)_ | ✔ _(IOCP; também http.sys via `THttpApiServer`)_ |
| Linux x64 | ✔ | ✔ | ✔ _(epoll, alvo primário)_ | ✔ _(epoll)_ |
| macOS Intel / ARM64 | ✔ | ✔ | ✔ _(kqueue)_ | ✔ |
| FreeBSD | — | ✔ | ✔ _(kqueue)_ | ✔ _(kqueue)_ |
| Android / iOS | — | — | — | — |

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
- **Instalação é manual** (espelha o padrão de instalação do mORMot2, não Boss): clone [`winddriver/Delphi-Cross-Socket`](https://github.com/winddriver/Delphi-Cross-Socket) (upstream) e adicione seus search paths. O upstream **não** embute o CnPack — clone também [`cnpack/cnvcl`](https://github.com/cnpack/cnvcl) e adicione `Source/Common` + `Source/Crypto` ao search path (o `Utils.Hash.pas` do Delphi-Cross-Socket usa `CnMD5`, `CnSHA1`, `CnSHA2`, `CnSM3` e `CnPemUtils` mais suas dependências transitivas). Três bug fixes que antes eram exclusivos do fork (`PATCH-IOCP-1`, hang do parser para respostas sem body, e o nil-guard em `_OnBodyEnd`) já foram ibtegradas no upstream a partir de 2026-Q2 — não é necessário aplicar patches no upstream para HTTP e HTTPS unidirecional.
- **TLS mútuo no servidor ainda é exclusivo do fork no nível de fonte.** O `Net.CrossSslSocket.Base.pas` e `Net.CrossSslSocket.OpenSSL.pas` do upstream **ainda não** expõem a cadeia de overloads `SetCACertificate(File)` nem o `SetVerifyPeer(Boolean)` virtual abstract que o Provider chama quando `THorseCrossSocketConfig.SSLVerifyPeer = True`. Um PR upstream está em preparação; até ele entrar, usuários de mTLS precisam ou aplicar os dois patches `Net.CrossSslSocket.*` manualmente sobre o upstream **ou** usar o release pré-empacotado do fork (próximo item).
- **Alternativa suportada — release do fork** [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3): clone único, inclui o CnPack **e já entrega os dois patches mTLS de `Net.CrossSslSocket.*` aplicados** — `SetCACertificate(File)` + `SetVerifyPeer(Boolean)` ficam imediatamente disponíveis. Use quando `SSLVerifyPeer = True` ou quando preferir a conveniência de uma única dependência em vez de acompanhar o upstream diretamente. Trade-off: o fork fica para trás do histórico de commits do upstream entre os syncs (tipicamente <24h via o [workflow automatizado de sync](https://github.com/freitasjca/Delphi-Cross-Socket/actions)).
- Veja [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket#readme) para o runbook completo das três opções de instalação e a matriz atual de teste por versão.

### mORMot2 — opcional, entre compiladores

- Compatível com **Delphi 7 até 12.3 Athens** — a faixa de compilador mais ampla entre todos os Providers, graças ao suporte legado de longa data do próprio mORMot. As unidades adapter do lado Horse carregam guards `{$IF CompilerVersion >= 32.0}` (Delphi 10.2+) em um número pequeno de fronteiras `Int64`/`Integer`; XE7+ no restante.
- Requer **FPC 3.2.0 ou superior** em builds FPC.
- Plataformas testadas: Windows x86 / x64, Linux x64, macOS ARM64.
- Substitui Indy (Delphi) e `fphttpserver` (FPC) pelo `THttpServer` do [mORMot2](https://github.com/synopse/mORMot2) (IOCP / epoll) — Pascal puro, sem dependências de bibliotecas C compiladas para HTTP padrão. No Windows, troque `THttpServer` por `THttpApiServer` para obter **HTTP em modo kernel via http.sys** sem nenhuma mudança de código.
- O mORMot2 **não** está disponível via `boss install` — clone [synopse/mORMot2](https://github.com/synopse/mORMot2) diretamente e adicione as entradas de search-path documentadas em [`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot#readme).
- No Delphi o build também exige os **blobs `.obj` pré-compilados** do `mormot2static.7z` ([último release do mORMot2](https://github.com/synopse/mORMot2/releases/latest)) extraídos em `mORMot2\static\delphi\`. Sem eles o linker falha com `E1026 File not found: '..\..\static\delphi\zlibdeflate.obj'`. As variantes FPC usam arquivos `.o` em `mORMot2\static\<target>` configurados via os paths `-Fl` do projeto.
- Veja [`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot#readme) para a matriz atual de teste por versão e o record de configuração (`THorseMormotConfig`).

## Tipos de aplicação host-managed

Apache / ISAPI / CGI / FastCGI **não usam um Provider self-hosted** — nem Indy, nem `fphttpserver`, nem CrossSocket, nem mORMot2 está envolvido. O processo host (Apache httpd, IIS, o webserver) é dono do socket e entrega a requisição ao Horse via subclasses de `Web.HTTPApp` (Delphi) ou `fpFCGI` / `fpHTTP` (FPC).

- **Módulo Apache** — Delphi (`Web.HTTPD24Impl`, `Web.ApacheApp`). Construa o `.so` / `.dll` com a arquitetura correspondente à do Apache.
- **Extensão ISAPI** — só Delphi (`Web.Win.ISAPIApp`). Windows + IIS. Combine a arquitetura com o pool de aplicações do IIS (pool 32-bit → build Win32).
- **CGI** — Delphi (`Web.CGIApp`) e FPC. Multiplataforma; um processo por requisição.
- **FastCGI** — FPC (`fpFCGI`). Processo persistente; conversa com o webserver via Unix socket ou TCP. FCGI no Delphi requer biblioteca de terceiros e não faz parte dos providers do Horse.

## Guards de versão de compilador no código-fonte

O Horse usa alguns guards defensivos. Se você contribui patches:

- `{$IF DEFINED(FPC)}` — código somente-FPC (RTL diferente, sintaxe de generics diferente).
- `{$IF CompilerVersion >= 32.0}` — Delphi 10.2 Tokyo introduziu retorno `Int64` em `TWebRequest.GetIntegerVariable` / `TWebResponse.SetIntegerVariable`. Qualquer override desses precisa do guard.
- `{$IF CompilerVersion >= 33.0}` — Delphi 10.3 Rio introduziu `var` inline. O core do Horse evita `var` inline para continuar compilando no XE7.

Para cada alteração, **faça a compilação de teste** tanto no **Delphi** (ex.: `dcc32`, `dcc64`) quanto no **FPC** (`fpc`). Procedimentos anônimos, generics e tipos Web/HTTPApp são as diferenças que mais causam problemas na compilação cruzada entre Delphi e FPC..

## Reportando um bug específico de versão

Ao abrir uma issue:

1. Inclua a **versão exata** de Delphi / FPC e a **plataforma-alvo** (Win64, Linux64, …).
2. Inclua o(s) **define(s) do provider** ativo(s) no projeto.
3. Diga se o bug é reproduzível no sample do provider correspondente em `samples/`.

A triagem é normalmente rápida quando plataforma e provider estão definidos.

## Veja também

- [Primeiros passos](./getting-started.pt-BR.md) — caminhos de instalação por IDE.
- [Providers e Tipos de aplicação](./providers.pt-BR.md) — o modelo de dois eixos: quais combinações Provider × Tipo-de-aplicação existem, e quais units cada uma usa.
