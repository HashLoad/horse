# Providers e Tipos de aplicação

*Leia em [English](./providers.md) ou [Português (BR)](./providers.pt-BR.md).*

O Horse separa duas escolhas arquiteturais que costumam ser confundidas:

1. **Provider** — o transporte HTTP que é dono do socket e parseia as requisições. Indy é o padrão; CrossSocket e mORMot2 são alternativas assíncronas opcionais; provedores futuros (nghttp2, …) seguirão o mesmo padrão.
2. **Tipo de aplicação** — como o binário Delphi/FPC é empacotado e iniciado: um executável console, um serviço Windows, um app VCL desktop, um módulo Apache, uma extensão IIS, e assim por diante.

Esses dois eixos são *conceitualmente* ortogonais. No build atual do Horse as duas escolhas são codificadas no mesmo conjunto de Conditional Defines mutuamente exclusivas, mas a documentação abaixo mantém os conceitos separados para que o modelo mental fique limpo.

O código da aplicação é portável entre todas as combinações — trocar é normalmente uma mudança de define.

---

## 1. O que é um Provider?

Um Provider é a camada entre o Horse e a rede. Ele:

1. É dono do socket de escuta (ou, para tipos de aplicação host-managed, é invocado pelo host).
2. Parseia uma requisição HTTP entrante num `TWebRequest` (Indy) ou numa representação interna de shadow fields (CrossSocket, providers futuros).
3. Chama `THorse.Execute(Req, Res)`, que roda a cadeia de middleware registrada e as rotas.
4. Serializa a resposta de volta para o cliente.

As mesmas rotas, middlewares e API `THorseRequest` / `THorseResponse` do Horse rodam sob qualquer Provider. Você não reescreve handlers ao trocar de transporte.

## 2. Catálogo de Providers

O **Provider padrão depende do compilador**:

- No **Delphi** (para os Tipos de aplicação Console / VCL / Daemon) o padrão é **Indy** (`TIdHTTPServer` via `IdHTTPWebBrokerBridge`).
- No **FPC** (para os Tipos de aplicação Daemon / HTTPApplication / LCL) o padrão é a biblioteca **`fphttpserver`** do FreePascal.
- O Provider **CrossSocket** opcional substitui *ambos* os padrões por um transporte assíncrono entre compiladores.

| Provider | Define de compilação | Status | Delphi | Lazarus |
|---|---|---|:---:|:---:|
| **Indy** _(padrão Delphi para self-hosted)_ | _(nenhum)_ | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;n/a |
| **`fphttpserver`** _(padrão FPC para self-hosted)_ | _(nenhum)_ | &nbsp;&nbsp;&nbsp;n/a | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[horse-provider-crosssocket](https://github.com/freitasjca/horse-provider-crosssocket)** | `HORSE_CROSSSOCKET` | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[horse-provider-mormot](https://github.com/freitasjca/horse-provider-mormot)** | `HORSE_PROVIDER_MORMOT` | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| **[HTTP.sys](./httpsys.pt-BR.md)** | `HORSE_PROVIDER_HTTPSYS` | Opcional, embutido (modo kernel Windows) | ✔ | ✔ |
| **[epoll](./epoll.pt-BR.md)** | `HORSE_PROVIDER_EPOLL` | Opcional, embutido (event loop assíncrono Linux) | ✔ | ✔ |
| **horse-provider-ics** | `HORSE_PROVIDER_ICS` | Opcional, pacote externo (Delphi: Windows + Linux64/macOS) | ? | ? |

> **Qual biblioteca faz o trabalho de HTTP, por Tipo de aplicação?** Esta é a pergunta-chave — e a resposta *nem sempre* é Indy. A abstração unificadora em todas as linhas é `Web.HTTPApp.TWebRequest` no Delphi ou `fpHTTP.TRequest` no FPC; abaixo disso, a biblioteca concreta difere.
>
> | Tipo de aplicação | Compilador | Biblioteca de transporte | Indy? |
> |---|---|---|:---:|
> | Console / VCL / Daemon | Delphi | **Indy** (`TIdHTTPServer` + `IdHTTPWebBrokerBridge`) | ✔ |
> | Daemon / HTTPApplication / LCL | FPC | **`fphttpserver`** | ✘ |
> | Qualquer self-hosted + `HORSE_CROSSSOCKET` | Qualquer um | **`Delphi-Cross-Socket`** | ✘ |
> | Qualquer self-hosted + `HORSE_PROVIDER_MORMOT` | Qualquer um | **`mORMot2`** (`THttpServer` / `THttpApiServer`) | ✘ |
> | Qualquer self-hosted + `HORSE_PROVIDER_HTTPSYS` | Qualquer um | **`HTTP.sys`** (Driver de Kernel do Windows) | ✘ |
> | Qualquer self-hosted + `HORSE_PROVIDER_EPOLL` | Qualquer um | **`epoll`** (API epoll nativa do Linux) | ✘ |
> | Qualquer Self-hosted + `HORSE_PROVIDER_ICS` | Delphi (Windows / Linux64 / macOS) | **`OverbyteICS`** (`THttpServer` / `TSslHttpServer`) | ✘ |
> | Módulo Apache | Qualquer um | **Apache httpd** (via `Web.HTTPApp.TApacheRequest` / `mod_horse`) | ✘ |
> | ISAPI | Delphi | **IIS** (via `Web.HTTPApp.TISAPIRequest`) | ✘ |
> | CGI | Delphi | **CGI runner do webserver** (via `Web.HTTPApp.TCGIRequest`) | ✘ |
> | FastCGI | FPC | Biblioteca **`fpFCGI`** (conversa com o webserver) | ✘ |

### Indy (padrão Delphi para self-hosted)

O transporte padrão no Delphi quando nenhum define `HORSE_*` é configurado e o Tipo de aplicação é Console, VCL ou Daemon. O Indy vem dentro do repositório do Horse — não precisa de `boss install` extra. Ele usa um modelo **thread-por-conexão**: uma thread OS por cliente aceito, gerenciada pelo pool de threads do Indy. A unit `IdHTTPWebBrokerBridge` faz a ponte entre o `TIdHTTPServer` do Indy e a abstração `Web.HTTPApp.TWebRequest` que o middleware do Horse enxerga.

**Propriedades:**
- Multiplataforma (Windows, Linux, macOS) — o Indy faz o trabalho pesado.
- Deploy trivial: copia o executável, executa.
- SSL via DLLs OpenSSL colocadas ao lado do binário.
- Teto de escala: tipicamente algumas centenas a ~1 000 conexões concorrentes antes de exaurir o pool de threads ou a pressão do scheduler aparecer.

**Escolha este quando:** você tem no máximo algumas centenas de clientes concorrentes, deploy é "só rodar o binário", e não precisa de long-polling / SSE / WebSockets em escala.

### `fphttpserver` (padrão FPC para self-hosted)

Em builds FPC (Lazarus), o Provider self-hosted padrão é a biblioteca **`fphttpserver`** do FreePascal — não o Indy. As provider units FPC (`Horse.Provider.FPC.Daemon`, `Horse.Provider.FPC.HTTPApplication`, `Horse.Provider.FPC.LCL`) fazem `uses fphttpserver, fpHTTP, httpdefs` e apresentam requisições entrantes como `TRequest` / `TResponse` do `HTTPDefs`.

**Propriedades:**
- Mesmo modelo thread-por-conexão do Indy, mas usando a RTL do FreePascal.
- Multiplataforma nos alvos FPC (Linux, Windows, macOS, BSD).
- Sem dependência do Indy — mantém builds FPC livres do empacotamento de DLLs do OpenSSL.
- SSL via OpenSSL através de handlers `Synapse` / `fpHTTPClient` (depende do provider).

**Escolha este quando:** você está no FPC e quer o build FPC-nativo mais leve possível, sem pacotes extras.

### CrossSocket (opcional)

[`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket) substitui o transporte Indy pelo [Delphi-Cross-Socket](https://github.com/winddriver/Delphi-Cross-Socket): uma biblioteca de E/S assíncrona que usa **IOCP no Windows, epoll no Linux e kqueue no macOS** — as mesmas primitivas que nginx e Node.js usam.

```sh
boss install horse-provider-crosssocket
```

Nos Conditional Defines do projeto: `HORSE_CROSSSOCKET`. Seu código fica o mesmo.

**O que muda vs. Indy:**

| | Indy | CrossSocket |
|---|---|---|
| Concorrência | Uma thread por conexão | Pool fixo de threads IO (`CPUCount*2+1`, ex.: 9 num 4-core, 17 num 8-core) |
| Custo de keep-alive ocioso | Uma thread por conexão ociosa | Um handle epoll/IOCP — desprezível |
| Alocação por requisição | Novo `THorseRequest`/`THorseResponse` | Pool pré-aquecido (32 contextos, escala até 512) |
| Teto de escala | ~1 000 concorrentes em hardware comum | 10 000+ concorrentes no mesmo hardware |
| Deploy Linux | Indy funciona mas não é sua plataforma primária | Primeira classe — epoll é a primitiva nativa de async no Linux |
| Validação pré-pipeline da requisição | Nenhuma | Tamanho de URL, limites de header, proteções contra smuggling, allowlist de método |
| Object pool, hot path sem alocação | Não | Sim |

**Escolha o CrossSocket quando:**
- Espera mais que algumas centenas de clientes concorrentes.
- Faz deploy em Linux (especialmente em containers / Docker).
- Serve long-polling, server-sent events ou muitas conexões keep-alive ociosas.
- Quer proteções de tamanho / smuggling impostas pela camada de transporte.

CrossSocket e Indy são **alternativas drop-in** para o mesmo código Horse. O mesmo middleware (`Horse.CORS`, `Jhonson`, `JWT`, `logger`, etc.) funciona nos dois.

Para configuração (certificados TLS, limites de tamanho de body, número de threads IO, mTLS), veja a [documentação do próprio provider](https://github.com/freitasjca/horse-provider-crosssocket#readme). Um teste de integração TLS unidirecional + mútuo acompanha o `tests/` do provider (`HorseCSTLSTestServer` / `…Client`, veja `tests/TLS-TESTS.md`).

### Instalação 

O `horse-provider-crosssocket` instala o Delphi-Cross-Socket via Boss. 
 
Se instalar manualmente:

1. Clone o [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket).
2. Clone o Delphi-Cross-Socket — **duas opções**:
   - **Recomendado:** upstream [`winddriver/Delphi-Cross-Socket`](https://github.com/winddriver/Delphi-Cross-Socket). Acompanha o ritmo de releases do mantenedor original e recebe as melhorias upstream assim que entram.
   - **Alternativa suportada:** o release do fork [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3), que embute o CnPack como subárvore vendored e adiciona as APIs de mTLS server-mode (`SetCACertificate(File)` + `SetVerifyPeer(Boolean)`). Um clone em vez de dois, à custa de ficar para trás dos commits upstream entre os syncs do fork. Escolha esta opção se você precisa de mTLS no servidor ou prefere a conveniência de uma dependência única.
3. **Somente** no caminho upstream, clone também o [`cnpack/cnvcl`](https://github.com/cnpack/cnvcl) e adicione `Source/Common` + `Source/Crypto` ao search path. O fork já embute esses arquivos.
4. Adicione os search paths resultantes ao campo Search-path do seu projeto — veja o [README do `horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket#installation) para o runbook completo das três opções (upstream, fork, e Boss-só-para-o-Horse).

> **Por que "alternativa suportada" e não "deprecated"?** O fork continua ativamente mantido para usuários que querem a conveniência do CnPack embutido ou precisam de mTLS hoje. O trade-off é direto: o upstream `winddriver/Delphi-Cross-Socket` tem cadência de commits mais alta do que qualquer fork consegue acompanhar, então o fork inevitavelmente fica para trás. Três bug fixes que antes eram exclusivos do fork (`PATCH-IOCP-1` cascade de shutdown, hang do parser para respostas sem body, e o nil-guard em `_OnBodyEnd`) já foram mergeados no upstream em 2026-Q2 — só as adições de mTLS continuam genuinamente exclusivas do fork. Um PR upstream para o mTLS está em preparação; uma vez mergeado, o único valor restante do fork será a conveniência do CnPack embutido.

### mORMot2 (opcional)

[`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot) substitui o transporte Indy pelo [mORMot2](https://github.com/synopse/mORMot2): uma biblioteca madura e de alto desempenho que usa **IOCP no Windows e epoll no Linux** — as mesmas primitivas do kernel que o CrossSocket usa. O mORMot2 gerencia seu próprio pool fixo de threads (padrão 32 threads), portanto nenhum `THorseWorkerPool` é necessário.

```sh
boss install horse-provider-mormot
```

Nos Conditional Defines do projeto: `HORSE_PROVIDER_MORMOT`. Seu código fica o mesmo.

**Três back-ends de servidor.** O provider pode hospedar qualquer um dos servidores HTTP do mORMot2, selecionado por `THorseMormotConfig.ServerKind`: `mskThreadPool` (`THttpServer`, o padrão — uma thread por requisição concorrente), `mskAsync` (`THttpAsyncServer`, um laço de eventos não bloqueante IOCP/epoll/kqueue que escala além de uma thread por requisição) ou `mskHttpApi` (`THttpApiServer`, HTTP em modo kernel via **http.sys** do Windows — somente Windows). Troque em tempo de execução (`Cfg.ServerKind := mskAsync` antes de `THorse.ListenWithConfig`) ou defina um define de projeto (`HORSE_MORMOT_ASYNC`, ou `HORSE_MORMOT_HTTPAPI` no Windows) para alterar o padrão. Sem nenhum deles, o padrão continua sendo `THttpServer` — comportamento inalterado. O `mskHttpApi` registra `http://+:<porta>/` no http.sys, o que exige direitos de Administrador ou um `netsh http add urlacl` executado uma única vez. Veja a [documentação do próprio provider](https://github.com/freitasjca/horse-provider-mormot#readme) para detalhes.

**O que muda vs. Indy:**

| | Indy | mORMot2 |
|---|---|---|
| Concorrência | Uma thread por conexão | Pool fixo de threads (padrão 32, configurável) |
| Custo de keep-alive ocioso | Uma thread por conexão ociosa | Thread só executa quando há dados prontos |
| Alocação por requisição | Novo `THorseRequest`/`THorseResponse` | Pool pré-aquecido (32 contextos, escala até 512) |
| Teto de escala | ~1 000 concorrentes em hardware comum | 10 000+ concorrentes no mesmo hardware |
| Suporte de compilador | Delphi XE7+ | Delphi 7 ao 12.3 Athens, FPC 3.2+ |
| http.sys (Windows) | ❌ | ✔ `THttpApiServer` — HTTP no modo kernel, sem alterações de código |
| Dependência externa | Indy (incluso) | mORMot2 (adicionar ao search path) |

**O que muda vs. CrossSocket:**

| | CrossSocket | mORMot2 |
|---|---|---|
| Pool de threads | `THorseWorkerPool` (4–64 threads Horse) | Integrado (padrão 32 threads) |
| Dependência externa | Delphi-Cross-Socket + CnPack | Pascal puro — sem bibliotecas C compiladas para HTTP padrão |
| Suporte a Delphi antigo | Delphi 10.2+ | Delphi 7+ |
| http.sys | ❌ | ✔ |

**Escolha o mORMot2 quando:**
- Precisa de suporte ao Delphi 7 / XE / XE2.
- Quer nenhuma dependência de biblioteca de terceiros para HTTP padrão (Pascal puro).
- Quer HTTP no modo kernel Windows via http.sys (`THttpApiServer`).
- Prefere um codebase comprovado em produção há mais de 15 anos.

**Escolha o CrossSocket quando:**
- Prefere o controle nativo assíncrono via IOCP/epoll.
- Seu projeto já depende do Delphi-Cross-Socket.

Ambos os providers usam IOCP/epoll e um pool de objetos de contexto, portanto a taxa de transferência é comparável sob cargas típicas.

Para configuração (`ServerKind`, `ThreadPool`, `MaxBodyBytes`, `DrainTimeoutMs`, `ServerBanner` e os campos TLS `SSL*` abaixo) e as implementações para cada tipo de aplicação  (VCL, Serviço Windows, daemon Linux), veja a [documentação do próprio provider](https://github.com/freitasjca/horse-provider-mormot#readme).

**HTTPS / TLS.** O `THorseMormotConfig` carrega a mesma superfície TLS dos providers CrossSocket / ICS; o provider constrói um `TNetTlsContext` do mORMot e o passa para `THttpServerSocketGeneric.WaitStarted`:

```pascal
Cfg := THorseMormotConfig.Default;
Cfg.SSLEnabled     := True;
Cfg.SSLCertFile    := 'server.crt';
Cfg.SSLPrivKeyFile := 'server.key';
Cfg.SSLCACertFile  := 'ca.crt';     // TLS mútuo
Cfg.SSLVerifyPeer  := True;          // exigir + verificar o certificado do cliente
THorseProviderMormot.ListenWithConfig(9443, Cfg);
```

O TLS se aplica aos **back-ends de socket** (`mskThreadPool`, `mskAsync`); no `mskHttpApi` o certificado é vinculado no nível do SO (`netsh http add sslcert`), então `SSLEnabled` gera um erro claro ali. Um teste de integração TLS unidirecional + mútuo acompanha o `tests/` do provider (`HorseMormotTLSTestServer` / `…Client`, veja `tests/TLS-TESTS.md`).

> **Instalação do mORMot2** — o mORMot2 não está disponível via `boss install`. Clone [synopse/mORMot2](https://github.com/synopse/mORMot2) e adicione `<mORMot2>/src`, `<mORMot2>/src/core`, `<mORMot2>/src/net` ao search path do compilador.

### ICS (opcional)

[`horse-provider-ics`](https://github.com/freitasjca/horse-provider-ics) substitui o transporte Indy pelo [OverbyteICS](https://wiki.overbyte.eu/wiki/index.php/ICS_Download) (`THttpServer` / `TSslHttpServer`). Seu diferencial é a pilha **OpenSSL 3.x / 4.x** moderna do ICS — **TLS 1.3, SNI, TLS mútuo (mTLS), controles de security level** — sendo a escolha quando você precisa de TLS atualizado no lado servidor. Os sockets do ICS têm afinidade de thread única, então o provider repassa o pipeline a um pool de workers e devolve a resposta à thread do laço de mensagens.

Nas Conditional Defines do seu projeto: `HORSE_PROVIDER_ICS`. Seu código permanece o mesmo.

```pascal
var
  Cfg: THorseICSConfig;
begin
  Cfg := THorseICSConfig.Default;
  Cfg.SSLEnabled     := True;
  Cfg.SSLCertFile    := 'server.pem';
  Cfg.SSLPrivKeyFile := 'server.key';
  Cfg.SSLCAFile      := 'ca.pem';   // TLS mútuo
  Cfg.SSLVerifyPeer  := True;
  THorseProviderICS.ListenWithConfig(9443, Cfg);
end;
```

**Escopo:** **somente Delphi — Windows e POSIX (Linux64 / macOS).** A camada POSIX do ICS (`Ics.Posix.WinTypes` + `Ics.Posix.PXMessages`) fornece o mesmo loop de mensagens `TIcsWndControl` no Linux/macOS, então o marshal-back do worker-pool e o TLS OpenSSL funcionam sem alteração de código; o OpenSSL acompanha como `.so` no Linux. Para um serviço Linux use `HORSE_APPTYPE_DAEMON` + `THorseICSLinuxDaemonApp.Run` (handlers SIGTERM/SIGINT + `Listen` bloqueante). Um port para **Lazarus/FPC não é viável** — a camada POSIX do ICS usa a RTL POSIX do *Delphi* e o ICS desativa o OpenSSL no FPC. Selecionar `HORSE_PROVIDER_ICS` no FPC gera um `FATAL` em tempo de compilação.

#### Instalação

O ICS **não** é instalável via Boss. **Instale o OverbyteICS seguindo as instruções oficiais do ICS:** baixe ou clone o ICS (v9.x) e adicione a pasta `Source/` ao search path do seu projeto. As DLLs do OpenSSL acompanham a distribuição do ICS. Em seguida, clone o [`horse-provider-ics`](https://github.com/freitasjca/horse-provider-ics) e adicione seu `src/` ao search path. Veja a [documentação do próprio provider](https://github.com/freitasjca/horse-provider-ics#readme) para a configuração completa, a suíte de testes de integração A–K e as limitações conhecidas (uploads precisam enviar `Content-Length`; keep-alive está desativado na v1). Um teste TLS unidirecional + mútuo dedicado acompanha (`HorseICSTLSTestServer` / `…Client`, veja `tests/TLS-TESTS.md`) — o teste mais importante para o ICS, cujo diferencial é justamente a pilha OpenSSL.

### HttpSys (opcional, nativo)

Diferente de CrossSocket / mORMot / ICS, o Provider **HttpSys** faz **parte do próprio Horse** — a unit `Horse.Provider.HttpSys` acompanha o framework. Ele se liga diretamente à **HTTP Server API do Windows** (`httpapi.dll`), ou seja, à pilha HTTP em modo kernel **http.sys** que também sustenta o IIS. **Não há biblioteca externa a instalar** nem dependência de Boss — é puro SO.

Nos Conditional Defines do seu projeto: `HORSE_PROVIDER_HTTPSYS`. Seu código permanece o mesmo.

**Abrangência:** **somente Windows** (http.sys é um recurso de kernel do Windows), em **Delphi e FPC/Lazarus**. Selecioná-lo em um alvo não-Windows é erro de compilação. É **mutuamente exclusivo** com os Providers CrossSocket / mORMot / ICS — exatamente um Provider de transporte por build (`Horse.pas` impõe isso com `{$MESSAGE FATAL}`).

**Por que http.sys?** Roteamento de requisições em modo kernel, cache de respostas e a capacidade de **compartilhar a porta 80/443 com o IIS** e outros apps http.sys na mesma máquina; o TLS é configurado no nível do SO (repositório de certificados do Windows) em vez de dentro do seu processo.

#### Instalação e requisitos de execução

Sem instalação — é nativo. Dois requisitos **no nível do SO** decorrem de o http.sys ser global da máquina:

- **Reserva de URL.** Ligar qualquer host diferente de `localhost`, ou uma porta privilegiada, exige uma URL ACL única ou direitos de Administrador:
  ```
  netsh http add urlacl url=http://+:9000/ user=Everyone
  ```
- **HTTPS** é ligado a um certificado no repositório do Windows, não a um arquivo `.pem`:
  ```
  netsh http add sslcert ipport=0.0.0.0:9443 certhash=<thumbprint> appid={<guid>}
  ```

Esse é o mesmo modelo do backend `mskHttpApi` do mORMot (`THttpApiServer`) — ambos usam o http.sys; o HttpSys é a forma autônoma e sem dependências de obtê-lo.

### Providers futuros

A arquitetura de interface híbrida (`IHorseRawRequest` / `IHorseRawResponse`) introduzida pelo CrossSocket torna simples adicionar transportes adicionais. Para nghttp2 veja [`building-a-new-provider.md`](https://github.com/freitasjca/horse-provider-crosssocket/blob/master/doc/building-a-new-provider.md).

---

## 3. Tipos de aplicação — self-hosted

Tipos self-hosted rodam o Provider que você escolheu. O Provider é dono do socket; seu binário é dono do processo.

| Tipo de aplicação | Define de compilação | Delphi | Lazarus |
|---|---|:---:|:---:|
| **Console** _(padrão)_ | _(nenhum)_ | ✔ | ✔ |
| **VCL** | `HORSE_VCL` | ✔ | ❌ |
| **Daemon — Serviço Windows** | `HORSE_DAEMON` | ✔ | n/a |
| **Daemon — daemon Linux (systemd)** | `HORSE_DAEMON` | ✔ | ✔ |
| **LCL** (GUI Lazarus) | `HORSE_LCL` | ❌ | ✔ |
| **HTTPApplication** (FPC) | _(padrão FPC)_ | ❌ | ✔ |

> **Nota** — `HORSE_DAEMON` é um tipo de aplicação guarda-chuva: o binário produzido é um **Serviço Windows** (`Vcl.SvcMgr.TService` + SCM) no Windows e um **daemon** (`signal(SIGTERM)` + systemd) no Linux. "Daemon" é o termo nativo no Unix; o Windows não tem palavra nativa equivalente, então o nome do define é emprestado e usado em modo cross-platform.

### Console — o padrão

O caso mais simples. Escreva um `.dpr` com `{$APPTYPE CONSOLE}`, chame `THorse.Listen(port)`, execute o binário. Funciona em Windows, Linux e macOS. A maioria dos projetos cabe nesse formato.

### VCL — servidor dentro de um app desktop

Útil quando seu app Delphi tem UI e você quer expor uma superfície HTTP (endpoint de controle remoto, painel admin embutido). Com `HORSE_VCL` definido, o Horse inicia o Provider numa thread em background; a thread principal da VCL fica livre para o form / message loop. Hoje só Indy; suporte VCL no CrossSocket é arquiteturalmente possível mas não expressável pelos defines atuais.

### Daemon — Serviço Windows ou daemon Linux (systemd)

`HORSE_DAEMON` é um tipo de aplicação adaptável à plataforma. O mesmo `.dpr` compila para os dois OS-alvo; a diretiva `{$IFDEF MSWINDOWS}` da unit seleciona o caminho de integração com o host em tempo de compilação.

- **Windows:** o Horse conecta `Listen` / `Stop` ao Service Control Manager via `Vcl.SvcMgr.TService`. Combinado com um encapsulador de serviço, você tem integração `sc start MeuServico` / `sc stop MeuServico`.
- **Linux:** o Horse instala handlers POSIX de sinais para `SIGTERM` / `SIGINT` (os sinais padrão de shutdown do systemd) e ignora `SIGPIPE`. Combinado com um arquivo `.service`, `systemctl start/stop MeuServico` funciona da mesma forma.

Mesmo transporte (Indy no Delphi, `fphttpserver` no FPC, ou CrossSocket / mORMot2 quando o Provider correspondente também está definido) por baixo nos dois casos. Veja o [Cheatsheet de Deploy](./deployment.pt-BR.md) para os dois templates de ciclo de vida lado a lado.

### LCL — aplicação Lazarus com GUI

A contraparte Lazarus do VCL. Com `HORSE_LCL` definido, o Provider roda junto com o message loop do Lazarus no FPC. Usado quando um app desktop Lazarus precisa expor HTTP.

### HTTPApplication — aplicação HTTP nativa FPC

Um tipo de aplicação standalone só-FPC que usa o scaffolding de servidor `fpHTTP` do FreePascal. Usado quando você quer um build puro-FPC integrado às ferramentas Lazarus sem envolver Indy.

---

## 4. Tipos de aplicação host-managed

Quando o tipo de aplicação é **host-managed**, o processo host (Apache httpd, IIS, o webserver) é dono do socket e entrega uma requisição pré-parseada ao seu código Delphi/FPC. Os Providers da §2 **não são usados de jeito nenhum** — **nem Indy, nem `fphttpserver`, nem CrossSocket**. A abstração de requisição vem de:

- **Delphi**: subclasses embutidas do `Web.HTTPApp` — `TApacheRequest` (módulo Apache), `TISAPIRequest` (IIS), `TCGIRequest` (CGI). O Horse usa `Web.WebBroker, Web.ApacheApp / Web.Win.ISAPIApp / Web.CGIApp` conforme o caso.
- **FPC**: biblioteca `fpFCGI` do FreePascal para FastCGI (o processo FCGI escuta num socket para o webserver, e depois dispatcha via `fpHTTP`).

O host *é* o transporte. O middleware do Horse continua enxergando o mesmo `TWebRequest` / `TRequest` de sempre — essa é a abstração unificadora.

| Tipo de aplicação | Define de compilação | Artefato de build | Hospedado por | Delphi | Lazarus |
|---|---|---|---|:---:|:---:|
| **Módulo Apache** | `HORSE_APACHE` | Módulo Apache `.so` / `.dll` | Apache httpd via `mod_horse` | ✔ | ✔ |
| **ISAPI** | `HORSE_ISAPI` | Extensão ISAPI `.dll` | IIS | ✔ | ❌ |
| **CGI** | `HORSE_CGI` | Executável standalone | Webserver (um processo por requisição) | ✔ | ✔ |
| **FastCGI** | `HORSE_FCGI` | Executável standalone | Webserver (pool FCGI persistente) | ✔ | ✔ |

**Escolha um destes quando:** você já tem deploy Apache / IIS / nginx e quer integrar o Horse sem subir um processo separado. O host trata SSL, gzip, virtual hosting; você foca na aplicação.

**Trade-offs:**
- Ciclo de vida controlado pelo host — sem hooks de shutdown gracioso, sem event loop seu.
- Build / deploy mais envolvido que um executável único.
- O catálogo de Providers acima não se aplica — recursos como E/S assíncrona do CrossSocket, proteções contra request smuggling, ou object pool são arquiteturalmente ausentes porque não há socket para possuir.

---

## 5. Matriz de compatibilidade

Provider × Tipo de aplicação — quais combinações são atualmente expressáveis (e arquiteturalmente compatíveis)? Os Tipos de aplicação só-Delphi ficam à esquerda; os só-FPC na sequência; host-managed à direita.

|  | Console (D) | VCL (D) | Daemon (D) | Daemon (FPC) | LCL (FPC) | HTTPApplication (FPC) | Apache | ISAPI | CGI | FCGI |
|---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| **Indy** _(padrão Delphi)_ | ✔ | ✔ | ✔ | n/a | n/a | n/a | n/a | n/a | n/a | n/a |
| **`fphttpserver`** _(padrão FPC)_ | n/a | n/a | n/a | ✔ | ✔ | ✔ | n/a | n/a | n/a | n/a |
| **CrossSocket** (`HORSE_PROVIDER_CROSSSOCKET`) | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ❌ | ❌ | ❌ | ❌ |
| **mORMot2** (`HORSE_PROVIDER_MORMOT`) | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ❌ | ❌ | ❌ | ❌ |
| **ICS** (`HORSE_PROVIDER_ICS`) _(Delphi; Windows + Linux64/macOS)_ | ✔ | ✔ | ✔ | n/a | n/a | n/a | ❌ | ❌ | ❌ | ❌ |
| **HttpSys** (`HORSE_PROVIDER_HTTPSYS`) _(Windows; nativo)_ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ❌ | ❌ | ❌ | ❌ |
| _Host-managed_ (Apache/ISAPI/CGI/FCGI) | n/a | n/a | n/a | n/a | n/a | n/a | ✔ | ✔ | ✔ | ✔ |

Legenda:
- **✔** — suportado e expressável com os defines atuais. Desde o PATCH-HORSE-2, toda célula CrossSocket × Tipo-de-aplicação é suportada via as units de produto cruzado em `horse-provider-crosssocket` (ex. `Horse.Provider.CrossSocket.VCL`, `…Daemon`, `…FPC.Daemon`, `…FPC.LCL`, `…FPC.HTTPApplication`). O mORMot2 traz o conjunto cruzado equivalente em `horse-provider-mormot`: `Horse.Provider.Mormot` (Console padrão), `…Mormot.VCL`, `…Mormot.Daemon` (Serviço Windows TService + runner POSIX numa única unit), `…Mormot.FPC.Daemon`, `…Mormot.FPC.LCL`, `…Mormot.FPC.HTTPApplication`.
- **❌** — arquiteturalmente impossível. Apache / ISAPI / CGI / FCGI são donos do socket; um transporte self-hosted assíncrono como CrossSocket ou mORMot2 não pode coexistir.
- **n/a** — combinação sem sentido. Indy não roda no FPC; `fphttpserver` não roda no Delphi; tipos host-managed não usam um Provider self-hosted; tipos self-hosted não rodam no ciclo de vida de um host.

O PATCH-HORSE-1 em `Horse.pas` força as células ❌ em tempo de compilação com `{$MESSAGE FATAL}` para que projetos mal configurados falhem rápido em vez de silenciosamente pegarem o caminho errado.

---

## 6. Selecionando seus defines

A seleção acontece em **tempo de compilação** via Project Options → Conditional Defines. O PATCH-HORSE-2 divide os defines em três namespaces explícitos — um por eixo — e a cadeia compõe eles.

### Os três namespaces

| Eixo | Prefixo | Significado |
|---|---|---|
| A · **Provider** | `HORSE_PROVIDER_*` | Biblioteca de transporte HTTP — Indy (padrão Delphi), `fphttpserver` (padrão FPC), CrossSocket, mORMot2, ICS (Delphi/Windows), HttpSys (http.sys do Windows, nativo) |
| B · **Tipo de aplicação** | `HORSE_APPTYPE_*` | Formato de ciclo de vida do binário — Console (padrão), VCL, Daemon, LCL, HTTPApplication |
| C · **Runtime host-managed** | `HORSE_HOST_*` | Webserver é dono do socket — Apache, ISAPI, CGI, FastCGI |

O Eixo C vence outright quando definido (nenhum Provider envolvido). Os Eixos A e B compõem livremente.

### Combinações comuns

| Objetivo | Define(s) a configurar |
|---|---|
| Padrão — Console + Indy (Delphi) | _(nenhum)_ |
| Padrão — HTTPApplication + `fphttpserver` (FPC) | _(nenhum)_ |
| App VCL desktop + Indy | `HORSE_APPTYPE_VCL` |
| Serviço Windows + Indy | `HORSE_APPTYPE_DAEMON` |
| GUI Lazarus + `fphttpserver` | `HORSE_APPTYPE_LCL` |
| Console assíncrono de alta performance + CrossSocket | `HORSE_PROVIDER_CROSSSOCKET` |
| **CrossSocket + VCL** *(novo no PATCH-HORSE-2)* | `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_VCL` |
| **CrossSocket + serviço Windows** *(novo no PATCH-HORSE-2)* | `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_DAEMON` |
| **CrossSocket + daemon Linux (FPC)** *(novo no PATCH-HORSE-2)* | `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_DAEMON` (no FPC) |
| **CrossSocket + Lazarus LCL** *(novo no PATCH-HORSE-2)* | `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_LCL` |
| **mORMot2 Console** | `HORSE_PROVIDER_MORMOT` |
| **mORMot2 + VCL** | `HORSE_PROVIDER_MORMOT` + `HORSE_APPTYPE_VCL` |
| **mORMot2 + serviço Windows** | `HORSE_PROVIDER_MORMOT` + `HORSE_APPTYPE_DAEMON` (no Windows) |
| **mORMot2 + daemon Linux** | `HORSE_PROVIDER_MORMOT` + `HORSE_APPTYPE_DAEMON` (no Linux) |
| **ICS Console** (Windows) | `HORSE_PROVIDER_ICS` |
| **ICS + VCL** (Windows) | `HORSE_PROVIDER_ICS` + `HORSE_APPTYPE_VCL` |
| **ICS + servi�o Windows** | `HORSE_PROVIDER_ICS` + `HORSE_APPTYPE_DAEMON` |
| **HttpSys Console** (Windows) | `HORSE_PROVIDER_HTTPSYS` |
| **HttpSys + servi�o Windows** | `HORSE_PROVIDER_HTTPSYS` + `HORSE_APPTYPE_DAEMON` |
| M�dulo Apache | `HORSE_HOST_APACHE` |
| Extens�o ISAPI no IIS | `HORSE_HOST_ISAPI` |
| CGI simples | `HORSE_HOST_CGI` |
| FastCGI | `HORSE_HOST_FCGI` |

### Aliases legados (compatibilidade com Horse <3.2)

Todo define antigo é traduzido automaticamente pro namespace novo por um bloco de aliases no topo do `Horse.pas` — todo `.dproj` / `.lpi` existente continua compilando sem mudanças.

| Define legado | Define namespaced novo |
|---|---|
| `HORSE_CROSSSOCKET` | `HORSE_PROVIDER_CROSSSOCKET` |
| `HORSE_VCL` | `HORSE_APPTYPE_VCL` |
| `HORSE_DAEMON` | `HORSE_APPTYPE_DAEMON` |
| `HORSE_LCL` | `HORSE_APPTYPE_LCL` |
| `HORSE_APACHE` | `HORSE_HOST_APACHE` |
| `HORSE_ISAPI` | `HORSE_HOST_ISAPI` |
| `HORSE_CGI` | `HORSE_HOST_CGI` |
| `HORSE_FCGI` | `HORSE_HOST_FCGI` |
| `HORSE_NOPROVIDER` | inalterado (escape hatch) |

Projetos novos devem preferir os nomes namespaced — são autodocumentados e tornam o modelo de três eixos visível nos conditional defines do projeto.

O `Horse.pas` resolve a seleção ativa numa cadeia `{$IFDEF}` de duas etapas — host-managed primeiro (Eixo C vence outright), depois composição Provider × Tipo-de-aplicação. **Não há troca em runtime.** Se você quer entregar o mesmo código como binário console e também como módulo Apache, build duas vezes com defines diferentes.

## 7. O que NÃO muda ao trocar

- Suas declarações de rota.
- Os corpos dos seus handlers.
- Seus middlewares.
- A API de `THorseRequest` / `THorseResponse` que seu código usa.
- As versões suportadas de Delphi / FPC (dentro da faixa de cada Provider — veja [Suporte de Compilador](./compiler-support.pt-BR.md)).

O Provider e o Tipo de aplicação são intencionalmente trocáveis. Se você mantém seu código neutro ao transporte (e o Horse incentiva isso), pode testar Indy vs CrossSocket simplesmente trocando o define e recompilando.

---

## 8. Rodando o CrossSocket em cada Tipo de aplicação

O `HORSE_CROSSSOCKET` não pode combinar com `HORSE_VCL` / `HORSE_DAEMON` / `HORSE_LCL` / `HORSE_ISAPI` / `HORSE_APACHE` / `HORSE_CGI` / `HORSE_FCGI` em tempo de compilação (o PATCH-HORSE-1 força isso). Para obter o formato de runtime de cada um desses Tipos de aplicação **usando o CrossSocket como transporte**, defina apenas `HORSE_CROSSSOCKET` e escreva você mesmo a pequena casca específica do formato.

O padrão comum em todos os formatos:

1. **Defina apenas `HORSE_CROSSSOCKET`** — nenhum outro define `HORSE_*` de provider.
2. Escolha o **tipo de projeto** certo para o formato desejado (Console / VCL Forms / Lazarus / Service Application / …).
3. Acione `THorse.Listen` a partir do **evento de ciclo de vida** certo daquele formato.
4. Chame `THorse.StopListen` no shutdown para o CrossSocket processe as requisições ativas (SEC-30) antes do processo ser encerrado.

O `Listen` do CrossSocket escolhe automaticamente entre bloqueante e não-bloqueante a partir do `IsConsole`:

- `IsConsole = True` (binário console) → `Listen` bloqueia a thread chamadora até `StopListen` ser chamado.
- `IsConsole = False` (VCL / LCL / TService / …) → `Listen` inicia as threads de IO e retorna imediatamente; a thread chamadora fica livre pra rodar o message loop da GUI ou o loop de controle do serviço. Chame `StopListen` num handler de teardown.

### 8.1 Console (Delphi)

O caso padrão e mais simples. `THorse.Listen` bloqueia a thread principal; o `StopListen` num handler de Ctrl-C desbloqueia.

```pascal
program MyServer;

{$APPTYPE CONSOLE}                 // → IsConsole = True

uses
  Winapi.Windows, Horse;

function CtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  case dwCtrlType of
    CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT,
    CTRL_SHUTDOWN_EVENT:
      begin
        THorse.StopListen;          // drena → Listen retorna
        Result := True;
      end;
  else
    Result := False;
  end;
end;

begin
  SetConsoleCtrlHandler(@CtrlHandler, True);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin Res.Send('pong'); end);

  THorse.Listen(9000);             // bloqueia
end.
```

Define: `HORSE_PROVIDER_CROSSSOCKET` (ou o alias legado `HORSE_CROSSSOCKET`). Tipo de projeto: **Console Application**.

### 8.2 App VCL desktop (Delphi)

Um projeto VCL Forms com servidor HTTP embutido. A thread principal da VCL roda o message loop; o CrossSocket roda nas próprias threads de IO.

```pascal
unit Main.Form;

interface

uses
  Vcl.Forms, System.Classes, Horse;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin Res.Send('pong'); end);

  // IsConsole = False num app VCL → Listen inicia as threads de IO
  // e retorna imediatamente. O message loop da VCL mantém o form vivo.
  THorse.Listen(9000);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  THorse.StopListen;               // drenagem graciosa antes do form fechar
end;

end.
```

Defines: `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_VCL` (PATCH-HORSE-2). Tipo de projeto: **VCL Forms Application**. **Não** adicione `{$APPTYPE CONSOLE}` no .dpr — isso forçaria `IsConsole = True` e o Listen bloquearia a thread principal, travando a UI.

**Dica:** a classe-base de conveniência opcional `TfrmHorseVCLHost` (em `Horse.Provider.CrossSocket.VCL`) pré-conecta `FormCreate` → `THorse.Listen(Port)` e `FormClose` → `THorse.StopListen`. Herde dela em vez de escrever a wiring acima na mão.

### 8.3 Daemon Linux (Delphi cross-compilado pra Linux)

Um binário Linux standalone supervisionado pelo systemd. O formato daemon é fornecido pelo systemd, não pelo Horse.

```pascal
program MyDaemon;

{$APPTYPE CONSOLE}                 // → IsConsole = True; Listen bloqueia

uses
  {$IFDEF LINUX} Posix.Signal, {$ENDIF}
  Horse;

{$IFDEF LINUX}
procedure HandleSignal(ASignal: Integer); cdecl;
begin
  THorse.StopListen;               // SIGTERM do systemd → drena → sai
end;
{$ENDIF}

begin
  {$IFDEF LINUX}
  signal(SIGTERM, @HandleSignal);
  signal(SIGINT,  @HandleSignal);
  {$ENDIF}

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin Res.Send('pong'); end);

  THorse.Listen(9000);
end.
```

Unit do systemd em `/etc/systemd/system/myhorse.service`:

```ini
[Unit]
Description=Meu Horse Server (CrossSocket)
After=network.target

[Service]
Type=simple
User=horse
WorkingDirectory=/opt/myhorse
ExecStart=/opt/myhorse/MyDaemon
ExecStop=/bin/kill -TERM $MAINPID
Restart=on-failure
RestartSec=5s
LimitNOFILE=65536

[Install]
WantedBy=multi-user.target
```

```sh
sudo systemctl daemon-reload && sudo systemctl enable --now myhorse
```

Defines: `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_DAEMON` (PATCH-HORSE-2). Target do projeto: **Linux64**.

**Dica:** o runner de conveniência opcional `THorseCrossSocketLinuxDaemonApp.Run(@SetupRoutes, Port)` (em `Horse.Provider.CrossSocket.Daemon` — a mesma unit do `THorseCrossSocketService` da §8.4, só que o branch não-Windows) instala handlers `signal(SIGTERM/SIGINT)`, ignora `SIGPIPE` e chama `THorse.Listen` pra você. O corpo inteiro do `program` se resume a uma única chamada.

> **Nota sobre `HORSE_APPTYPE_DAEMON` no Delphi:** o mesmo define significa "Serviço Windows" quando compilando pro Windows e "daemon Linux" quando compilando pro Linux. O `Horse.Provider.CrossSocket.Daemon.pas` carrega ambos os helpers de ciclo de vida (`THorseCrossSocketService` sob `{$IFDEF MSWINDOWS}`, `THorseCrossSocketLinuxDaemonApp` sob `{$ELSE}`). Isso espelha o comportamento multiplataforma do Indy-based `Horse.Provider.Daemon.pas`. "Daemon" é a *intenção* (processo de longa duração supervisionado pelo OS); a maquinaria específica do OS é escolhida pelo target de build.

### 8.4 Serviço Windows (Delphi)

Uma Service Application do Delphi que dirige o CrossSocket. Os hooks de start/stop do Windows SCM chamam `THorse.Listen` / `StopListen` numa worker thread pra que o message pump do SCM continue responsivo.

```pascal
unit MyHorseSvc;

interface

uses
  System.SysUtils, System.Classes, Vcl.SvcMgr, Horse;

type
  TMyHorseService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FListenerThread: TThread;
  end;

var
  MyHorseService: TMyHorseService;

implementation

{$R *.dfm}

procedure TMyHorseService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin Res.Send('pong'); end);

  // IsConsole = False num app de serviço → Listen retorna imediatamente
  // e o CrossSocket roda nas próprias threads de IO. Mas também precisamos
  // que a chamada do *serviço* seja não-bloqueante pra que o SCM receba o
  // ack — então rodamos o Listen numa thread dedicada que possuímos.
  FListenerThread := TThread.CreateAnonymousThread(
    procedure begin THorse.Listen(9000); end);
  FListenerThread.FreeOnTerminate := False;
  FListenerThread.Start;

  Started := True;
end;

procedure TMyHorseService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  THorse.StopListen;                // drena
  if Assigned(FListenerThread) then
  begin
    FListenerThread.WaitFor;
    FreeAndNil(FListenerThread);
  end;
  Stopped := True;
end;

end.
```

Defines: `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_DAEMON` (PATCH-HORSE-2). Tipo de projeto: **Service Application** (File → New → Other → Service Application).

**Dica:** a classe-base de conveniência opcional `THorseCrossSocketService` (em `Horse.Provider.CrossSocket.Daemon`) pré-conecta `ServiceStart` pra spawnar a worker thread do `Listen` e `ServiceStop` pra drenar + dar join limpo. Herde dela em vez de escrever a wiring acima na mão.

Instale / desinstale pelos verbos padrão do SCM. **Rode a partir de um Prompt de Comando elevado (Administrador)** — `/install`, `/uninstall`, `sc start` e `sc stop` passam todos pelo Service Control Manager, que rejeita tokens não-elevados com `EOSError ... Code: 5. Access is denied.` mesmo se o usuário tá no grupo Administradores (o UAC filtra o token).

```bat
MyHorseServer.exe /install
sc start MyHorseService
sc stop  MyHorseService
MyHorseServer.exe /uninstall
```

Alternativa mais simples se não quiser escrever o wrapper TService: construa um binário console (§8.1) e registre como serviço Windows via [NSSM](https://nssm.cc/). O NSSM envia `Ctrl+Break` no stop; o `CtrlHandler` da §8.1 pega e chama `StopListen`.

### 8.5 Daemon Linux (FPC / Lazarus)

Mesmo formato que §8.3 mas construído com FPC. O Provider padrão FPC num build sem define é o `fphttpserver`; definir `HORSE_CROSSSOCKET` troca pelo CrossSocket e a camada HTTP do binário fica assíncrona.

```pascal
program MyDaemon;

{$MODE DELPHI}{$H+}
{$APPTYPE CONSOLE}                 // → IsConsole = True; Listen bloqueia

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  SysUtils, Horse;

{$IFDEF UNIX}
procedure HandleSignal(ASignal: cint); cdecl;
begin
  THorse.StopListen;
end;
{$ENDIF}

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  {$IFDEF UNIX}
  fpSignal(SIGTERM, @HandleSignal);
  fpSignal(SIGINT,  @HandleSignal);
  {$ENDIF}

  THorse.Get('/ping', @GetPing);   // note o @ — o FPC exige
  THorse.Listen(9000);
end.
```

Defines: `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_DAEMON` (PATCH-HORSE-2). Compile com `fpc` ou `lazbuild`, target Linux.

**Dica:** o runner de conveniência opcional `THorseCrossSocketDaemonApp.Run(@SetupRoutes, Port)` (em `Horse.Provider.CrossSocket.FPC.Daemon`) instala os handlers `fpSignal(SIGTERM)` / `SIGINT` e chama o `THorse.Listen` pra você. O corpo inteiro do `program` colapsa pra uma linha: `THorseCrossSocketDaemonApp.Run(@SetupRoutes, 9000);`.

A unit do systemd é idêntica à §8.3.

### 8.6 App LCL Lazarus desktop (FPC)

Contraparte Lazarus da §8.2. App Lazarus com GUI embutindo um servidor HTTP CrossSocket.

```pascal
unit Main.Form;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Forms, Horse;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  end;

implementation

{$R *.lfm}

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  THorse.Get('/ping', @GetPing);
  THorse.Listen(9000);             // IsConsole = False → retorna imediatamente
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  THorse.StopListen;
end;

end.
```

Defines: `HORSE_PROVIDER_CROSSSOCKET` + `HORSE_APPTYPE_LCL` (PATCH-HORSE-2). Tipo de projeto: **Lazarus Application** (um projeto GUI normal). Não use `{$APPTYPE CONSOLE}`.

**Dica:** a classe-base de conveniência opcional `TfrmHorseLCLHost` (em `Horse.Provider.CrossSocket.FPC.LCL`) é o espelho Lazarus do `TfrmHorseVCLHost` — mesmas `Port` / `OnHorseListen` / `FormCreate`/`FormClose` pré-conectados.

### 8.7 FPC HTTPApplication

O formato HTTPApplication do FPC é um executável FPC standalone que tradicionalmente usa `fphttpapp.Application.Run` pra ser dono do main loop. Com o CrossSocket, o loop pertence às threads de IO do CrossSocket — então a estrutura colapsa pro mesmo formato da §8.5 (um binário FPC formato console chamando `THorse.Listen`).

```pascal
program MyHttpApp;

{$MODE DELPHI}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  SysUtils, Horse;

{$IFDEF UNIX}
procedure HandleSignal(ASignal: cint); cdecl;
begin
  THorse.StopListen;
end;
{$ENDIF}

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  {$IFDEF UNIX}
  fpSignal(SIGTERM, @HandleSignal);
  fpSignal(SIGINT,  @HandleSignal);
  {$ENDIF}

  THorse.Get('/ping', @GetPing);
  THorse.Listen(9000);             // CrossSocket é dono do loop
end.
```

Define: `HORSE_PROVIDER_CROSSSOCKET` (sem `HORSE_APPTYPE_*` — HTTPApplication é o padrão FPC self-hosted; o PATCH-HORSE-2 resolve a cadeia pra unit console-shape existente). A classe de conveniência `THorseCrossSocketHTTPApp.Run(@SetupRoutes, Port)` (em `Horse.Provider.CrossSocket.FPC.HTTPApplication`) delega pro mesmo runner com signal handler da §8.5.

Se você realmente precisa manter o ciclo de vida do `fphttpapp.TFPHTTPApplication` (porque alguma biblioteca da qual depende espera isso), instancie ele mas **nunca** chame `Application.Run` — deixe o `THorse.Listen` ser dono do main loop. Dois event loops concorrentes no mesmo processo é o modo de falha que o PATCH-HORSE-1 previne explicitamente em tempo de compilação.

---

### Referência rápida

| Tipo de aplicação | `{$APPTYPE CONSOLE}` | `IsConsole` | `Listen` bloqueia? | Onde chamar `Listen` | Onde chamar `StopListen` |
|---|:---:|:---:|:---:|---|---|
| Console (Delphi) | ✔ | True  | Sim | `begin … end.`            | `SetConsoleCtrlHandler` |
| VCL (Delphi) | ❌ | False | Não | `FormCreate`              | `FormClose` |
| Daemon Linux (Delphi) | ✔ | True  | Sim | `begin … end.`            | handler POSIX `SIGTERM` |
| Serviço Windows (Delphi) | ❌ | False | Não | `TService.ServiceStart` numa worker thread | `TService.ServiceStop` |
| Daemon Linux (FPC) | ✔ | True  | Sim | `begin … end.`            | `fpSignal(SIGTERM, …)` |
| LCL (FPC / Lazarus) | ❌ | False | Não | `FormCreate`              | `FormClose` |
| HTTPApplication FPC | ✔ | True  | Sim | `begin … end.`            | `fpSignal(SIGTERM, …)` |

Todos os sete casos compartilham a mesma configuração em tempo de compilação: **defina apenas `HORSE_CROSSSOCKET`**, nenhum outro define `HORSE_*` de provider.

---

## 9. Rodando o mORMot2 em cada Tipo de aplicação

O mesmo padrão de `IsConsole` / ciclo de vida da §8 se aplica ao mORMot2 — as units do provider (`Horse.Provider.Mormot.*`) envolvem o `THttpServer` do mORMot em vez do `TCrossHttpServer`, mas expõem o mesmo contrato `Listen` / `StopListen`. A receita de quatro passos é idêntica:

1. **Defina apenas `HORSE_PROVIDER_MORMOT`** — nenhum outro define `HORSE_*` de provider.
2. Escolha o **tipo de projeto** correto para o shape desejado (Console / VCL Forms / Lazarus / Service Application / …).
3. Dispare `THorse.Listen` a partir do **evento de ciclo de vida** correto para esse shape.
4. Chame `THorse.StopListen` no shutdown para que o contador de requisições ativas do mORMot drene as requisições em voo antes do processo terminar.

O `Listen` do mORMot honra o mesmo switch `IsConsole` que o CrossSocket: `True` → bloqueia a thread chamadora até `StopListen`; `False` → retorna imediatamente após `THttpServer.WaitStarted`, deixando o loop GUI / serviço / LCL controlar a thread principal.

### 9.1 Lado a lado com a §8 — as únicas diferenças

Todo esqueleto de código de §8.1 a §8.7 se porta para o mORMot2 com **duas substituições**:

| Em §8 (CrossSocket) | No equivalente mORMot2 |
|---|---|
| Define `HORSE_PROVIDER_CROSSSOCKET` | Define `HORSE_PROVIDER_MORMOT` |
| Helper `Horse.Provider.CrossSocket.{VCL,Daemon,FPC.Daemon,FPC.LCL,FPC.HTTPApplication}` | `Horse.Provider.Mormot.{VCL,Daemon,FPC.Daemon,FPC.LCL,FPC.HTTPApplication}` |

Os helpers de conveniência têm correspondência um-para-um — a tabela abaixo é o inventário completo:

| Shape | Helper CrossSocket | Helper mORMot2 |
|---|---|---|
| Console (§8.1) | _(nenhum — uso direto do provider)_ | _(nenhum — uso direto do provider)_ |
| VCL desktop (§8.2) | `TfrmHorseVCLHost` em `Horse.Provider.CrossSocket.VCL` | `TfrmHorseMormotVCLHost` em `Horse.Provider.Mormot.VCL` |
| Daemon Linux, Delphi (§8.3) | `THorseCrossSocketLinuxDaemonApp.Run` em `Horse.Provider.CrossSocket.Daemon` | `THorseMormotLinuxDaemonApp.Run` em `Horse.Provider.Mormot.Daemon` |
| Serviço Windows (§8.4) | `THorseCrossSocketService` (base `TService`) em `Horse.Provider.CrossSocket.Daemon` | `THorseMormotService` (base `TService`) em `Horse.Provider.Mormot.Daemon` |
| Daemon Linux, FPC (§8.5) | `THorseCrossSocketDaemonApp.Run` em `Horse.Provider.CrossSocket.FPC.Daemon` | `THorseMormotFPCDaemonApp.Run` em `Horse.Provider.Mormot.FPC.Daemon` |
| LCL Lazarus (§8.6) | `TfrmHorseLCLHost` em `Horse.Provider.CrossSocket.FPC.LCL` | `TfrmHorseMormotLCLHost` em `Horse.Provider.Mormot.FPC.LCL` |
| FPC HTTPApplication (§8.7) | `THorseCrossSocketHTTPApp.Run` em `Horse.Provider.CrossSocket.FPC.HTTPApplication` | `THorseMormotHTTPApp.Run` em `Horse.Provider.Mormot.FPC.HTTPApplication` |

A mesma regra `{$IFDEF MSWINDOWS}` / `{$ELSE}` que dá ao `HORSE_APPTYPE_DAEMON` dois significados sob CrossSocket vale também para o mORMot: `Horse.Provider.Mormot.Daemon.pas` traz `THorseMormotService` (TService Windows) sob `{$IFDEF MSWINDOWS}` e `THorseMormotLinuxDaemonApp` (runner POSIX com handler de signal) sob `{$ELSE}`.

### 9.2 Exemplo mínimo — Console (mORMot2)

```pascal
program MyMormotServer;

{$APPTYPE CONSOLE}                 // → IsConsole = True

uses
  Winapi.Windows, Horse;           // resolve THorse → THorseProviderMormot

function CtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  case dwCtrlType of
    CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT,
    CTRL_SHUTDOWN_EVENT:
      begin
        THorse.StopListen;
        Result := True;
      end;
  else
    Result := False;
  end;
end;

begin
  SetConsoleCtrlHandler(@CtrlHandler, True);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin Res.Send('pong'); end);

  THorse.Listen(9000);             // bloqueia
end.
```

Define: `HORSE_PROVIDER_MORMOT`. Tipo de projeto: **Console Application**. Sem runner de conveniência aqui — Console é uso direto do provider, idêntico à §8.1 exceto pela unit resolvida atrás de `THorse`.

### 9.3 Ajustando o transporte mORMot

Diferente do CrossSocket (que usa `THorseWorkerPool` para a pipeline Horse), o mORMot tem seu próprio pool de threads dentro do `THttpServer` — padrão 32 threads. Ajuste via `THorseMormotConfig` e o ponto de entrada tipado:

```pascal
uses
  Horse, Horse.Provider.Mormot, Horse.Provider.Mormot.Config;

var
  Config: THorseMormotConfig;
begin
  Config                := THorseMormotConfig.Default;
  Config.ThreadPool     := 64;                  // pool maior para handlers bloqueantes
  Config.MaxBodyBytes   := 16 * 1024 * 1024;    // 16 MB
  Config.DrainTimeoutMs := 10_000;              // janela de drain gracioso
  Config.ServerBanner   := 'MyServer/1.0';      // enviado como header Server:

  THorseProviderMormot.ListenWithConfig(9000, Config);
end.
```

O record completo de configuração, swap para http.sys (`THttpApiServer`), opções de TLS e defaults de segurança estão documentados em [`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot#readme).

---

## 10. Nota arquitetural

Todo Provider self-hosted implementa a mesma classe base `THorseProviderAbstract`. O Provider:

1. É dono do socket de escuta.
2. Parseia uma requisição entrante.
3. Chama `THorse.Execute(Req, Res)`, que roda a cadeia de middleware registrada e as rotas.
4. Serializa a resposta de volta para o cliente.

Providers não-Indy (CrossSocket e qualquer transporte futuro) usam um pequeno conjunto de interfaces leves — `IHorseRawRequest` / `IHorseRawResponse` — adaptadas para parecer com `TWebRequest` / `TWebResponse` por compatibilidade com middleware. Como resultado, middleware que mexe diretamente em `Req.RawWebRequest.Method` ou `Res.RawWebResponse.SetCustomHeader` (ex. `Horse.CORS`) continua funcionando sem mudanças em todos os Providers.

Se quiser construir um **novo** Provider, o [guia de arquitetura de interface híbrida](https://github.com/freitasjca/horse-provider-crosssocket/blob/master/doc/building-a-new-provider.md) no repo do CrossSocket descreve o padrão.

### 10.1 `TCP_NODELAY` nos Providers self-hosted

Todos os Providers self-hosted desativam o **algoritmo de Nagle** (`TCP_NODELAY`) em cada conexão aceita:

- **Indy** (Console / Daemon / VCL) define `AContext.Binding.UseNagle := False` a partir do `OnConnect` da bridge.
- **CrossSocket** chama `TSocketAPI.SetTcpNoDelay(AConnection.Socket, True)` a partir do `OnConnected` do servidor — no transporte compartilhado, então todo Application type do CrossSocket (Console, VCL, Daemon, variantes FPC) herda o ajuste.
- **mORMot2** já habilita `TCP_NODELAY` por padrão.

**Por quê.** Sem isso, no **loopback do Linux**, o vai-e-volta de requisição/resposta pequenas de uma conexão HTTP keep-alive colide com o temporizador de **ACK atrasado** (~40 ms) do kernel, prendendo a vazão em um piso fixo de ~44 ms/requisição (~2 270 req/s) independentemente da velocidade real do servidor. Desativar o Nagle elimina essa espera. (O loopback do Windows não dispara o problema, mas a opção é inofensiva-a-benéfica lá também — menor latência.)

**Afeta mais do que respostas pequenas?** É uma opção **por conexão**, então se aplica a *toda* requisição, não só às pequenas — mas o efeito é **positivo ou neutro para todos os formatos de resposta, nunca prejudicial**. Muda apenas *quando* os bytes já bufferizados saem do socket, nunca *quais* bytes ou *em que ordem*; o TCP continua sendo um fluxo de bytes confiável e ordenado, e o conteúdo da resposta é idêntico byte a byte.

| Formato da resposta | Efeito |
|---|---|
| Respostas pequenas (APIs JSON, só cabeçalhos, 204) | **Melhora** — remove a espera do ACK atrasado em keep-alive |
| Respostas grandes (downloads de arquivo, corpos grandes) | **Neutro** — o Nagle só segurava um pedaço final sub-MSS; esse flush final agora é imediato |
| POST / uploads | **Neutro** — o Nagle é uma questão de saída; o corpo de entrada não é afetado |

A desvantagem clássica do `TCP_NODELAY` — muitas gravações minúsculas por resposta virando muitos pacotes pequenos — não se aplica aqui: Indy, CrossSocket e Horse bufferizam a resposta inteira e a enviam em uma (ou poucas) gravações. Isso segue a prática universal de servidores HTTP (nginx, Apache, Go `net/http`, mORMot definem `TCP_NODELAY` incondicionalmente).

> A família `fphttpserver` do FPC e os tipos host-managed (Apache / ISAPI / CGI / FastCGI) **não** são alterados: o `fphttpserver` não expõe um hook por conexão adequado, e implantações host-managed não são donas do socket de aceitação HTTP — vale a política de Nagle do próprio servidor web da frente.

### 10.2 Defaults de conexão do provider Indy — `MaxConnections`, `ListenQueue` e `ReadTimeout`

Os providers Indy self-hosted (Console / Daemon / VCL) agora aplicam defaults seguros
quando você não os define explicitamente:

| Propriedade | Default efetivo antigo | Novo default | Por quê |
|---|---|---|---|
| `THorse.MaxConnections` | **32** (limite do pool de módulos do WebBroker em `Web.WebReq.TWebRequestHandler`) | **1024** | O limite de 32 retorna **~60 % de HTTP 500** (`EWebBrokerException: "Maximum number of concurrent connections exceeded"`) sob **keep-alive + middleware de cabeçalhos de resposta + concorrência ≥ ~40**. Elevar o teto do pool de módulos torna o build pronto-para-uso seguro. |
| `THorse.ListenQueue` | **15** (o `IdListenQueueDefault` do Indy) | **511** | 15 é pequeno demais para rajadas de conexões concorrentes → conexões recusadas/descartadas. 511 espelha o backlog do nginx (o SO limita ao `net.core.somaxconn` / `SOMAXCONN`). |
| `THorse.ReadTimeout` | **0** (sem timeout, aguarda indefinidamente) | **0** | O tempo limite em milissegundos para operações de leitura de socket nas conexões aceitas. Definir este valor (ex.: `10000` para 10 segundos) evita que clientes lentos ou travados mantenham sockets e threads do servidor abertos indefinidamente. (Apenas Indy). |

**Comportamento e compatibilidade.** Aplicados **apenas quando o valor é deixado em branco** — se
você já chama `THorse.MaxConnections := N`, `THorse.ListenQueue := N` ou `THorse.ReadTimeout := N` antes do `Listen`, o seu
valor prevalece, sem mudança. O default de `MaxConnections` eleva **somente** o teto do pool de
módulos do WebBroker (a causa dos 500); **não** impõe um limite de conexões TCP do Indy a menos que
você defina um. Aumente os dois para concorrência muito alta (ex.: `1000`+ em c≈500).

```pascal
THorse.MaxConnections := 4096;   // opcional — sobrescreve o default de 1024
THorse.ListenQueue    := 1024;   // opcional — sobrescreve o default de 511
THorse.ReadTimeout    := 10000;  // opcional — define o timeout de leitura de conexões para 10 segundos
THorse.Listen(9000);
```

> **Escopo do provider:** configurações de conexão como `MaxConnections`, `ListenQueue` e `ReadTimeout` aplicam-se apenas aos provedores self-hosted do **Indy** (Console / Daemon / VCL).
> - **Tipos gerenciados por host** (Apache, ISAPI, CGI, FastCGI) delegam o ciclo de vida e os timeouts das conexões para a configuração do próprio servidor web hospedeiro (IIS/Apache).
> - **HttpSys** configura os timeouts de conexão a nível de kernel do Windows (via registro ou `netsh`).
> - **CrossSocket e mORMot** gerenciam seus próprios pools de threads e I/O de rede; os limites de conexão do CrossSocket ficam em `THorseCrossSocketConfig.MaxConnections`, e o mORMot dimensiona seu próprio pool fixo de threads (`THorseMormotConfig.ThreadPool`, default 32). As constantes ficam em `Horse.Provider.Config` (`DEFAULT_MAX_CONNECTIONS`, `DEFAULT_LISTEN_QUEUE`).

#### Como dimensioná-los — e por que **não** derivam da contagem de CPUs

Esses dois valores governam **concorrência de I/O e absorção de rajadas de conexão**, não
processamento — por isso são deliberadamente **defaults fixos, e não função da contagem de CPUs da
máquina.** Dimensione-os pelas variáveis que de fato importam:

- **`ListenQueue` = o backlog de accept TCP do kernel** — um *buffer de rajada* para conexões
  aguardando o `accept()`. Dimensione pela sua **rajada de chegada de conexões de pico**, não pelos
  núcleos; uma máquina mais rápida esvazia a fila *mais rápido* e precisa de *menos* backlog, não
  mais. O SO limita rigidamente ao `net.core.somaxconn` (Linux) / `SOMAXCONN` (Windows) — aumente-o
  para acompanhar se você definir um valor alto.
- **`MaxConnections` = um teto de requisições simultâneas em andamento.** O nível útil é guiado pelo
  **perfil de I/O** dos seus handlers: handlers CPU-bound saturam perto da contagem de núcleos, mas
  handlers **I/O-bound** típicos (banco, HTTP externo, arquivos) rodam proveitosamente muito mais
  requisições simultâneas do que há núcleos, porque a maioria está bloqueada aguardando. É um **teto
  de segurança**, então o limite prático é **RAM / `ulimit -n`** — o Indy é thread-por-conexão
  (~1 MB de stack + um fd por conexão), logo, p.ex., 1024 conexões ≈ até ~1 GB de stacks no pior caso.

> **Por que não escalar com a CPU?** `MaxConnections` é um *teto*; derivá-lo dos núcleos daria às
> máquinas pequenas um teto *baixo* e reintroduziria os 500 de keep-alive justamente nas máquinas
> menos capazes de absorvê-los. A contagem de CPUs pertence aos knobs que contam threads de
> multiplexação/processamento — e esses já escalam sozinhos: as threads de IO do CrossSocket
> (`CPUCount*2+1`) e o `THorseWorkerPool` CPU-bound (4–64 threads). Tetos de conexão e backlogs de
> accept são outra categoria.

**Regra prática:** aumente `MaxConnections` em direção ao seu pico esperado de requisições simultâneas
em andamento (limitado por RAM/fd), e `ListenQueue` em direção ao seu pico esperado de rajada de
accept (limitado pelo `somaxconn`). Deixe nos defaults até um teste de carga indicar o contrário.

## Veja também

- [Primeiros passos](./getting-started.pt-BR.md) — seu primeiro servidor Console + Indy.
- [Ecossistema de Middlewares](./middleware-ecosystem.pt-BR.md) — middleware que funciona em todos os Providers.
- [Suporte de Compilador](./compiler-support.pt-BR.md) — versões Delphi/FPC por Provider e Tipo de aplicação.
- [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket) — documentação própria do Provider CrossSocket assíncrono opcional.
- [`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot) — documentação própria do Provider mORMot2 assíncrono opcional.
- [`horse-provider-ics`](https://github.com/freitasjca/horse-provider-ics) — o Provider OverbyteICS opcional (TLS OpenSSL 3.x/4.x), somente Delphi (Windows + POSIX/Linux64/macOS).
- **HttpSys** (`Horse.Provider.HttpSys`, nativo do Horse) — transporte http.sys nativo do Windows em modo kernel; sem dependência externa, somente Windows.
