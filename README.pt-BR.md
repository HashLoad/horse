<p align="center">
  <a href="https://github.com/HashLoad/horse/blob/master/img/horse.png">
    <img alt="Horse" height="150" src="https://github.com/HashLoad/horse/blob/master/img/horse.png">
  </a>
</p><br>
<p align="center">
  <b>Horse</b> é um <b>framework web</b> para Delphi e Lazarus inspirado no <a href="https://github.com/expressjs/express">Express</a>.<br>
  Projetado para <b>facilitar</b> o desenvolvimento <b>rápido</b>, de forma <b>minimalista</b> e com <b>alta performance</b>.
</p><br>
<p align="center">
  <a href="https://t.me/hashload">
    <img src="https://img.shields.io/badge/telegram-join%20channel-7289DA?style=flat-square">
  </a>
</p>

<p align="center">
  <i>Leia em <a href="./README.md">English</a> ou <a href="./README.pt-BR.md">Português (BR)</a>.</i>
</p>

## ⚙️ Instalação

A instalação é feita pelo comando [`boss install`](https://github.com/HashLoad/boss):

```sh
boss install horse
```

*(Opcional)* Instale o [**Horse Wizard**](https://github.com/HashLoad/horse-wizard) para integração com a IDE.

## ⚡️ Início rápido — Delphi

```delphi
uses Horse;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

## ⚡️ Início rápido — Lazarus

```delphi
{$MODE DELPHI}{$H+}

uses Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Pong');
end;

begin
  THorse.Get('/ping', GetPing);
  THorse.Listen(9000);
end.
```

## 📖 Documentação

O guia completo fica em [`doc/`](./doc/index.pt-BR.md) — um pequeno wiki que complementa a referência rápida abaixo:

| Tópico | Leia |
|---|---|
| Primeiro servidor, instalação, configuração no Delphi/Lazarus | [Primeiros passos](./doc/getting-started.pt-BR.md) |
| Definir rotas, parâmetros de rota, grupos de rotas, query strings | [Roteamento](./doc/routing.pt-BR.md) |
| `THorseRequest` / `THorseResponse` — body, headers, cookies, sessions, status, streaming | [Request e Response](./doc/request-response.pt-BR.md) |
| Usar middleware, ordem de registro, o `Next` proc | [Middleware](./doc/middleware.pt-BR.md) |
| **Criar e publicar seu próprio middleware** — esqueleto, thread safety, neutralidade a Provider, empacotamento Boss | [**Criando um Middleware**](./doc/writing-middleware.pt-BR.md) |
| **Escolher um provider de transporte** — Indy (padrão), CrossSocket, mORMot2, ICS, HttpSys, Apache, ISAPI, CGI, daemons | [**Providers**](./doc/providers.pt-BR.md) |
| **Deploy** como Console / VCL / Daemon / Serviço Windows / LCL / HTTPApplication — receita de uma página | [**Cheatsheet de Deploy**](./doc/deployment.pt-BR.md) |
| Catálogo completo de middlewares com descrições estendidas | [Ecossistema de Middlewares](./doc/middleware-ecosystem.pt-BR.md) |
| Testes de integração automatizados, resiliência (Access Violation) e limites de Stack | [Testes de Integridade](./doc/integrity-testing.pt-BR.md) |
| Versões suportadas de Delphi / FPC e plataformas | [Suporte de Compilador](./doc/compiler-support.pt-BR.md) |

## 🔌 Providers (camada de transporte)

Um _provider_ é o transporte HTTP que é dono do socket e entrega requisições aos seus handlers de rota. **O mesmo código de handler roda em qualquer provider** — você seleciona um em tempo de compilação via uma Conditional Define. O Provider padrão depende do compilador: **Indy** no Delphi (para Console / VCL / Daemon), **`fphttpserver`** no FPC (para Daemon / HTTPApplication / LCL). Os Providers opcionais **CrossSocket** e **mORMot2** substituem ambos por E/S assíncrona **IOCP / epoll / kqueue**; o Provider opcional **ICS** (Delphi; Windows + Linux64/macOS) traz a pilha moderna **OpenSSL 3.x / 4.x** do OverbyteICS — TLS 1.3, SNI, mTLS. O Provider **HttpSys** (Windows) é **nativo do Horse** — usa a pilha HTTP em modo kernel **http.sys** do sistema (a mesma do IIS), sem biblioteca externa.

| Provider | Define de compilação | Delphi | Lazarus |
| ----------------------------------------------------------------------------------------------- | ----------------------- | :------------------: | :-------------------------: |
| **Indy** _(padrão Delphi para self-hosted)_                                                     | _(nenhum)_              | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;n/a |
| **`fphttpserver`** _(padrão FPC para self-hosted)_                                              | _(nenhum)_              | &nbsp;&nbsp;&nbsp;n/a | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[horse-provider-crosssocket](https://github.com/freitasjca/horse-provider-crosssocket)**    | `HORSE_CROSSSOCKET`     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[horse-provider-mormot](https://github.com/freitasjca/horse-provider-mormot)**               | `HORSE_PROVIDER_MORMOT` | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
 🆕 **[HTTP.sys](./doc/httpsys.pt-BR.md)** _(driver de modo kernel do Windows para ultra-baixa latência)_ | `HORSE_PROVIDER_HTTPSYS` | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[epoll](./doc/epoll.pt-BR.md)** _(event loop assíncrono nativo do Linux)_                  | `HORSE_PROVIDER_EPOLL`   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| 🆕 **[horse-provider-ics](https://github.com/freitasjca/horse-provider-ics)** _(Delphi; Win + Linux/macOS)_  | `HORSE_PROVIDER_ICS`    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |

> **Nota** — Os tipos de aplicação Apache / ISAPI / CGI / FastCGI (abaixo) **não** usam nenhum desses Providers. O processo host (Apache, IIS, o webserver) é dono do socket; o Horse roda in-process. Veja [Providers e Tipos de aplicação](./doc/providers.pt-BR.md) para o modelo completo.

> **Instalação do Delphi-Cross-Socket** — clone [`winddriver/Delphi-Cross-Socket`](https://github.com/winddriver/Delphi-Cross-Socket) (upstream) **junto com** [`cnpack/cnvcl/.../Crypto`](https://github.com/cnpack/cnvcl/tree/master/Source/Crypto) para as units CnPack/Crypto exigidas, e adicione os _search paths_ ao seu projeto. Três correções que antes eram exclusivas do fork já foram incorporadas ao upstream desde o 2º trimestre de 2026, então o mainline do upstream é adequado para uso geral. Para **mTLS** do lado servidor (`SSLVerifyPeer = True` + `SSLCACertFile = ...`) use o release pré-empacotado [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3) — um único clone, CnPack já incluso, APIs de mTLS (`SetCACertificateFile` + `SetVerifyPeer`) prontas para uso. Veja [Instalação do horse-provider-crosssocket](./doc/providers.pt-BR.md#crosssocket-opcional) para o detalhamento completo dos dois caminhos.

> **Instalação do OverbyteICS** — o Provider ICS requer o [OverbyteICS](https://wiki.overbyte.eu/wiki/index.php/ICS_Download) (v9.x). **Instale o ICS seguindo as instruções oficiais do ICS** — baixe/clone o ICS e adicione a pasta `Source/` ao _search path_ do seu projeto (o ICS não é instalável via Boss). Para TLS, as bibliotecas OpenSSL acompanham o ICS (DLLs no Windows, `.so` no Linux). O Provider ICS é **somente Delphi — Windows e POSIX (Linux64 / macOS)** via o pump de mensagens próprio do ICS (`Ics.Posix.*`) (no Linux use `HORSE_APPTYPE_DAEMON` + `THorseICSLinuxDaemonApp.Run`); um port para **Lazarus/FPC não é viável** — a camada POSIX do ICS usa a RTL POSIX do Delphi e o ICS desativa o OpenSSL no FPC. Seu diferencial é a pilha OpenSSL 3.x / 4.x do ICS (TLS 1.3, SNI, mTLS). Veja [horse-provider-ics](https://github.com/freitasjca/horse-provider-ics) para configuração, a suíte de testes A–K e limitações conhecidas.

> **HttpSys** — **sem instalação**: a unit `Horse.Provider.HttpSys` acompanha o Horse e usa diretamente a `httpapi.dll` do Windows (http.sys), portanto não há biblioteca externa. Defina `HORSE_PROVIDER_HTTPSYS` (Windows; Delphi ou Lazarus). Como o http.sys é uma pilha HTTP em modo kernel e de escopo da máquina, vincular um host diferente de `localhost` ou uma porta privilegiada exige uma reserva de URL única (`netsh http add urlacl url=http://+:9000/ user=Everyone`) ou direitos de Administrador; o HTTPS usa o repositório de certificados do Windows via `netsh http add sslcert`. É mutuamente exclusivo com os Providers CrossSocket / mORMot / ICS (um transporte por build).

## 🎯 Tipos de aplicação

Como o binário é empacotado e iniciado. Tipos **self-hosted** rodam sob o Provider escolhido acima; tipos **host-managed** delegam o socket ao webserver, que se torna o transporte.

| Tipo de aplicação | Define de compilação | Delphi | Lazarus |
| ---------------------------------------------------------------------------- | ----------------- | :------------------: | :-------------------------: |
| _**Self-hosted** (usa o Provider selecionado)_                                                                                                                            |
| [Console](./doc/providers.pt-BR.md) _(padrão)_                              | _(nenhum)_        | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| [VCL](./doc/providers.pt-BR.md)                                              | `HORSE_VCL`       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
| [Daemon — Serviço Windows](./doc/providers.pt-BR.md)                        | `HORSE_DAEMON`    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;n/a |
| [Daemon — daemon Linux (systemd)](./doc/providers.pt-BR.md)                 | `HORSE_DAEMON`    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| [LCL](./doc/providers.pt-BR.md) (GUI Lazarus)                               | `HORSE_LCL`       | &nbsp;&nbsp;&nbsp;❌ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| [HTTPApplication](./doc/providers.pt-BR.md) (FPC)                           | _(padrão FPC)_    | &nbsp;&nbsp;&nbsp;❌ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| _**Host-managed** (o webserver é dono do socket; Provider acima não é usado)_                                                                                              |
| [Módulo Apache](./doc/providers.pt-BR.md)                                   | `HORSE_APACHE`    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| [ISAPI](./doc/providers.pt-BR.md) (IIS)                                     | `HORSE_ISAPI`     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
| [CGI](./doc/providers.pt-BR.md)                                              | `HORSE_CGI`       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
| [FastCGI](./doc/providers.pt-BR.md)                                          | `HORSE_FCGI`      | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |

> **Nota** – `HORSE_DAEMON` é um tipo de aplicação **adaptável à plataforma**:  
> - No **Windows** → compila como um **Serviço Windows** (`Vcl.SvcMgr.TService` + SCM)  
> - No **Linux** → compila como um **daemon systemd** (utiliza `signal(SIGTERM)` + systemd)  
>  
> “Daemon” é o termo nativo do Unix; o Windows não possui um equivalente exato, por isso o mesmo nome da definição é usado em ambas as plataformas.


Veja [Providers](./doc/providers.pt-BR.md) para a matriz de compatibilidade completa e como combinar Provider × Tipo de aplicação.

## 🧬 Middlewares oficiais

Para um _ecossistema_ de middlewares mais _sustentável_, colocamos os middlewares oficiais em repositórios separados:

| Middleware | Delphi | Lazarus |
| ------------------------------------------------------------------- | -------------------- | --------------------------- |
|  [horse/json](https://github.com/HashLoad/jhonson)                  | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/basic-auth](https://github.com/HashLoad/horse-basic-auth)   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/cors](https://github.com/HashLoad/horse-cors)               | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/stream](https://github.com/HashLoad/horse-octet-stream)     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/jwt](https://github.com/HashLoad/horse-jwt)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/exception](https://github.com/HashLoad/handle-exception)    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/logger](https://github.com/HashLoad/horse-logger)           | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/compression](https://github.com/HashLoad/horse-compression) | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |

## 🌱 Middlewares de terceiros

Esta é uma lista de middlewares criados pela comunidade Horse — abra um PR se quiser ver o seu aqui!

| Middleware | Delphi | Lazarus |
| ---------------------------------------------------------------------------------------------------------- | -------------------- | --------------------------- |
|  [bittencourtthulio/etag](https://github.com/bittencourtthulio/Horse-ETag)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [bittencourtthulio/paginate](https://github.com/bittencourtthulio/Horse-Paginate)                         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [bittencourtthulio/cachecontrol](https://github.com/bittencourtthulio/horse-cachecontrol)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [gabrielbaltazar/gbswagger](https://github.com/gabrielbaltazar/gbswagger)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [willhubner/socketIO](https://github.com/WillHubner/Horse-SocketIO)                                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/ratelimit](https://github.com/dliocode/horse-ratelimit)                                         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/slowdown](https://github.com/dliocode/horse-slowdown)                                           | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [giorgiobazzo/upload](https://github.com/giorgiobazzo/horse-upload)                                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/query](https://github.com/dliocode/horse-query)                                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [CarlosHe/healthcheck](https://github.com/CarlosHe/horse-healthcheck)                                     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [CarlosHe/staticfiles](https://github.com/CarlosHe/horse-staticfiles)                                     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [CachopaWeb/horse-server-static](https://github.com/CachopaWeb/horse-server-static)                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [arvanus/horse-exception-logger](https://github.com/arvanus/horse-exception-logger)                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [claudneysessa/Horse-CSResponsePagination](https://github.com/claudneysessa/Horse-CSResponsePagination)   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [claudneysessa/Horse-XSuperObjects](https://github.com/claudneysessa/Horse-XSuperObjects)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [andre-djsystem/horse-bearer-auth](https://github.com/andre-djsystem/horse-bearer-auth)                   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [andre-djsystem/horse-manipulate-request](https://github.com/andre-djsystem/horse-manipulate-request)     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [andre-djsystem/horse-manipulate-response](https://github.com/andre-djsystem/horse-manipulate-response)   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [antoniojmsjr/Horse-IPGeoLocation](https://github.com/antoniojmsjr/Horse-IPGeoLocation)                   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [antoniojmsjr/Horse-XMLDoc](https://github.com/antoniojmsjr/Horse-XMLDoc)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [isaquepinheiro/horse-jsonbr](https://github.com/HashLoad/JSONBr)                                         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [IagooCesaar/Horse-JsonInterceptor](https://github.com/IagooCesaar/Horse-JsonInterceptor)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/horse-datalogger](https://github.com/dliocode/horse-datalogger)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [marcobreveglieri/horse-prometheus-metrics](https://github.com/marcobreveglieri/horse-prometheus-metrics) | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [weslleycapelari/horse-documentation](https://github.com/weslleycapelari/horse-documentation)             | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [weslleycapelari/horse-validator](https://github.com/weslleycapelari/horse-validator)                     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |

## Versões do Delphi

O `Horse` funciona com Delphi 13 Florence, Delphi 12 Athens, Delphi 11 Alexandria, Delphi 10.4 Sydney, Delphi 10.3 Rio, Delphi 10.2 Tokyo, Delphi 10.1 Berlin, Delphi 10 Seattle, Delphi XE8 e Delphi XE7.

Para a matriz completa de plataformas por provider, veja [Suporte de Compilador](./doc/compiler-support.pt-BR.md).

## 🤝 Contribuindo

Veja [CONTRIBUTING.pt-BR.md](./CONTRIBUTING.pt-BR.md) para reportar bugs, sugerir features e enviar mudanças de código ou documentação. Os docs bilíngues EN / PT-BR são mantidos sincronizados — ao editar um idioma, edite o outro no mesmo PR.

## 💻 Contribuidores

<a href="https://github.com/Hashload/horse/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=Hashload/horse" />
</a>

## ⚠️ Licença

O `Horse` é software livre e de código aberto, licenciado sob a [Licença MIT](https://github.com/HashLoad/horse/blob/master/LICENSE).

## 📐 Testes

![tests](https://github.com/GlerystonMatos/horse/workflows/tests/badge.svg) ![Console Coverage ](https://img.shields.io/badge/console%20coverage-45%25-blue) ![VCL Coverage ](https://img.shields.io/badge/vcl%20coverage-43%25-blue)
