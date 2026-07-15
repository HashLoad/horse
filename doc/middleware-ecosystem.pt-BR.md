# Ecossistema de Middlewares

*Leia em [English](./middleware-ecosystem.md) ou [Português (BR)](./middleware-ecosystem.pt-BR.md).*

O Horse mantém um core pequeno. Quase tudo que você associaria a um "framework web" — parsing JSON, CORS, JWT, compressão, logging — fica em pacotes de middleware separados e opt-in. Esta página cataloga os oficiais (mantidos pela HashLoad) e a lista da comunidade.

Para como o middleware realmente funciona, veja [Middleware](./middleware.pt-BR.md).

---

## Middlewares oficiais (HashLoad/*)

Para um ecossistema mais sustentável, os middlewares oficiais ficam em repositórios separados, não no core do Horse. Instale com o Boss:

```sh
boss install <repo>
```

| Middleware | O que faz | Delphi | Lazarus |
|---|---|:---:|:---:|
| [horse/jhonson](https://github.com/HashLoad/jhonson) | Parseia bodies JSON de requisição, serializa respostas JSON | ✔️ | ✔️ |
| [horse/basic-auth](https://github.com/HashLoad/horse-basic-auth) | Autenticação HTTP Basic | ✔️ | ✔️ |
| [horse/cors](https://github.com/HashLoad/horse-cors) | Headers de Cross-Origin Resource Sharing + preflight | ✔️ | ✔️ |
| [horse/stream](https://github.com/HashLoad/horse-octet-stream) | Tratamento de body `application/octet-stream` | ✔️ | ✔️ |
| [horse/jwt](https://github.com/HashLoad/horse-jwt) | Autenticação por JSON Web Token | ✔️ | ✔️ |
| [horse/exception](https://github.com/HashLoad/handle-exception) | Converte exceptions lançadas em respostas JSON de erro consistentes | ✔️ | ✔️ |
| [horse/logger](https://github.com/HashLoad/horse-logger) | Log de acesso por requisição | ✔️ | ✔️ |
| [horse/compression](https://github.com/HashLoad/horse-compression) | Compressão gzip / deflate de resposta | ✔️ | ✔️ |

### Composição típica

```delphi
uses
  Horse,
  Horse.Jhonson,
  Horse.CORS,
  Horse.HandleException,
  Horse.Logger,
  Horse.JWT;

begin
  THorse
    .Use(Horse.HandleException.New)    // mais externo — erros viram respostas limpas
    .Use(Horse.Logger.New)             // logar toda requisicao
    .Use(CORS)                         // headers CORS + preflight OPTIONS
    .Use(Jhonson)                      // parsear bodies JSON
    .Use(JWT('secret'));               // exigir token valido (mais interno)

  // rotas …

  THorse.Listen(9000);
end.
```

A ordem de registro importa — veja [Middleware](./middleware.pt-BR.md#registro).

---

## Middlewares de terceiros (comunidade)

Estes não são mantidos pela HashLoad mas estão listados aqui por serem usados com frequência. A lista cresce com o tempo — abra um PR contra este doc se o seu deveria estar aqui.

| Middleware | Descrição | Delphi | Lazarus |
|---|---|:---:|:---:|
| [bittencourtthulio/etag](https://github.com/bittencourtthulio/Horse-ETag) | Geração de ETag | ✔️ | ✔️ |
| [bittencourtthulio/paginate](https://github.com/bittencourtthulio/Horse-Paginate) | Paginação de resposta de listagem | ✔️ | ✔️ |
| [bittencourtthulio/cachecontrol](https://github.com/bittencourtthulio/horse-cachecontrol) | Headers Cache-Control | ✔️ | ❌ |
| [gabrielbaltazar/gbswagger](https://github.com/gabrielbaltazar/gbswagger) | Geração Swagger / OpenAPI | ✔️ | ❌ |
| [willhubner/socketIO](https://github.com/WillHubner/Horse-SocketIO) | Servidor Socket.IO | ✔️ | ❌ |
| [dliocode/ratelimit](https://github.com/dliocode/horse-ratelimit) | Rate limiting | ✔️ | ❌ |
| [dliocode/slowdown](https://github.com/dliocode/horse-slowdown) | Atraso progressivo de requisição | ✔️ | ❌ |
| [giorgiobazzo/upload](https://github.com/giorgiobazzo/horse-upload) | Helpers de upload de arquivo | ✔️ | ❌ |
| [dliocode/query](https://github.com/dliocode/horse-query) | DSL de query string | ✔️ | ❌ |
| [CarlosHe/healthcheck](https://github.com/CarlosHe/horse-healthcheck) | Endpoint `/healthz` | ✔️ | ❌ |
| [CarlosHe/staticfiles](https://github.com/CarlosHe/horse-staticfiles) | Serviço de arquivos estáticos | ✔️ | ❌ |
| [CachopaWeb/horse-server-static](https://github.com/CachopaWeb/horse-server-static) | Serviço de arquivos estáticos (alternativa) | ✔️ | ✔️ |
| [arvanus/horse-exception-logger](https://github.com/arvanus/horse-exception-logger) | Log de exceptions | ✔️ | ✔️ |
| [claudneysessa/Horse-CSResponsePagination](https://github.com/claudneysessa/Horse-CSResponsePagination) | Paginação de resposta | ✔️ | ❌ |
| [claudneysessa/Horse-XSuperObjects](https://github.com/claudneysessa/Horse-XSuperObjects) | Integração com XSuperObject | ✔️ | ❌ |
| [andre-djsystem/horse-bearer-auth](https://github.com/andre-djsystem/horse-bearer-auth) | Autenticação por Bearer token | ✔️ | ✔️ |
| [andre-djsystem/horse-manipulate-request](https://github.com/andre-djsystem/horse-manipulate-request) | Helpers de mutação de requisição | ✔️ | ✔️ |
| [andre-djsystem/horse-manipulate-response](https://github.com/andre-djsystem/horse-manipulate-response) | Helpers de mutação de resposta | ✔️ | ✔️ |
| [antoniojmsjr/Horse-IPGeoLocation](https://github.com/antoniojmsjr/Horse-IPGeoLocation) | Geolocalização de IP | ✔️ | ❌ |
| [antoniojmsjr/Horse-XMLDoc](https://github.com/antoniojmsjr/Horse-XMLDoc) | Geração de documentação XML | ✔️ | ❌ |
| [isaquepinheiro/horse-jsonbr](https://github.com/HashLoad/JSONBr) | Helpers JSON | ✔️ | ❌ |
| [IagooCesaar/Horse-JsonInterceptor](https://github.com/IagooCesaar/Horse-JsonInterceptor) | Interceptor de JSON em requisição/resposta | ✔️ | ❌ |
| [dliocode/horse-datalogger](https://github.com/dliocode/horse-datalogger) | Logger de dados estruturado | ✔️ | ❌ |
| [weslleycapelari/horse-documentation](https://github.com/weslleycapelari/horse-documentation) | Geração automática de docs de API | ✔️ | ❌ |
| [weslleycapelari/horse-validator](https://github.com/weslleycapelari/horse-validator) | Validação de payload de requisição | ✔️ | ❌ |
| [regyssilveira/horse-rate-limit](https://github.com/regyssilveira/horse-rate-limit) | Controle de limite de requisições (Rate Limiting) com suporte a múltiplos storages | ✔️ | ✔️ |
| [regyssilveira/horse-compression-v2](https://github.com/regyssilveira/horse-compression-v2) | Compressão de alta performance com suporte a Gzip, Deflate e Brotli | ✔️ | ✔️ |
| [regyssilveira/horse-static](https://github.com/regyssilveira/horse-static) | Servidor de arquivos estáticos de alta performance com suporte a Range, Cache e SPA Fallback | ✔️ | ✔️ |
| [regyssilveira/horse-dto](https://github.com/regyssilveira/horse-dto) | Middleware de auto-binding e validação declarativa de dados | ✔️ | ✔️ |
| [regyssilveira/horse-rbac](https://github.com/regyssilveira/horse-rbac) | Controle de acesso baseado em funções (RBAC) e escopos (scopes) | ✔️ | ✔️ |
| [regyssilveira/horse-schema-validation](https://github.com/regyssilveira/horse-schema-validation) | Middleware de validação de esquemas (schemas) de payload de requisições | ✔️ | ✔️ |
| [regyssilveira/horse-multipart](https://github.com/regyssilveira/horse-multipart) | Middleware para processamento e parsing de corpos de requisição multipart/form-data | ✔️ | ✔️ |
| [regyssilveira/horse-helmet](https://github.com/regyssilveira/horse-helmet) | Middleware de segurança para proteção e configuração de cabeçalhos HTTP | ✔️ | ✔️ |
| [regyssilveira/horse-ssl-redirect](https://github.com/regyssilveira/horse-ssl-redirect) | Redirecionamento automático de requisições HTTP inseguras para HTTPS | ✔️ | ✔️ |
| [regyssilveira/horse-crud](https://github.com/regyssilveira/horse-crud) | Middleware para o framework Horse voltado à geração automática e dinâmica de rotas CRUD a partir de entidades decoradas | ✔️ | ✔️ |
| [regyssilveira/horse-request-id](https://github.com/regyssilveira/horse-request-id) | Middleware de geração e rastreamento de Request ID (Correlation ID) para o ecossistema do framework Horse | ✔️ | ✔️ |
| [regyssilveira/horse-sanitize](https://github.com/regyssilveira/horse-sanitize) | Middleware para sanitizar parâmetros de requisições do ecossistema Horse contra XSS e injeções nocivas | ✔️ | ✔️ |


---

## Telemetria

Estes são middlewares focados em observabilidade, métricas e rastreamento de aplicações:

| Middleware | Descrição | Delphi | Lazarus |
|---|---|:---:|:---:|
| [marcobreveglieri/horse-prometheus-metrics](https://github.com/marcobreveglieri/horse-prometheus-metrics) | Endpoint de métricas Prometheus | ✔️ | ❌ |
| [regyssilveira/horse-opentelemetry](https://github.com/regyssilveira/horse-opentelemetry) | Integração com OpenTelemetry para rastreamento distribuído | ✔️ | ✔️ |
| [regyssilveira/horse-prometheus](https://github.com/regyssilveira/horse-prometheus) | Integração com Prometheus para coleta de métricas | ✔️ | ✔️ |

---

## Providers de transporte de terceiros

Além de middleware, a comunidade produziu **Providers de transporte** alternativos — a camada que o Horse usa para ser dono do socket e parsear HTTP. Este é um eixo separado da lista de middlewares acima (veja [Providers e Tipos de aplicação](./providers.pt-BR.md) para o modelo conceitual completo). Os Providers de terceiros ativamente mantidos são:

| Provider | Descrição | Licença | Repositório |
|---|---|---|---|
| **horse-provider-crosssocket** | Transporte assíncrono IOCP / epoll / kqueue. Substitui o Provider padrão Indy pelo [Delphi-Cross-Socket](https://github.com/winddriver/Delphi-Cross-Socket). A instalação é manual (espelha o mORMot2): clone `winddriver/Delphi-Cross-Socket` + [`cnpack/cnvcl`](https://github.com/cnpack/cnvcl) e adicione os search paths. Alternativa suportada para usuários de mTLS ou que querem uma dependência única: [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3) (embute o CnPack + APIs de mTLS). Ativado por `HORSE_PROVIDER_CROSSSOCKET`. Exige Delphi 10.2+ / FPC 3.2+. Baseline de teste: 80 passados / 1 falho (a limitação documentada de Set-Cookie multi-valor do core do Horse). | MIT | [freitasjca/horse-provider-crosssocket](https://github.com/freitasjca/horse-provider-crosssocket) |
| **horse-provider-mormot** | Transporte assíncrono IOCP / epoll. Substitui o Provider padrão Indy pelo `THttpServer` do [mORMot2](https://github.com/synopse/mORMot2) — pure Pascal, sem deps em C compilado para HTTP padrão. Troca opcional para `THttpApiServer` para HTTP em modo kernel via http.sys no Windows. Ativado por `HORSE_PROVIDER_MORMOT`. Compatível com Delphi 7 até 12.3 Athens e FPC 3.2+. Inclui units de conveniência cross-product para Console, VCL, Daemon (TService Windows + runner POSIX), LCL Lazarus, e HTTPApplication FPC. | MIT | [freitasjca/horse-provider-mormot](https://github.com/freitasjca/horse-provider-mormot) |

Os dois Providers usam a mesma arquitetura de interface híbrida (`IHorseRawRequest` / `IHorseRawResponse`), então todo middleware do Horse funciona sem alterações em qualquer um. Os dois defines de Provider são mutuamente exclusivos — exatamente um transporte por build.

Veja [Providers e Tipos de aplicação](./providers.pt-BR.md) para quando trocar o transporte Indy padrão, a matriz de compatibilidade vs. cada Tipo de aplicação, e como combinar as duas escolhas.

## Adicionando seu middleware nesta lista

1. Publique seu pacote com um `boss.json` compatível com Boss.
2. Abra um PR contra `doc/middleware-ecosystem.pt-BR.md` (e/ou `doc/middleware-ecosystem.md`) adicionando uma linha na tabela apropriada.
3. Confirme que compila na matriz que você está alegando (Delphi / Lazarus).

Não há exclusividade — qualquer pessoa pode publicar um pacote compatível com Horse sem coordenação.

## Veja também

- [Middleware](./middleware.pt-BR.md) — o modelo, como criar o seu, ordem de registro.
- [Providers](./providers.pt-BR.md) — middleware funciona entre todos os transportes; por isso.
