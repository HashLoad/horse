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
| [marcobreveglieri/horse-prometheus-metrics](https://github.com/marcobreveglieri/horse-prometheus-metrics) | Endpoint de métricas Prometheus | ✔️ | ❌ |
| [weslleycapelari/horse-documentation](https://github.com/weslleycapelari/horse-documentation) | Geração automática de docs de API | ✔️ | ❌ |
| [weslleycapelari/horse-validator](https://github.com/weslleycapelari/horse-validator) | Validação de payload de requisição | ✔️ | ❌ |

## Providers de transporte de terceiros

Além de middleware, a comunidade produziu **Providers de transporte** alternativos — a camada que o Horse usa para ser dono do socket e parsear HTTP. Este é um eixo separado da lista de middlewares acima (veja [Providers e Tipos de aplicação](./providers.pt-BR.md) para o modelo conceitual completo). O Provider de terceiros mais ativamente mantido é:

| Provider | Descrição | Licença | Repositório |
|---|---|---|---|
| **horse-provider-crosssocket** | Transporte assíncrono IOCP / epoll / kqueue. Substitui o Provider padrão Indy pelo [Delphi-Cross-Socket](https://github.com/winddriver/Delphi-Cross-Socket). Mira deploys de alta concorrência. | MIT | [freitasjca/horse-provider-crosssocket](https://github.com/freitasjca/horse-provider-crosssocket) |

Veja [Providers e Tipos de aplicação](./providers.pt-BR.md) para quando trocar o transporte Indy padrão, a matriz de compatibilidade vs. cada Tipo de aplicação, e como combinar as duas escolhas.

## Adicionando seu middleware nesta lista

1. Publique seu pacote com um `boss.json` compatível com Boss.
2. Abra um PR contra `doc/middleware-ecosystem.pt-BR.md` (e/ou `doc/middleware-ecosystem.md`) adicionando uma linha na tabela apropriada.
3. Confirme que compila na matriz que você está alegando (Delphi / Lazarus).

Não há exclusividade — qualquer pessoa pode publicar um pacote compatível com Horse sem coordenação.

## Veja também

- [Middleware](./middleware.pt-BR.md) — o modelo, como criar o seu, ordem de registro.
- [Providers](./providers.pt-BR.md) — middleware funciona entre todos os transportes; por isso.
