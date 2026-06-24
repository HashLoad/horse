# Request e Response

*Leia em [English](./request-response.md) ou [Português (BR)](./request-response.pt-BR.md).*

Todo callback de rota do Horse recebe um `THorseRequest` e um `THorseResponse`. Esta página é a referência da API de ambos.

```delphi
procedure(Req: THorseRequest; Res: THorseResponse)
```

Para a declaração da rota em si, veja [Roteamento](./routing.pt-BR.md). Para middleware que envolve esses objetos, veja [Middleware](./middleware.pt-BR.md).

---

## `THorseRequest`

| Accessor | Tipo | O que retorna |
|---|---|---|
| `Body` | `string` | Corpo da requisição decodificado como UTF-8. Idempotente — múltiplas leituras retornam a mesma string em cache. |
| `Body<T>` | genérico | Retorna `FBody as T` — usado quando middleware (ex. `Jhonson`) parseia o body num objeto. |
| `Body(AObject)` / `Body(AObject, AOwnsBody)` | setter | Usado por middleware para anexar um objeto parseado. Com `AOwnsBody = True` (o padrão / forma de 1 argumento) o Horse é dono do objeto e o libera, liberando antes qualquer valor anterior que lhe pertença. Transportes cujo corpo é uma referência não-proprietária para um buffer de socket (ex.: CrossSocket) passam `AOwnsBody = False` para que o `Clear` apenas anule a referência sem liberá-la. |
| `Params` | `THorseCoreParam` | Parâmetros de caminho: `Req.Params['id']`. |
| `Query` | `THorseCoreParam` | Query string: `Req.Query['name']`. |
| `Headers` | `THorseCoreParam` | Headers da requisição: `Req.Headers['Content-Type']`. Lookup case-insensitive. |
| `Cookie` | `THorseCoreParam` | Header `Cookie:` parseado: `Req.Cookie['session']`. Valores que contêm `=` (base64/JWT) são preservados por inteiro (o parser separa apenas no **primeiro** `=`). |
| `ContentFields` | `THorseCoreParam` | Campos parseados de body `application/x-www-form-urlencoded`. |
| `Sessions` | `THorseSessions` | Mapa de sessões no servidor (um por requisição). |
| `Method` | `string` | Verbo HTTP cru: `'GET'`, `'POST'`, `'OPTIONS'`, etc. |
| `MethodType` | `TMethodType` | Forma enum (veja [Roteamento](./routing.pt-BR.md)). |
| `PathInfo` | `string` | Caminho decodificado: `/users/42`. |
| `Host` | `string` | Valor do header `Host:`. |
| `ContentType` | `string` | Header `Content-Type` da requisição. |
| `RawWebRequest` | `TWebRequest` / `TRequest` | O objeto subjacente do provider — `TIdHTTPRequestInfo` do Indy no provider padrão, um adaptador nos providers não-Indy. |

### Ler um body de requisição

```delphi
THorse.Post('/echo',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    // Body texto cru
    Res.Send('Voce enviou: ' + Req.Body);
  end);
```

Para JSON, registre o middleware `Jhonson` uma vez no startup e use `Body<TJSONObject>`:

```delphi
uses Horse, Horse.Jhonson, System.JSON;

THorse.Use(Jhonson);

THorse.Post('/items',
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    Json: TJSONObject;
  begin
    Json := Req.Body<TJSONObject>;
    Res.Send('Recebi: ' + Json.GetValue('name').Value);
  end);
```

### Ler params, query, headers

Todos retornam `THorseCoreParam`, um accessor parecido com dicionário:

```delphi
THorse.Get('/search/:type',
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    Limit: Integer;
  begin
    if not TryStrToInt(Req.Query['limit'], Limit) then
      Limit := 10;

    Res.Send(Format('Buscando %s, limite %d, por %s',
      [Req.Params['type'], Limit, Req.Headers['X-User']]));
  end);
```

`THorseCoreParam`:

- `Items[name: string]: string` — indexer padrão, retorna `''` se ausente.
- `TryGetValue(name; out value): Boolean` — distingue ausente vs vazio.
- `Dictionary: TDictionary<string,string>` (Delphi) / `TStringList` (FPC) — acesso direto à coleção se precisar iterar.

### Upload de arquivos

Requisições `multipart/form-data` populam `Req.ContentFields`:

```delphi
THorse.Post('/upload',
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    Stream: TStream;
  begin
    Stream := Req.ContentFields.Field('file').AsStream;   // campo de arquivo (campos de texto: Field('x').AsString)
    try
      Stream.SaveToFile('uploaded.bin');
      Res.Send('Salvo ' + IntToStr(Stream.Size) + ' bytes');
    finally
      // Caminho CrossSocket: NUNCA libere — é referência não-dona.
      // Caminho Indy: também não libere; THorseRequest gerencia.
    end;
  end);
```

---

## `THorseResponse`

| Método | Retorna | Efeito |
|---|---|---|
| `Send(AContent: string)` | `THorseResponse` (fluente) | Escreve um body string. Status padrão `200`. |
| `Send<T>(AContent: T)` | `THorseResponse` | Escreve um objeto; a cadeia de middleware tipicamente serializa (ex. JSON via `Jhonson`). |
| `Status(AStatus: Integer)` | `THorseResponse` | Define código HTTP. Padrão `200`. |
| `Status(AStatus: THTTPStatus)` | `THorseResponse` | Variante tipada — ex. `THTTPStatus.NotFound`. |
| `Status` (sem arg) | `Integer` | Lê o status atualmente configurado. |
| `ContentType(AContentType: string)` | `THorseResponse` | Define o header `Content-Type`. |
| `AddHeader(AName, AValue: string)` | `THorseResponse` | Adiciona um header de resposta. |
| `RemoveHeader(AName: string)` | `THorseResponse` | Remove um header previamente adicionado. |
| `Cookie(AName, AValue: string)` | `THorseCookie` | Adiciona um cookie e o retorna para configurar atributos de forma fluente. Veja [Cookies](#cookies). |
| `AddCookie(ACookie: THorseCookie)` | `THorseResponse` | Adiciona um `THorseCookie` já construído (a propriedade do objeto passa para a resposta). |
| `RedirectTo(ALocation: string)` | `THorseResponse` | Envia `302 Found` com `Location:`. |
| `RedirectTo(ALocation, AStatus)` | `THorseResponse` | Permite escolher `301`, `307`, etc. |
| `SendFile(AFileName: string)` | `THorseResponse` | Faz stream de um arquivo como body; define `Content-Type` pela extensão. |
| `SendFile(AStream, AFileName, AContentType)` | `THorseResponse` | Stream em memória. |
| `Download(AFileName: string)` | `THorseResponse` | Como `SendFile` mas adiciona `Content-Disposition: attachment`. |
| `Download(AStream, AFileName, AContentType)` | `THorseResponse` | Stream + header de anexo. |
| `Render(AFileName: string)` | `THorseResponse` | Faz stream inline (sem header de anexo). |
| `RawWebResponse` | `TWebResponse` / `TResponse` | Response subjacente do provider. Usado por middleware que precisa de acesso direto. |

### Exemplos

**Texto puro:**
```delphi
Res.ContentType('text/plain').Send('hello');
```

**JSON (via Jhonson):**
```delphi
uses System.JSON;

var Json := TJSONObject.Create;
Json.AddPair('ok', TJSONBool.Create(True));
Res.Send<TJSONObject>(Json);   // Jhonson serializa + libera
```

**Status + body para um erro:**
```delphi
Res.Status(THTTPStatus.BadRequest)
   .ContentType('application/json')
   .Send('{"error":"campo obrigatorio"}');
```

**Redirect:**
```delphi
Res.RedirectTo('/login');
```

### Cookies

`Res.Cookie(name, value)` adiciona um cookie RFC 6265 e retorna um `THorseCookie`
(unit `Horse.Core.Cookie`) para configurar atributos de forma fluente. Cada cookie
vira seu próprio header `Set-Cookie` — dá para definir **vários** numa só resposta:

```delphi
uses Horse.Core.Cookie;   // para TSameSite (ssStrict / ssLax / ssNone)

Res.Cookie('sid', SessionId)
   .Path('/')
   .HttpOnly(True)
   .Secure(True)
   .SameSite(ssLax)
   .MaxAge(3600);

Res.Cookie('theme', 'dark');   // um segundo cookie → uma segunda linha Set-Cookie
```

| Atributo | Método |
|---|---|
| `Path` / `Domain` | `.Path('/')`, `.Domain('example.com')` |
| `Expires` (UTC) | `.Expires(EncodeDate(2030,1,1))` |
| `Max-Age` (segundos) | `.MaxAge(3600)` |
| `Secure` / `HttpOnly` | `.Secure(True)`, `.HttpOnly(True)` |
| `SameSite` | `.SameSite(ssStrict | ssLax | ssNone)` |

**Validação:** o nome do cookie precisa ser um token e nem o nome nem o valor podem
conter caracteres de controle, CR/LF ou `;` — entrada inválida levanta
`EHorseException`. Essa validação vale só para a API tipada; o caminho legado
`Res.AddHeader('Set-Cookie', …)` continua igual (mas guarda apenas **um** cookie,
por passar pelo mapa de headers).

**Notas por provider:**
- **CrossSocket** e **mORMot** emitem todos os atributos, uma linha `Set-Cookie` por cookie.
- **Indy (Delphi)** mapeia para `TWebResponse.Cookies`: `Max-Age` não é representável
  ali (use `.Expires()` no Indy); `HttpOnly` exige Delphi 10.1+, `SameSite` Delphi 10.4+.

**Download de arquivo:**
```delphi
Res.Download('reports/2026-05.pdf');
// Envia Content-Disposition: attachment; filename="2026-05.pdf"
```

**Stream de um arquivo gerado:**
```delphi
var Stream := TMemoryStream.Create;
GerarCSV(Stream);
Stream.Position := 0;
Res.Download(Stream, 'export.csv', 'text/csv');
```

**Adicionar header customizado:**
```delphi
Res.AddHeader('X-Rate-Limit-Remaining', '47').Send('ok');
```

### Helpers de status

`Horse.Commons.THTTPStatus` fornece constantes nomeadas para códigos comuns — preferível a números mágicos:

```pascal
Res.Status(THTTPStatus.OK);              // 200
Res.Status(THTTPStatus.Created);         // 201
Res.Status(THTTPStatus.NoContent);       // 204
Res.Status(THTTPStatus.BadRequest);      // 400
Res.Status(THTTPStatus.Unauthorized);    // 401
Res.Status(THTTPStatus.NotFound);        // 404
Res.Status(THTTPStatus.InternalServerError); // 500
```

`Horse.Commons.THTTPStatusHelper.ToString` converte o enum de volta à reason phrase padrão se você precisar imprimir.

### Erros e exceções

Lançar `EHorseException` no callback faz curto-circuito na resposta:

```delphi
uses Horse.Exception;

THorse.Get('/secret',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    if Req.Headers['X-Auth'] <> 'secret' then
      raise EHorseException.New.Status(THTTPStatus.Unauthorized).Error('Token invalido');
    Res.Send('welcome');
  end);
```

O framework converte a exception numa resposta JSON de erro. Qualquer outra exception não tratada vira `500` com body genérico.

Para interceptar exceptions globalmente, registre o middleware `handle-exception` ([`HashLoad/handle-exception`](https://github.com/HashLoad/handle-exception)) — ele formata seus erros de forma consistente.

---

## Notas específicas de provider

A maioria do código da aplicação nunca precisa se preocupar com o transporte. Algumas exceções:

- **Posse do body no CrossSocket:** `Req.Body<TStream>` no provider CrossSocket retorna uma referência não proprietária para o buffer de recepção. Nunca o libere com Free. Se precisar do stream após a requisição retornar, copie o conteúdo para um  `TMemoryStream` próprio. (Não se aplica ao Indy — o Indy entrega um stream próprio.)
- **Handlers concorrentes:** Indy roda uma thread por conexão; CrossSocket distribui para um pool de threads de IO. Em ambos, seu handler roda até o fim numa única thread, então estado por-requisição é seguro. Estado compartilhado precisa de lock explícito (`TCriticalSection`, `TMonitor`).
- **`Req.RawWebRequest` e `Res.RawWebResponse`:** middleware que mexe nos objetos subjacentes (ex. `Horse.CORS` definindo `Access-Control-Allow-Origin` direto) continua funcionando entre providers — providers não-Indy retornam um adaptador que expõe a mesma superfície.

Veja [Providers](./providers.pt-BR.md) para o detalhamento completo.

## Veja também

- [Roteamento](./routing.pt-BR.md) — declarar as rotas que produzem esses callbacks.
- [Middleware](./middleware.pt-BR.md) — envolver callbacks com lógica transversal.
- [Ecossistema de Middlewares](./middleware-ecosystem.pt-BR.md) — `Jhonson` (JSON), `CORS`, `JWT`, `compression` e mais.
