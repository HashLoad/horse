# Roteamento

*Leia em [English](./routing.md) ou [Português (BR)](./routing.pt-BR.md).*

Uma *rota* liga um caminho de URL + método HTTP a um callback. Este documento cobre tudo o que o Horse expressa numa definição de rota.

Para a API dos objetos de request e response passados a cada callback, veja [Request e Response](./request-response.pt-BR.md).

---

## Rotas básicas

`THorse` expõe um método por verbo HTTP. Cada um recebe uma string de caminho e um callback:

```delphi
THorse.Get   ('/ping',     procedure(Req: THorseRequest; Res: THorseResponse) begin Res.Send('pong');    end);
THorse.Post  ('/items',    procedure(Req: THorseRequest; Res: THorseResponse) begin Res.Send('created'); end);
THorse.Put   ('/items/:id', ...);
THorse.Patch ('/items/:id', ...);
THorse.Delete('/items/:id', ...);
THorse.Head  ('/items/:id', ...);
```

O roteamento por método é exato: `THorse.Get` só atende requisições `GET` para aquele caminho. Uma requisição com método errado em um caminho conhecido retorna `405 Method Not Allowed`. Uma requisição com caminho desconhecido retorna `404 Not Found`.

## Parâmetros de caminho

Use um segmento prefixado com dois-pontos para capturar parte da URL:

```delphi
THorse.Get('/users/:id',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    Res.Send('User: ' + Req.Params['id']);
  end);
```

- `GET /users/42` → `User: 42` (e `Req.Params['id'] = '42'`)
- `GET /users/`   → `404`
- `GET /users`    → `404`

Múltiplos parâmetros num só caminho funcionam da mesma forma:

```delphi
THorse.Get('/teams/:teamId/members/:memberId',
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    T, M: string;
  begin
    T := Req.Params['teamId'];
    M := Req.Params['memberId'];
    Res.Send(Format('Team %s, member %s', [T, M]));
  end);
```

Parâmetros de caminho são sempre strings. Converta você mesmo com `StrToInt`, `TryStrToInt`, etc.

## Query strings

Query strings são acessadas via `Req.Query`:

```delphi
// GET /search?name=Horse&category=framework
THorse.Get('/search',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    Res.Send('Procurando ' + Req.Query['name'] + ' em ' + Req.Query['category']);
  end);
```

Chaves ausentes retornam string vazia. Se precisa distinguir ausente de vazio, use `Req.Query.TryGetValue` (retorna `False` quando ausente).

## Grupos de rotas

Agrupe rotas relacionadas sob um prefixo comum:

```delphi
THorse.Group.Prefix('/api/v1')
  .Get   ('/users',     ListUsers)
  .Post  ('/users',     CreateUser)
  .Get   ('/users/:id', GetUser)
  .Put   ('/users/:id', UpdateUser)
  .Delete('/users/:id', DeleteUser);
```

O acima é idêntico a escrever `THorse.Get('/api/v1/users', ...)`, etc. Grupos podem ter middleware próprio:

```delphi
THorse.Group
  .Use(JWT(SECRET))           // middleware aplica a tudo neste grupo
  .Prefix('/api/v1/admin')
  .Get ('/stats', GetStats)
  .Post('/audit', WriteAudit);
```

## Middleware wildcard

`THorse.Use(...)` registra middleware que roda em toda requisição, independentemente do caminho:

```delphi
THorse.Use(MyLogger);            // toda requisição é logada
THorse.Use('/api', RequireAuth); // apenas /api/* exige auth
```

Veja [Middleware](./middleware.pt-BR.md) para a história completa.

## O enum `TMethodType`

Internamente as rotas são armazenadas por método. O enum fica em `Horse.Commons`:

```pascal
type
  TMethodType = (mtAny, mtGet, mtPut, mtPost, mtHead, mtDelete, mtPatch);
```

`mtAny` é o wildcard usado pelo middleware (`THorse.Use`) — casa com qualquer método.

> **Nota:** `OPTIONS`, `TRACE` e `CONNECT` **não estão** em `TMethodType`. Eles roteiam como `mtAny` (casando apenas com middleware wildcard). O middleware `Horse.CORS` aproveita isso para interceptar `OPTIONS` no preflight. Se precisa distinguir pelo verbo cru no seu middleware, use `Req.Method: string` ou `Req.RawWebRequest.Method`.

## Regras de pattern matching

- **Case-sensitive** no caminho: `/Users` e `/users` são rotas diferentes.
- **Sem normalização de barra final**: `/users` e `/users/` são rotas diferentes. Defina a convenção do projeto e mantenha.
- **Primeiro registro vence em padrões idênticos** — registrar `/users/:id` duas vezes é duplicata; a segunda emite erro em runtime nas versões recentes do Horse (`Duplicate route detected: [GET] /users/:id`).
- **Segmentos de parâmetro não aceitam múltiplos dois-pontos** — `/users/:id:name` é inválido; use dois segmentos (`/users/:id/:name`).

## Sub-recursos

Não há um `mount` embutido como no Express, mas você expressa sub-recursos com grupos:

```delphi
procedure RegisterUsersRoutes(AGroup: THorseCoreGroup);
begin
  AGroup
    .Get   ('',         ListUsers)
    .Post  ('',         CreateUser)
    .Get   ('/:id',     GetUser);
end;

// No main:
RegisterUsersRoutes(THorse.Group.Prefix('/api/v1/users'));
```

O mesmo `RegisterUsersRoutes` pode ser montado sob múltiplos prefixos (`/api/v1/users` e `/api/v2/users`) sem duplicação — útil para versionamento de API quando v2 só adiciona endpoints.

## Listar rotas (para debug)

O Horse não traz um helper tipo `printRoutes()`, mas você itera a árvore do router diretamente. Mais simples é imprimir cada rota ao registrar:

```delphi
procedure RegisterAndLog(const Method, Path: string; Cb: THorseCallback);
begin
  case Method of
    'GET':  THorse.Get   (Path, Cb);
    'POST': THorse.Post  (Path, Cb);
    // ...
  end;
  WriteLn(Method, ' ', Path);
end;
```

Para apps não-triviais, mantenha o registro de rotas centralizado em uma unit — o layout fica óbvio num único arquivo.

## Veja também

- [Request e Response](./request-response.pt-BR.md) — o que fazer dentro do callback.
- [Middleware](./middleware.pt-BR.md) — `THorse.Use`, o `Next` proc, ordem de registro.
- [Providers](./providers.pt-BR.md) — como o transporte entrega a requisição ao seu callback.
