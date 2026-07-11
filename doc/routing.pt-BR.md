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

## Middlewares por Rota (Locais)

Você pode passar uma cadeia de middlewares específicos para uma única rota em formato de array (`array of THorseCallback`):

```delphi
// Sintaxe Estática com array de middlewares
THorse.Get('/admin/dashboard', [MiddlewareAutenticacao, MiddlewareLogAcesso],
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    Res.Send('Painel Administrativo');
  end);

// Sintaxe Fluente de Rota com array de middlewares
THorse.Route('/relatorios')
  .Get([MiddlewareAutenticacao, MiddlewareLogAcesso], GetRelatorioHandler)
  .Post([MiddlewareAutenticacao], PostRelatorioHandler);
```

Os middlewares de rota são executados após os middlewares globais e após os middlewares do grupo da rota, mas antes do callback final (handler) da rota.

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

## Roteador Radix (Opcional - Performance Extrema)

Para aplicações de grande escala ou APIs de alta performance com centenas de rotas, o Horse inclui opcionalmente um roteador baseado em **Árvore de Prefixos (Radix Tree)**.

Ao contrário do roteador linear padrão que busca as rotas em $O(N)$ (buscando uma a uma até casar), o roteador Radix analisa o caminho da URL em $O(K)$, onde $K$ é o comprimento da rota digitada. Isso resulta em throughput extremo e tempo de resposta constante, independentemente de quantas rotas existam na sua aplicação.

### Como ativar o Radix Router

Chame o método estático `THorse.UseRadixRouter` logo no início da inicialização da sua aplicação (antes de registrar qualquer rota):

```delphi
begin
  THorse.UseRadixRouter;

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

## Roteamento Avançado (Parâmetros Opcionais e Regex)

A partir da versão atual, o Horse suporta nativamente restrições de rotas baseadas em Expressões Regulares e parâmetros opcionais de caminho de URL em todos os roteadores (`THorseRouterTree` e `THorseRadixRouter`).

### 1. Parâmetros Opcionais
Ao adicionar um ponto de interrogação `?` ao final de um parâmetro de caminho (ex: `:id?`), você sinaliza ao Horse que esse parâmetro pode estar ausente na URL requisitada:

```delphi
THorse.Get('/users/:id?',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    if Req.Params.Items['id'].IsEmpty then
      Res.Send('Retornando todos os usuarios')
    else
      Res.Send('Retornando o usuario ' + Req.Params.Items['id']);
  end);
```

* Requisitar `GET /users/123` -> Match! Retorna `Retornando o usuario 123` (`Req.Params.Items['id'] = '123'`)
* Requisitar `GET /users` -> Match! Retorna `Retornando todos os usuarios` (`Req.Params.Items['id'] = ''`)

### 2. Restrições com Expressões Regulares (Regex)
Você pode restringir a correspondência de um parâmetro passando um padrão Regex entre parênteses logo após o nome do parâmetro:

```delphi
// Esta rota so sera acionada se o parametro 'id' contiver apenas numeros decimais
THorse.Get('/users/:id(\d+)',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    Res.Send('Usuario numerico: ' + Req.Params.Items['id']);
  end);
```

* Requisitar `GET /users/456` -> Match! Retorna `Usuario numerico: 456`
* Requisitar `GET /users/john` -> Não coincide com a Regex (continua a busca por outras rotas parametrizadas compatíveis ou retorna `404 Not Found`).

### 3. Coexistência e Ambiguidade de Rotas
O mecanismo de roteamento resolve a ambiguidade de rotas dando precedência da seguinte forma (da mais específica para a mais genérica):
1. **Rota Estática Exata:** ex: `GET /users/new`
2. **Rota Paramétrica com Regex:** ex: `GET /users/:id(\d+)`
3. **Rota Paramétrica Opcional/Texto Geral:** ex: `GET /users/:id?`

---

## Veja também

- [Request e Response](./request-response.pt-BR.md) — o que fazer dentro do callback.
- [Middleware](./middleware.pt-BR.md) — `THorse.Use`, o `Next` proc, ordem de registro.
- [Providers](./providers.pt-BR.md) — como o transporte entrega a requisição ao seu callback.
