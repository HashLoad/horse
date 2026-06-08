# Criando um Middleware do Horse

*Leia em [English](./writing-middleware.md) ou [Português (BR)](./writing-middleware.pt-BR.md).*

Este guia percorre a implementação de um middleware Horse de qualidade de produção — do esqueleto mínimo até um pacote Boss publicado. Para o básico de *usar* middleware, veja [Middleware](./middleware.pt-BR.md). Para entender por que alguns padrões funcionam em todos os Providers e outros quebram, veja [Providers e Tipos de aplicação](./providers.pt-BR.md).

---

## 1. O que "qualidade de produção" significa aqui

Cinco propriedades — seu middleware deve atender todas se você for publicar para terceiros:

1. **Neutro a Provider** — funciona no Indy (padrão Delphi), `fphttpserver` (padrão FPC), CrossSocket e qualquer Provider futuro. Apache / ISAPI / CGI também.
2. **Entre compiladores** — compila no Delphi 10.4 Sydney ou mais recente **e** FPC 3.2 ou mais recente.
3. **Thread-safe** — handlers do Horse rodam em threads diferentes em Providers diferentes; estado compartilhado precisa de guarda.
4. **Configurável** — aceita opções sem depender de variáveis globais quando possível.
5. **Instalável via Boss** — tem um `boss.json` para que consumidores possam `boss install` dele.

Se você só precisa de um middleware único pra sua própria app, um `procedure` top-level no código do projeto resolve — pule para a §2 (esqueleto) e §6 (curto-circuito), ignore o resto. O guia completo é pro caso de empacotamento para reuso.

---

## 2. O esqueleto mínimo

```pascal
unit Horse.Middleware.MyThing;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)} SysUtils {$ELSE} System.SysUtils {$ENDIF},
  Horse;

procedure MyMiddleware(
  Req: THorseRequest;
  Res: THorseResponse;
  Next: {$IF DEFINED(FPC)}TNextProc{$ELSE}TProc{$ENDIF});

implementation

procedure MyMiddleware(Req: THorseRequest; Res: THorseResponse;
  Next: {$IF DEFINED(FPC)}TNextProc{$ELSE}TProc{$ENDIF});
begin
  // antes — inspecionar / mutar Req, preparar estado, falhar rápido
  try
    Next();
  finally
    // depois — inspecionar / mutar Res, desmontar estado
  end;
end;

end.
```

O wrap `try / finally` é a forma canônica — permite que o código "depois" veja o status da resposta independente de sucesso, falha ou curto-circuito. Use sempre que precisar das duas fases.

`THorseCallback` (o tipo usado por `THorse.Use`) embrulha essa forma de procedure num `TProc<THorseRequest, THorseResponse, TProc>` no Delphi ou num `THorseProc` no FPC.

---

## 3. Adicionando configuração

Três padrões, em complexidade crescente. Escolha o mais leve que serve.

### a) Sem config — procedure pura

Pra middleware "sempre-ligado" sem botões (ex. um logger pra stdout):

```pascal
THorse.Use(MyMiddleware);   // passe a procedure diretamente
```

### b) Record de config + factory function

Pra middleware onde a maioria quer defaults mas alguns querem override:

```pascal
type
  TMyConfig = record
    Threshold: Integer;
    Verbose:   Boolean;
    class function Default: TMyConfig; static;
  end;

function MyMiddleware: THorseCallback; overload;                          // usa defaults
function MyMiddleware(const AConfig: TMyConfig): THorseCallback; overload;
```

Uso:

```pascal
THorse.Use(MyMiddleware);                                     // defaults
THorse.Use(MyMiddleware(TMyConfig.Default.Set(... )));        // customizado
```

Padrão usado pelo `Horse.Middleware.SecurityHeaders` e `Horse.Middleware.RequestGuard`.

### c) Classe estática com factory `.New`

Pra middleware com estado substancial (buckets de rate-limiter, pools de conexão):

```pascal
type
  TMyMiddleware = class
  public
    class function New: THorseCallback; overload;
    class function New(const AConfig: TMyConfig): THorseCallback; overload;
  end;

THorse.Use(TMyMiddleware.New);
```

A factory retorna um `THorseCallback` (uma procedure anônima / referência a procedure) — o caller passa pro `THorse.Use(...)`.

**Evite variáveis globais de unit para configuração.** Estado por-processo compartilhado entre todos os consumidores do pacote é a causa da maioria dos bugs de middleware de terceiros (ex. o `horse-cors` historicamente usava esse padrão, e é por isso que você não consegue rodar dois servidores Horse com políticas CORS diferentes no mesmo processo). Use um record/classe que captura a config dentro do closure.

---

## 4. Estado por-requisição vs compartilhado

| Tipo de estado | Onde vive | Seguro? |
|---|---|---|
| Objeto parseado do body | `Req.Body(AObject)` — o Horse é dono e libera | Sim |
| Valor computado usado downstream | Um campo num record dentro de `Req.Body<T>` (ou variável de closure que captura o request) | Sim |
| Contador, cache, registry | Class var ou unit var | Só com lock |

Pra passar um valor pro próximo middleware ou pro handler, o padrão mais limpo é usar `Req.Body<T>` se fizer sentido semântico, ou escrever o valor num response header que middleware downstream lê.

Evite: jogar estado por-request num `TThreadList<string,T>` global chaveado por algo derivado da requisição. Funciona mas é desperdício — o tempo de vida do objeto request já é o que você quer; a thread em que ele roda não importa.

---

## 5. Thread safety

Handlers do Horse rodam em threads diferentes dependendo do Provider:

| Provider | Onde os handlers rodam | Implicação |
|---|---|---|
| Indy (Delphi self-hosted) | Uma thread Indy por conexão | Centenas de threads sob carga |
| `fphttpserver` (FPC self-hosted) | Uma thread por conexão | Igual ao Indy |
| CrossSocket | Threads de IO (pool fixo, `CPUCount*2+1`) ou worker threads (`THorseWorkerPool`, 4–64) | O mesmo handler pode rodar em threads diferentes em requests diferentes |
| Apache / ISAPI / CGI | Worker thread do host | Varia pela configuração do host |

Em todo Provider, **uma única invocação de handler roda numa única thread do começo ao fim**. Você não precisa de locks dentro de uma requisição. Mas estado compartilhado entre requests precisa de proteção.

**Regra prática**: toda `class var` ou `var` de unit que seu middleware introduz precisa de proteção explícita (`TCriticalSection`, `TMonitor`, `TThreadList`, etc.). Mesmo `Inc(Counter)` numa variável compartilhada não é atômico no hot path.

Pra contadores de alta frequência, prefira operações atômicas:

```pascal
{$IF DEFINED(FPC)}
  Result := InterlockedIncrement(FCounter);
{$ELSE}
  Result := TInterlocked.Increment(FCounter);
{$IFEND}
```

Pra mapas, `System.SyncObjs.TMonitor` na instância do dicionário é o padrão correto mais simples:

```pascal
TMonitor.Enter(FBuckets);
try
  if not FBuckets.TryGetValue(IP, Count) then Count := 0;
  Inc(Count);
  FBuckets.AddOrSetValue(IP, Count);
finally
  TMonitor.Exit(FBuckets);
end;
```

---

## 6. Curto-circuito limpo

Duas formas de encerrar a cadeia cedo.

### a) Setar a resposta e dar `Exit` sem chamar `Next()`

Mais claro pros casos "auth falhou" / "rate limited":

```pascal
if Req.Headers['X-Api-Key'] <> SECRET then
begin
  Res.Status(THTTPStatus.Unauthorized).Send('bad key');
  Exit;     // cadeia para aqui; handler nao roda
end;
Next();
```

### b) Lançar `EHorseCallbackInterrupted`

Escolha certa quando você está aninhado dentro de um `try / except` no seu próprio código e precisa desfazer limpo, ou quando um helper existente já setou a resposta e você só precisa quebrar a cadeia:

```pascal
uses Horse.Exception.Interrupted;
...
  Res.Status(THTTPStatus.NoContent);
  raise EHorseCallbackInterrupted.Create;
```

**Nunca capture `EHorseCallbackInterrupted` num `on E: Exception do` genérico** — deixe propagar. O pipeline runner do Horse captura explicitamente e trata como sinal normal de fim-de-cadeia. Um catch genérico transforma todo curto-circuito num "erro" logado.

---

## 7. Dependência de ordem

Se seu middleware depende de outro ter rodado antes, documente alto no topo da unit:

```pascal
unit Horse.Middleware.MyValidator;

{
  EXIGE: Horse.Jhonson PRECISA ser registrado ANTES deste middleware,
  porque lemos Req.Body<TJSONObject> que o Jhonson popula.

  ORDEM RECOMENDADA:
    THorse.Use(HandleException);    // mais externo
    THorse.Use(Logger);
    THorse.Use(Jhonson);
    THorse.Use(MyValidator);        // aqui
    THorse.Use(JWT(SECRET));
}
```

Cadeia comum de dependências entre os middlewares oficiais:

```
HandleException → Logger → CORS → Jhonson → JWT → (seu middleware) → Routes
```

Se você reordenar, coisas estranhas acontecem — ex. um `Logger` registrado *depois* da rota pode não enxergar o status final; `Jhonson` registrado *depois* do JWT significa que o JWT não enxerga um body parseado.

Se seu middleware não tem dependência de ordem, diga isso. Silêncio é ambíguo; "independente de ordem" explícito tranquiliza.

---

## 8. Compatibilidade com Provider — a grande

É aqui que a maioria dos middlewares de terceiros quebra no CrossSocket. **Evite estes anti-padrões:**

### Anti-padrão A: cast de `Req.RawWebRequest` pra uma subclasse específica do Indy

```pascal
// NAO faca — quebra no CrossSocket, FPC, Apache, ISAPI, …
LIndyReq := Req.RawWebRequest as TIdHTTPAppRequest;
LIndyReq.PostStream.Position := 0;
```

`Req.RawWebRequest` é `TWebRequest` (Delphi) ou `TRequest` (FPC). A subclasse concreta varia por Provider: Indy provê `TIdHTTPAppRequest`; CrossSocket provê `TCrossSocketWebRequest`; Apache provê `TApacheRequest`. Qualquer cast amarra seu middleware a um Provider.

### Anti-padrão B: assumir uma biblioteca específica

```pascal
// NAO — assume Indy e seu parsing especifico de Body
LField := Req.RawWebRequest.Files.Items[0].FieldName;
```

`Files` existe no `TIdHTTPAppRequest` do Indy mas pode não ser populado corretamente no caminho do CrossSocket. Use `Req.ContentFields` — nível Horse, neutro a Provider.

### Boa prática: fique na API `THorseRequest` / `THorseResponse`

Neutra a Provider por design:

| O que você quer | Use |
|---|---|
| Método HTTP (verbo) | `Req.Method` (PATCH-REQ-10) ou `Req.MethodType` |
| Path | `Req.PathInfo` |
| Header | `Req.Headers['X-...']` |
| Query string | `Req.Query['name']` |
| Param de path | `Req.Params['id']` |
| Cookie | `Req.Cookie['session']` |
| Body como string | `Req.Body` (cacheado no CrossSocket — PATCH-REQ-9) |
| Body como objeto parseado | `Req.Body<TJSONObject>` (depois do Jhonson) |
| IP do cliente | `Req.RawWebRequest.RemoteAddr` *(sem alternativa de alto nível — inevitável)* |

### Quando realmente precisa alcançar o objeto subjacente

Exemplos: inspeção de certificado cliente TLS, acesso a stream cru, percorrer cadeia `X-Forwarded-For`. O padrão de adaptador híbrido (PATCH-REQ-8) garante que `Req.RawWebRequest.Method` / `.Host` / `.RemoteAddr` / `.GetFieldByName(...)` funcionam em todo Provider. Fique na API abstrata de `TWebRequest`:

```pascal
LIP := Req.RawWebRequest.GetFieldByName('X-Forwarded-For');
if LIP = '' then
  LIP := Req.RawWebRequest.RemoteAddr;
```

Esse trecho funciona em Indy, `fphttpserver`, CrossSocket, Apache e ISAPI sem mudanças.

Pro lado da resposta: `Res.AddHeader` / `Res.RawWebResponse.SetCustomHeader` funcionam em todo lugar; `Res.AddHeader` é preferido (popula o `FCustomHeaders` do próprio Horse que o bridge do CrossSocket lê).

---

## 9. Considerações entre compiladores

Três armadilhas concretas.

### a) Tipo de `Next` difere

```pascal
procedure MyMiddleware(Req: THorseRequest; Res: THorseResponse;
  Next: {$IF DEFINED(FPC)}TNextProc{$ELSE}TProc{$ENDIF});
```

Sempre envelope `Next` com essa conditional — `TProc` no Delphi, `TNextProc` (um alias de method-of-object declarado em `Horse.Callback`) no FPC.

### b) Procedures anônimas

Delphi suporta procedures anônimas em todo lugar. FPC suporta desde 3.2 em `{$MODE DELPHI}`, mas pra compatibilidade com FPC 3.0 (ainda comum em alguns Lazarus antigos) prefira procedures top-level:

```pascal
procedure MyMiddleware(...);   // top-level — funciona nos dois
begin
  ...
end;

// Delphi:
THorse.Use(MyMiddleware);

// FPC:
THorse.Use(@MyMiddleware);     // o @ e obrigatorio
```

### c) Codepage de `string`

`string` é UTF-16 no Delphi, UTF-8 no FPC. Pra headers HTTP e bodies, o Horse normaliza pra UTF-8 na fronteira da requisição — você geralmente não precisa pensar nisso dentro do middleware. **Mas** se seu middleware faz trabalho byte-a-byte (computar hashes, assinar, contar bytes do payload), `Length(SomeString)` retorna a contagem de code-units que difere entre plataformas. Use `Length(TEncoding.UTF8.GetBytes(SomeString))` quando precisar da contagem de bytes do protocolo.

---

## 10. Testando seu middleware

Três camadas de teste, cada uma pegando uma classe diferente de bug:

### Teste unitário — rápido, estreito

Chame a procedure do middleware diretamente com `THorseRequest` / `THorseResponse` mockados. Feedback rápido mas não pega problemas de integração.

### Teste de integração — mais lento, mais amplo

Registre uma rota de teste com seu middleware, suba um servidor Horse real numa porta de teste, dispare requisições HTTP com `TIdHTTP` (Delphi) ou `THTTPClient` (Delphi 10.4+) ou `TFPHTTPClient` (FPC). Pega dependência de ordem, problemas de threading e bugs específicos de Provider.

```pascal
// Servidor de teste
THorse.Use(MyMiddleware);
THorse.Get('/test', procedure(Req: THorseRequest; Res: THorseResponse)
                    begin Res.Send('ok'); end);
THorse.Listen(9099);

// Cliente de teste
LResp := TIdHTTP.Create.Get('http://127.0.0.1:9099/test');
Assert(LResp = 'ok');
```

### Matriz de Providers — pega os raros

No mínimo, rode seus testes de integração sob **Indy** (padrão — sem define) e **CrossSocket** (`HORSE_PROVIDER_CROSSSOCKET`, ou o alias legado `HORSE_CROSSSOCKET`). Se você declara suporte a FPC, rode no FPC também com o padrão `fphttpserver`. O padrão de teste de referência fica em [`horse-provider-crosssocket/samples/tests/`](https://github.com/freitasjca/horse-provider-crosssocket/tree/master/samples/tests) — 32 testes HTTP black-box cobrindo roteamento, tratamento de body, requests concorrentes, CORS e precedência de shadow fields. Copie o padrão.

---

## 11. Empacotando para Boss

### `boss.json` mínimo

```json
{
  "name": "horse-middleware-mything",
  "description": "Descricao de uma linha do que o middleware faz",
  "version": "1.0.0",
  "homepage": "https://github.com/your-org/horse-middleware-mything",
  "license": "MIT",
  "mainsrc": "src/",
  "browsingpath": "src/",
  "dependencies": {
    "github.com/HashLoad/horse": ">=3.1.95"
  }
}
```

Suba a dependência de `horse` pra `>=3.1.96` se seu middleware usa PATCH-REQ-9 (`Req.Body: string` cacheado) ou PATCH-REQ-10 (`Req.Method: string`).

### Layout de repo recomendado

```
horse-middleware-mything/
├── boss.json
├── LICENSE
├── README.md                              ← curto; exemplo, instalação, link pro wiki
├── src/
│   └── Horse.Middleware.MyThing.pas
└── samples/
    └── tests/
        ├── HorseMyThingTestServer.dpr     ← inicia servidor, sai por sinal
        └── HorseMyThingTestClient.dpr     ← exit code = num. de falhas
```

Tag de release correspondente ao `boss.json.version` (ex. `v1.0.0`) e o Boss resolve:

```sh
boss install github.com/your-org/horse-middleware-mything
```

### Nomenclatura de versão

O Boss v3.0.12 e mais recente parseia **só semver pontuado** — `MAJOR.MINOR.PATCH`. **Sem hifens, sem sufixos de pré-release**. Então:

- ✅ `1.0.0`, `1.0.1`, `2.3.4`
- ❌ `1.0.0-beta.1`, `1.0.0-rc1`, `2.3.4-alpha`

Se precisar de pré-releases pra teste interno, use uma branch separada (não uma tag).

---

## 12. Publicando

Depois que o pacote compila e os testes passam na matriz de Providers:

1. **Tag o release** no GitHub correspondente à versão do `boss.json`.
2. **Crie um GitHub Release** com notas — `gh release create v1.0.0 --notes "..."` funciona.
3. **Abra um PR contra [`middleware-ecosystem.pt-BR.md`](./middleware-ecosystem.pt-BR.md)** adicionando uma linha na tabela "Middlewares de terceiros" — **nas duas versões EN e PT-BR** (veja [CONTRIBUTING](../CONTRIBUTING.pt-BR.md) sobre a regra bilíngue).
4. **Anuncie no canal do Telegram** (`@hashload`) pra comunidade ver.

---

## Middlewares de referência

Os dois middlewares mantidos junto com `horse-provider-crosssocket` são as referências canônicas mais limpas para os padrões deste guia:

| Repo | Padrão demonstrado | Particularmente útil pra |
|---|---|---|
| [`horse-request-guard`](https://github.com/freitasjca/horse-request-guard) | Classe com factory `.New`, record de config, validação em camadas | Criar middleware que faz curto-circuito em entrada inválida com status HTTP específicos (400, 405, 413, 414, 431) |
| [`horse-security-headers`](https://github.com/freitasjca/horse-security-headers) | Injeção de header no lado da resposta, writes de header neutros a Provider | Criar middleware que muta a resposta depois do `Next()` retornar |

Ambos vêm com suíte de testes de integração que exercita o middleware em Indy e CrossSocket — clone, leia, copie o padrão de teste.

---

## Veja também

- [Middleware](./middleware.pt-BR.md) — *usando* middleware (lado do consumidor).
- [Providers e Tipos de aplicação](./providers.pt-BR.md) — por que neutralidade a Provider importa, e o que `Req.RawWebRequest` aponta em cada Provider.
- [Suporte de Compilador](./compiler-support.pt-BR.md) — exigências de versão Delphi / FPC e os guards de versão de compilador a usar.
- [Ecossistema de Middlewares](./middleware-ecosystem.pt-BR.md) — pacotes publicados que você pode estudar pra mais padrões.
- [`CONTRIBUTING.pt-BR.md`](../CONTRIBUTING.pt-BR.md) — estilo de código, a regra bilíngue, o processo de PR.
