# Middleware

*Leia em [English](./middleware.md) ou [Português (BR)](./middleware.pt-BR.md).*

Middleware permite envolver handlers de rota com lógica transversal — parsing de body, autenticação, headers de CORS, logging, formatação de erros. Middleware do Horse tem o mesmo formato do middleware do Express: um procedimento que recebe a requisição, a resposta e um `Next` proc.

Para o catálogo de pacotes (JSON, JWT, CORS, etc.), veja [Ecossistema de Middlewares](./middleware-ecosystem.pt-BR.md).

---

## O modelo

Um middleware é um procedimento com esta forma:

```delphi
procedure MyMiddleware(
  Req:  THorseRequest;
  Res:  THorseResponse;
  Next: {$IF DEFINED(FPC)}TNextProc{$ELSE}TProc{$ENDIF});
```

Ele pode:

1. **Inspecionar ou mutar** a requisição e a resposta.
2. **Chamar `Next()`** para continuar a cadeia (passa controle ao próximo middleware ou, no fim, ao handler).
3. **Não chamar `Next()`** para fazer curto-circuito — útil em falhas de auth.
4. **Lançar uma exception** — `EHorseCallbackInterrupted` encerra a cadeia silenciosamente; qualquer outra vira `500`.

## Um primeiro middleware

```delphi
procedure Logger(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Start: TDateTime;
begin
  Start := Now;
  try
    Next();
  finally
    WriteLn(Format('[%s] %s %s -> %d (%dms)',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Start),
       Req.Method, Req.PathInfo, Res.Status,
       MilliSecondsBetween(Now, Start)]));
  end;
end;

// Registre no startup, antes das rotas
THorse.Use(Logger);
```

O padrão `try / finally` é a forma canônica de envolver a requisição inteira — o timer dispara sucedeu, falhou ou fez curto-circuito.

## Registro

`THorse.Use(...)` aceita:

```delphi
THorse.Use(MyMiddleware);              // aplica a toda requisição
THorse.Use('/api', MyMiddleware);      // aplica apenas a /api/*
THorse.Group.Prefix('/admin')
  .Use(RequireAuth)                    // aplica apenas a /admin/*
  .Get('/users', ListUsers);
```

**A ordem de registro importa.** O middleware roda na ordem em que foi registrado, num modelo cebola aninhado:

```
THorse.Use(A);                         // mais externo
THorse.Use(B);
THorse.Use(C);                         // mais interno
THorse.Get('/x', Handler);
```

Fluxo da requisição:
```
A → B → C → Handler → C → B → A
```

…onde o lado direito de cada seta é o código que roda após o `Next()` retornar. Então `A` roda primeiro e tem a última palavra; `C` envolve o handler mais de perto.

**Implicação prática:** registre middleware em ordem *mais-externo-primeiro*. Se o logging precisa ver o código de status final, registre `Logger` *primeiro*. Se o handler de exceção precisa pegar erros não tratados, registre `HandleException` *primeiro*. Se o parsing de body é necessário para tudo abaixo, registre `Jhonson` cedo.

Um bloco típico de startup:

```delphi
THorse
  .Use(HandleException)    // 1. mais externo — exceções viram respostas limpas
  .Use(Logger)             // 2. logar toda requisição concluída
  .Use(CORS)               // 3. adicionar headers CORS
  .Use(Jhonson)            // 4. parsear body JSON
  .Use(JWT(SECRET))        // 5. autenticar (próximo do handler)

THorse.Get('/users', ListUsers);
```

## Interrompendo a cadeia

Pule o `Next()` para parar a cadeia sem chamar o handler. O caso mais comum é autenticação:

```delphi
procedure RequireApiKey(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  if Req.Headers['X-Api-Key'] <> 'expected-key' then
  begin
    Res.Status(THTTPStatus.Unauthorized).Send('API Key invalida ou ausente');
    Exit;                              // cadeia para; handler nao roda
  end;
  Next();                              // valido — continua
end;
```

Ou lance `EHorseCallbackInterrupted` (definida em `Horse.Exception.Interrupted`) para o mesmo efeito — útil quando você já preparou a resposta e quer só encerrar a cadeia:

```delphi
uses Horse.Exception.Interrupted;

procedure HandlePreflight(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  if Req.Method = 'OPTIONS' then
  begin
    Res.Status(THTTPStatus.NoContent);
    raise EHorseCallbackInterrupted.Create;   // pula o resto da cadeia
  end;
  Next();
end;
```

É assim que `Horse.CORS` implementa o preflight hoje.

## Criando o seu — exemplo completo

> Procurando o guia completo de autoria — neutralidade a Provider, thread safety, empacotamento pra Boss, testes na matriz de Providers? Veja **[Criando um Middleware](./writing-middleware.pt-BR.md)**. O trecho abaixo é a versão rápida.

Um rate limiter simples, por IP, por minuto:

```delphi
unit Horse.Middleware.RateLimit;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, Horse;

type
  THorseRateLimit = class
  private
    class var FBuckets: TDictionary<string, Integer>;
    class var FLock: TCriticalSection;
    class var FLimit: Integer;
    class var FResetEvery: Integer;        // segundos
  public
    class procedure Init(ALimit: Integer = 60; AWindowSeconds: Integer = 60); static;
    class procedure Done; static;
    class function Middleware: THorseCallback; static;
  end;

implementation

class procedure THorseRateLimit.Init(ALimit, AWindowSeconds: Integer);
begin
  FLimit      := ALimit;
  FResetEvery := AWindowSeconds;
  FBuckets    := TDictionary<string, Integer>.Create;
  FLock       := TCriticalSection.Create;
end;

class procedure THorseRateLimit.Done;
begin
  FreeAndNil(FBuckets);
  FreeAndNil(FLock);
end;

class function THorseRateLimit.Middleware: THorseCallback;
begin
  Result :=
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      IP:    string;
      Count: Integer;
    begin
      // IP do cliente — em producao use X-Forwarded-For atras de proxy
      IP := Req.Headers['X-Real-IP'];
      if IP = '' then IP := Req.RawWebRequest.RemoteAddr;
      if IP = '' then IP := 'unknown';

      FLock.Acquire;
      try
        FBuckets.TryGetValue(IP, Count);
        Inc(Count);
        FBuckets.AddOrSetValue(IP, Count);
      finally
        FLock.Release;
      end;

      if Count > FLimit then
      begin
        Res.AddHeader('X-RateLimit-Remaining', '0');
        Res.Status(THTTPStatus.TooManyRequests).Send('Rate limit excedido');
        Exit;
      end;

      Res.AddHeader('X-RateLimit-Remaining', IntToStr(FLimit - Count));
      Next();
    end;
end;

end.
```

Uso:

```delphi
uses Horse, Horse.Middleware.RateLimit;

THorseRateLimit.Init(120, 60);   // 120 req/min
THorse.Use(THorseRateLimit.Middleware);
THorse.Listen(9000);
```

(Em produção, atualize os buckets a cada `FResetEvery` segundos; este esqueleto omite a thread de timer.)

## Quando escrever middleware vs colocar lógica no handler

- **Preocupação transversal que aplica a muitas rotas** → middleware (auth, logging, CORS, parsing de body).
- **Lógica específica de um recurso** → handler ou método do controller.
- **Lógica específica de poucas rotas dentro de um grupo** → middleware no escopo do grupo (`THorse.Group.Use(...)`).

Se você repetir as mesmas seis linhas no início de cada handler, isso é um middleware.

## Armadilhas comuns

| Sintoma | Causa provável |
|---|---|
| Handler roda duas vezes | Chamando `Next()` duas vezes no mesmo middleware. |
| Handler nunca roda | Esqueceu o `Next()` e também não enviou resposta — requisição trava até timeout. |
| Headers do middleware não aparecem | Middleware chamou `Next()` *antes* de definir o header; a resposta já foi enviada. Defina headers antes do `Next()` ou use o wrap `try/finally`. |
| Ordem não bate com a expectativa | Middleware registrado tarde, depois de algumas rotas. Registre todos os middlewares antes das rotas. |
| `EHorseCallbackInterrupted` aparece como erro no log | Um `on E: Exception` genérico em algum lugar está pegando. Capture `EHorseCallbackInterrupted` *antes* do handler genérico. |

## Veja também

- [Ecossistema de Middlewares](./middleware-ecosystem.pt-BR.md) — `Jhonson`, `CORS`, `JWT`, `compression`, `handle-exception` e vários pacotes da comunidade.
- [Request e Response](./request-response.pt-BR.md) — a API que você usa dentro do middleware.
- [Roteamento](./routing.pt-BR.md) — como `THorse.Use` difere de `THorse.Get/Post/...`.
