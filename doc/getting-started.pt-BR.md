# Primeiros passos

*Leia em [English](./getting-started.md) ou [Português (BR)](./getting-started.pt-BR.md).*

Esta página leva você de um projeto em branco até um servidor Horse rodando em menos de cinco minutos. Assume que você tem **Delphi 10.4 ou mais recente** (ou **Lazarus 2.2 / FPC 3.2+**) e conexão com a internet para o Boss baixar dependências.

Se algo abaixo não se aplicar ao seu ambiente, consulte primeiro [Suporte de Compilador](./compiler-support.pt-BR.md).

---

## 1. Instalar o Boss

O [Boss](https://github.com/HashLoad/boss) é o gerenciador de pacotes que o Horse usa para distribuição. Instalação única:

```sh
# Windows (PowerShell, como administrador)
iwr https://github.com/HashLoad/boss/releases/latest/download/boss-windows.exe -OutFile boss.exe
Move-Item boss.exe C:\Windows\System32\

# Linux / macOS
curl -L https://github.com/HashLoad/boss/releases/latest/download/boss-linux -o boss
chmod +x boss
sudo mv boss /usr/local/bin/
```

Verifique:

```sh
boss --version
```

## 2. Criar o projeto e adicionar o Horse

Crie uma nova aplicação console em Delphi (ou um projeto Lazarus), salve e, num terminal aberto no diretório do projeto:

```sh
boss init -q                  # gera um boss.json
boss install horse            # adiciona o Horse ao search path do projeto
```

O Boss baixa a versão estável mais recente do Horse, coloca em `modules/horse/` e adiciona o caminho ao seu `.dproj` / `.lpi` automaticamente. Agora você pode usar `uses Horse;` em qualquer unidade.

## 3. Servidor mínimo — Delphi

```delphi
program HelloHorse;

{$APPTYPE CONSOLE}

uses
  Horse;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  WriteLn('Escutando em http://127.0.0.1:9000/ping');
  THorse.Listen(9000);
end.
```

Compile, execute e em outro terminal:

```sh
curl http://127.0.0.1:9000/ping
# pong
```

Pronto. `Ctrl-C` para parar.

## 4. Servidor mínimo — Lazarus / FPC

```pascal
program HelloHorse;

{$MODE DELPHI}{$H+}

uses
  Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  THorse.Get('/ping', @GetPing);
  THorse.Listen(9000);
end.
```

Dois pontos importantes para FPC:

- `{$MODE DELPHI}{$H+}` é obrigatório para que procedimentos anônimos e a semântica `string = AnsiString` correspondam ao Delphi.
- Callbacks de rota usam `@` porque o Lazarus distingue valor de procedimento de referência a procedimento.

## 5. Convenções de estrutura de projeto

Você não é obrigado a seguir — o Horse não impõe layout — mas os exemplos oficiais usam:

```
meu-servidor/
├── boss.json                ← manifesto do Boss
├── boss-lock.json           ← versões fixadas (auto-gerado)
├── modules/                 ← dependências instaladas pelo Boss (gitignored)
├── src/
│   ├── HelloHorse.dpr       ← entry point (registra rotas, chama Listen)
│   ├── Controllers/         ← uma unit por recurso (Users, Products, ...)
│   ├── Services/            ← regra de negócio, sem dependência do Horse
│   └── Middlewares/         ← preocupações transversais
└── tests/
    └── ...
```

Um entry point típico compõe a aplicação:

```delphi
uses
  Horse, Horse.Jhonson, Horse.CORS,
  Controllers.Users, Controllers.Products;

begin
  THorse
    .Use(Jhonson)                 // parsing de body JSON
    .Use(CORS);                    // CORS permissivo

  Controllers.Users.RegisterRoutes;
  Controllers.Products.RegisterRoutes;

  THorse.Listen(9000);
end.
```

…e cada controller expõe um `RegisterRoutes` que chama `THorse.Get/Post/Put/…`.

## 6. Mesmo código, sete formatos de deploy

O hello-world Console acima é o formato de deploy mais simples, mas **o mesmo código `THorse.Get` / `THorse.Listen` pode ser entregue em qualquer um destes**, mudando só o tipo de projeto e os Conditional Defines:

| Você quer entregar como… | Compile com | Adicionar define `HORSE_*`? | Hook de shutdown |
|---|---|---|---|
| Binário console | Delphi ou FPC | (nenhum) — o que você acabou de construir | `SetConsoleCtrlHandler` (Win) / `signal SIGTERM` (Linux) |
| App VCL desktop embutindo um servidor | Delphi | (nenhum — escreva um projeto Forms) | `FormClose` |
| Daemon Linux | Delphi ou FPC, target Linux64 | (nenhum — binário console + systemd) | `signal SIGTERM` / `fpSignal SIGTERM` |
| Serviço Windows | Delphi (Service Application) | (nenhum — escreva um projeto `TService`) | `TService.OnStop` |
| App Lazarus LCL desktop | FPC | (nenhum — escreva um projeto Lazarus) | `FormClose` |
| Módulo Apache | Delphi | `HORSE_APACHE` | (Apache é dono do ciclo de vida) |
| Extensão ISAPI IIS | Delphi | `HORSE_ISAPI` | (IIS é dono do ciclo de vida) |
| Binário CGI / FastCGI | Delphi ou FPC | `HORSE_CGI` / `HORSE_FCGI` | (host é dono do ciclo de vida) |

Para o caminho de alta performance, adicione **um** dos seguintes defines de Provider (onde a tabela diz "nenhum") pra trocar o transporte do Indy / `fphttpserver` padrão por um Provider assíncrono:

| Define do Provider | Backed by | Quando escolher |
|---|---|---|
| `HORSE_PROVIDER_CROSSSOCKET` | [`winddriver/Delphi-Cross-Socket`](https://github.com/winddriver/Delphi-Cross-Socket) (upstream) + [`cnpack/cnvcl`](https://github.com/cnpack/cnvcl) para as units CnPack/Crypto — IOCP / epoll / kqueue. Instale ambos manualmente (espelhando o setup do mORMot2). Para mTLS no servidor (`SSLVerifyPeer = True`), use a alternativa suportada [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3) que embute o CnPack e adiciona `SetCACertificateFile` + `SetVerifyPeer` num clone único. | Você prefere controle async nativo ou já depende de Delphi-Cross-Socket. Exige Delphi 10.2+. |
| `HORSE_PROVIDER_MORMOT` | [mORMot2](https://github.com/synopse/mORMot2) `THttpServer` — IOCP / epoll | Você quer compatibilidade com Delphi 7+, HTTP em modo kernel via http.sys, ou HTTP em pure-Pascal sem deps em C compilado. |

Os dois Providers são mutuamente exclusivos (um transporte por build). O alias legado `HORSE_CROSSSOCKET` continua funcionando para sempre (PATCH-HORSE-2 traduz para `HORSE_PROVIDER_CROSSSOCKET`); não há alias legado para o mORMot — ele é novo.

Receitas concretas (tipo de projeto, esqueleto de código, comandos de instalação) pra cada formato: [Cheatsheet de Deploy](./deployment.pt-BR.md), ou a forma mais longa em [Providers e Tipos de aplicação §8](./providers.pt-BR.md#8-rodando-o-crosssocket-em-cada-tipo-de-aplicação).

## 7. Próximos passos

- [Roteamento](./routing.pt-BR.md) — declarar endpoints, parâmetros de caminho, grupos de rotas.
- [Request e Response](./request-response.pt-BR.md) — ler entrada, escrever saída.
- [Middleware](./middleware.pt-BR.md) — parsing JSON, CORS, JWT, logging.
- [Providers e Tipos de aplicação](./providers.pt-BR.md) — quando trocar o transporte padrão Indy (por exemplo, para um deploy Linux de alta concorrência — veja a seção sobre CrossSocket).
- [Cheatsheet de Deploy](./deployment.pt-BR.md) — uma página pra entregar o binário em qualquer um dos sete formatos acima.

## Resolução de problemas

| Sintoma | Causa provável |
|---|---|
| `Unit not found Horse` | O `boss install horse` não rodou — execute novamente no diretório do projeto. |
| Servidor inicia e sai imediatamente | A chamada `Listen` é não-bloqueante em algumas plataformas; em uma app console garanta que a thread principal não chegue ao `end.` Use um `ReadLn` ou um handler de sinal. |
| Porta já em uso | Outro serviço está em `:9000`. Escolha outra porta ou pare o conflito. |
| `Address already in use` após um crash | O processo anterior não liberou o socket. Aguarde 30 s pelo `TIME_WAIT` ou mude a porta. |
| Compilação falha no FPC com "method-pointer expected" | Faltou o `@` antes do nome do procedimento em `THorse.Get(...)`. |
