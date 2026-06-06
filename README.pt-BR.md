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
| **Escolher um provider de transporte** — Indy (padrão), CrossSocket, mORMot2, Apache, ISAPI, CGI, daemons | [**Providers**](./doc/providers.pt-BR.md) |
| **Deploy** como Console / VCL / Daemon / Serviço Windows / LCL / HTTPApplication — receita de uma página | [**Cheatsheet de Deploy**](./doc/deployment.pt-BR.md) |
| Catálogo completo de middlewares com descrições estendidas | [Ecossistema de Middlewares](./doc/middleware-ecosystem.pt-BR.md) |
| Versões suportadas de Delphi / FPC e plataformas | [Suporte de Compilador](./doc/compiler-support.pt-BR.md) |

## Doe
A organização HashLoad desenvolve e dá suporte ao Horse. Para expandir a comunidade de colaboradores e usuários, e
permitir que os mantenedores dediquem mais tempo aos projetos, [please
donate today](https://opencollective.com/hashload).

## 🤝 Contribuindo
Veja [CONTRIBUTING.pt-BR.md](./CONTRIBUTING.pt-BR.md) para reportar bugs, sugerir features e enviar mudanças de código ou documentação. Os docs bilíngues EN / PT-BR são mantidos sincronizados — ao editar um idioma, edite o outro no mesmo PR.

## ⚠️ Licença
O `Horse` é software livre e de código aberto, licenciado sob a [Licença MIT](https://github.com/HashLoad/horse/blob/master/LICENSE).
