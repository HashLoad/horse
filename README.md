<p align="center">
  <a href="https://github.com/HashLoad/horse/blob/master/img/horse.png">
    <img alt="Horse" height="150" src="https://github.com/HashLoad/horse/blob/master/img/horse.png">
  </a>
</p><br>
<p align="center">
  <b>Horse</b> is an <a href="https://github.com/expressjs/express">Express</a>-inspired <b>web framework</b> for Delphi and Lazarus.<br>
  Designed to <b>ease</b> things up for <b>fast</b> development in a <b>minimalist</b> way and with high <b>performance</b>.
</p><br>
<p align="center">
  <a href="https://t.me/hashload">
    <img src="https://img.shields.io/badge/telegram-join%20channel-7289DA?style=flat-square">
  </a>
</p>

<p align="center">
  <i>Read this in <a href="./README.md">English</a> or <a href="./README.pt-BR.md">Português (BR)</a>.</i>
</p>

## ⚙️ Installation

Installation is done using the [`boss install`](https://github.com/HashLoad/boss) command:

```sh
boss install horse
```

*(Optional)* Install the [**Horse Wizard**](https://github.com/HashLoad/horse-wizard) for IDE integration.

## ⚡️ Quickstart Delphi

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

## ⚡️ Quickstart Lazarus

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

## 📖 Documentation

The full guide lives in [`doc/`](./doc/index.md) — a small wiki that complements the quick reference below:

| Topic | Read |
|---|---|
| First server, install paths, Delphi/Lazarus setup | [Getting Started](./doc/getting-started.md) |
| Defining routes, route params, route groups, query strings | [Routing](./doc/routing.md) |
| `THorseRequest` / `THorseResponse` — body, headers, cookies, sessions, status, streaming | [Request & Response](./doc/request-response.md) |
| Using middleware, registration order, the `Next` proc | [Middleware](./doc/middleware.md) |
| **Writing & publishing your own middleware** — skeleton, thread safety, Provider neutrality, Boss packaging | [**Writing a Middleware**](./doc/writing-middleware.md) |
| **Choosing a transport provider** — Indy (default), CrossSocket, mORMot2, Apache, ISAPI, CGI, daemons | [**Providers**](./doc/providers.md) |
| **Deploy** as Console / VCL / Daemon / Windows Service / LCL / HTTPApplication — one-page recipe | [**Deployment Cheatsheet**](./doc/deployment.md) |
| Full middleware catalogue with extended descriptions | [Middleware Ecosystem](./doc/middleware-ecosystem.md) |
| Supported Delphi / FPC versions and platforms | [Compiler Support](./doc/compiler-support.md) |

## Donate
The HashLoad organization develops and supports Horse. In order to grow the community of contributors and users, and
allow the maintainers to devote more time to the projects, [please
donate today](https://opencollective.com/hashload).

## 🤝 Contributing
See [CONTRIBUTING.md](./CONTRIBUTING.md) for how to report bugs, suggest features, and submit code or documentation changes. Bilingual EN / PT-BR docs are kept in sync — when editing one language, edit the other in the same PR.

## ⚠️ License
`Horse` is free and open-source software licensed under the [MIT License](https://github.com/HashLoad/horse/blob/master/LICENSE).
