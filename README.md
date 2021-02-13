<p align="center">
  <a href="https://github.com/HashLoad/horse/blob/master/img/horse.png">
    <img alt="Horse" height="150" src="https://github.com/HashLoad/horse/blob/master/img/horse.png">
  </a>  
</p><br>
<p align="center">
  <b>Horse</b> is an <a href="https://github.com/expressjs/express">Express</a> inspired <b>web framework</b> for Delphi.<br>Designed to <b>ease</b> things up for <b>fast</b> development in a <b>minimalist</b> way and with high <b>performance</b>.
</p><br>
<p align="center">
  <a href="https://t.me/hashload">
    <img src="https://img.shields.io/badge/telegram-join%20channel-7289DA?style=flat-square">
  </a>
</p>

## ⚙️ Installation
Installation is done using the [`boss install`](https://github.com/HashLoad/boss) command:
``` sh
$ boss install horse
```
* (Optional) Install [**wizard**](https://github.com/HashLoad/horse-wizard)

## ⚡️ Quickstart
```delphi
uses Horse;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

## 🧬 Official Middlewares

For a more _maintainable_ middleware _ecosystem_, we've put official [middlewares](https://docs.gofiber.io/middleware) into separate repositories:

| Middleware | Delphi | Lazarus |
| ------------------------------------------------------------------- | -------------------- | --------------------------- |
|  [horse/json](https://github.com/HashLoad/jhonson)                  | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/basic-auth](https://github.com/HashLoad/horse-basic-auth)   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/cors](https://github.com/HashLoad/horse-cors)               | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/stream](https://github.com/HashLoad/horse-octet-stream)     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/jwt](https://github.com/HashLoad/horse-jwt)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [horse/exception](https://github.com/HashLoad/handle-exception)    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/logger](https://github.com/HashLoad/horse-logger)           | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/compression](https://github.com/HashLoad/horse-compression) | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |

## 🌱 Third Party Middlewares

This is a list of middlewares that are created by the Horse community, please create a PR if you want to see yours!

| Middleware | Delphi | Lazarus |
| ---------------------------------------------------------------------------------- | -------------------- | --------------------------- |
|  [bittencourtthulio/etag](https://github.com/bittencourtthulio/Horse-ETag)         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [bittencourtthulio/paginate](https://github.com/bittencourtthulio/Horse-Paginate) | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [gabrielbaltazar/gbswagger](https://github.com/gabrielbaltazar/gbswagger)         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [willhubner/socketIO](https://github.com/WillHubner/Horse-SocketIO)               | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/ratelimit](https://github.com/dliocode/horse-ratelimit)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/slowdown](https://github.com/dliocode/horse-slowdown)                   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [giorgiobazzo/upload](https://github.com/giorgiobazzo/horse-upload)               | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/query](https://github.com/dliocode/horse-query)                         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |

## ⚠️ License

`Horse` is free and open-source software licensed under the [MIT License](https://github.com/HashLoad/horse/blob/master/LICENSE). 

## 📐 Tests

![tests](https://github.com/GlerystonMatos/horse/workflows/tests/badge.svg) ![Console Coverage ](https://img.shields.io/badge/console%20coverage-45%25-blue) ![VCL Coverage ](https://img.shields.io/badge/vcl%20coverage-43%25-blue)
