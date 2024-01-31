<p align="center">
  <a href="https://github.com/HashLoad/horse/blob/master/img/horse.png">
    <img alt="Horse" height="150" src="https://github.com/HashLoad/horse/blob/master/img/horse.png">
  </a>  
</p><br>
<p align="center">
  <b>Horse</b> is an <a href="https://github.com/expressjs/express">Express</a> inspired <b>web framework</b> for Delphi and Lazarus.<br>Designed to <b>ease</b> things up for <b>fast</b> development in a <b>minimalist</b> way and with high <b>performance</b>.
</p><br>
<p align="center">
  <a href="https://t.me/hashload">
    <img src="https://img.shields.io/badge/telegram-join%20channel-7289DA?style=flat-square">
  </a>
</p>

## ⚙️ Installation
Installation is done using the [`boss install`](https://github.com/HashLoad/boss) command:
``` sh
boss install horse
```
* (Optional) Install [**wizard**](https://github.com/HashLoad/horse-wizard)

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

## 🧬 Official Middlewares

For a more _maintainable_ middleware _ecosystem_, we've put official middlewares into separate repositories:

| Middleware | Delphi | Lazarus |
| ------------------------------------------------------------------- | -------------------- | --------------------------- |
|  [horse/json](https://github.com/HashLoad/jhonson)                  | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/basic-auth](https://github.com/HashLoad/horse-basic-auth)   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/cors](https://github.com/HashLoad/horse-cors)               | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/stream](https://github.com/HashLoad/horse-octet-stream)     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/jwt](https://github.com/HashLoad/horse-jwt)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/exception](https://github.com/HashLoad/handle-exception)    | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/logger](https://github.com/HashLoad/horse-logger)           | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [horse/compression](https://github.com/HashLoad/horse-compression) | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |

## 🌱 Third Party Middlewares

This is a list of middlewares that are created by the Horse community, please create a PR if you want to see yours!

| Middleware | Delphi | Lazarus |
| ---------------------------------------------------------------------------------------------------------- | -------------------- | --------------------------- |
|  [bittencourtthulio/etag](https://github.com/bittencourtthulio/Horse-ETag)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [bittencourtthulio/paginate](https://github.com/bittencourtthulio/Horse-Paginate)                         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [bittencourtthulio/cachecontrol](https://github.com/bittencourtthulio/horse-cachecontrol)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [gabrielbaltazar/gbswagger](https://github.com/gabrielbaltazar/gbswagger)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [willhubner/socketIO](https://github.com/WillHubner/Horse-SocketIO)                                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/ratelimit](https://github.com/dliocode/horse-ratelimit)                                         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/slowdown](https://github.com/dliocode/horse-slowdown)                                           | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [giorgiobazzo/upload](https://github.com/giorgiobazzo/horse-upload)                                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/query](https://github.com/dliocode/horse-query)                                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [CarlosHe/healthcheck](https://github.com/CarlosHe/horse-healthcheck)                                     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [CarlosHe/staticfiles](https://github.com/CarlosHe/horse-staticfiles)                                     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [CachopaWeb/horse-server-static](https://github.com/CachopaWeb/horse-server-static)                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [arvanus/horse-exception-logger](https://github.com/arvanus/horse-exception-logger)                       | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [claudneysessa/Horse-CSResponsePagination](https://github.com/claudneysessa/Horse-CSResponsePagination)   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [claudneysessa/Horse-XSuperObjects](https://github.com/claudneysessa/Horse-XSuperObjects)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [andre-djsystem/horse-bearer-auth](https://github.com/andre-djsystem/horse-bearer-auth)                   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [andre-djsystem/horse-manipulate-request](https://github.com/andre-djsystem/horse-manipulate-request)     | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [andre-djsystem/horse-manipulate-response](https://github.com/andre-djsystem/horse-manipulate-response)   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;✔️ |
|  [antoniojmsjr/Horse-IPGeoLocation](https://github.com/antoniojmsjr/Horse-IPGeoLocation)                   | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [antoniojmsjr/Horse-XMLDoc](https://github.com/antoniojmsjr/Horse-XMLDoc)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [isaquepinheiro/horse-jsonbr](https://github.com/HashLoad/JSONBr)                                         | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [IagooCesaar/Horse-JsonInterceptor](https://github.com/IagooCesaar/Horse-JsonInterceptor)                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [dliocode/horse-datalogger](https://github.com/dliocode/horse-datalogger)                                 | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |
|  [marcobreveglieri/horse-prometheus-metrics](https://github.com/marcobreveglieri/horse-prometheus-metrics) | &nbsp;&nbsp;&nbsp;✔️ | &nbsp;&nbsp;&nbsp;&nbsp;❌ |

## Delphi Versions
`Horse` works with Delphi 11 Alexandria, Delphi 10.4 Sydney, Delphi 10.3 Rio, Delphi 10.2 Tokyo, Delphi 10.1 Berlin, Delphi 10 Seattle, Delphi XE8 and Delphi XE7.

## 💻 Code Contributors

<a href="https://github.com/Hashload/horse/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=Hashload/horse" />
</a>

## ⚠️ License

`Horse` is free and open-source software licensed under the [MIT License](https://github.com/HashLoad/horse/blob/master/LICENSE). 

## 📐 Tests

![tests](https://github.com/GlerystonMatos/horse/workflows/tests/badge.svg) ![Console Coverage ](https://img.shields.io/badge/console%20coverage-45%25-blue) ![VCL Coverage ](https://img.shields.io/badge/vcl%20coverage-43%25-blue)
