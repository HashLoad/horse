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
  
var
  App: THorse;
  
begin
  App := THorse.Create(9000);

  App.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);
    
  App.Start;
end.
```

## 🧬 Official Middlewares

For an more _maintainable_ middleware _ecosystem_, we've put official [middlewares](https://docs.gofiber.io/middleware) into separate repositories:

- [horse/json](https://github.com/HashLoad/jhonson)
- [horse/basic-auth](https://github.com/HashLoad/horse-basic-auth)
- [horse/cors](https://github.com/HashLoad/horse-cors)
- [horse/stream](https://github.com/HashLoad/horse-octet-stream)
- [horse/jwt](https://github.com/HashLoad/horse-jwt)
- [horse/exception](https://github.com/HashLoad/handle-exception)
- [horse/logger](https://github.com/HashLoad/horse-logger)
- [horse/compression](https://github.com/HashLoad/horse-compression)

## 🌱 Third Party Middlewares

This is a list of middlewares that are created by the Horse community, please create a PR if you want to see yours!
- [bittencourtthulio/etag](https://github.com/bittencourtthulio/Horse-ETag)
- [bittencourtthulio/paginate](https://github.com/bittencourtthulio/Horse-Paginate)
- [gabrielbaltazar/gbswagger](https://bitbucket.org/gabrielbaltazar/gbswagger)

## ⚠️ License

`Horse` is free and open-source software licensed under the [MIT License](https://github.com/HashLoad/horse/blob/master/LICENSE). 
