# Horse
Fast, opinionated, minimalist web framework for Delphi

## ⚙️ Installation
Installation is done using the [`boss install`](https://github.com/HashLoad/boss) command:
``` sh
$ boss install horse
```
* (Optional) Install [**wizard**](https://github.com/viniciussanchez/horse-wizard)

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
- [horse/cors](https://github.com/HashLoad/horse-cors)
- [horse/stream](https://github.com/HashLoad/horse-octet-stream)
- [horse/jwt](https://github.com/HashLoad/horse-jwt)
- [horse/exception](https://github.com/HashLoad/handle-exception)

## 🌱 Third Party Middlewares

This is a list of middlewares that are created by the Horse community, please create a PR if you want to see yours!
- [viniciussanchez/basic-auth](https://github.com/viniciussanchez/horse-basic-auth)
- [viniciussanchez/compression](https://github.com/viniciussanchez/horse-compression)
- [bittencourtthulio/etag](https://github.com/bittencourtthulio/Horse-ETag)

### [Documentation](https://romantic-lalande-2d106d.netlify.com/pt-br)
