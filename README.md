# Horse
Fast, opinionated, minimalist web framework for Delphi

### For install in your project using [boss](https://github.com/HashLoad/boss):
``` sh
$ boss install github.com/HashLoad/horse
```

Sample Horse Server
```delphi
uses
  Horse.API, Horse.CGI;
  
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

## Middlewares
* [**jhonson**](https://github.com/HashLoad/jhonson) - Middleware for parse JSON in HORSE
* [**horse-cors**](https://github.com/HashLoad/horse-cors) - Middleware for inject CORS headers in HORSE
* [**horse-octet-stream**](https://github.com/HashLoad/horse-octet-stream) - Middleware for work with application/octet-stream in HORSE
* [**horse-jwt**](https://github.com/HashLoad/horse-jwt) - Middleware for JWT in HORSE
* [**horse-basic-auth**](https://github.com/viniciussanchez/horse-basic-auth) - Middleware for Basic Authentication in HORSE


### [Documentation](https://horse.hashload.com/pt-br)
