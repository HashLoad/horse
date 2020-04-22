# Horse
Fast, opinionated, minimalist web framework for Delphi

### For install in your project using [boss](https://github.com/HashLoad/boss):
``` sh
$ boss install horse
```
* (Optional) Install [**wizard**](https://github.com/viniciussanchez/horse-wizard)

### Samples
```delphi
uses
  Horse;
  
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
* [**horse-compression**](https://github.com/viniciussanchez/horse-compression) - Middleware for compression in HORSE
* [**handle-exception**](https://github.com/HashLoad/handle-exception) - Middleware for handler exceptions in HORSE
* [**horse-etag**](https://github.com/bittencourtthulio/Horse-ETag) - Horse Server Middleware for Etag Control

### [Documentation](https://romantic-lalande-2d106d.netlify.com/pt-br)
