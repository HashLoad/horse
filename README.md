# horse
Fast, opinionated, minimalist web framework for Delphi

### For install in your project using [boss](https://github.com/HashLoad/boss):
``` sh
$ boss install github.com/HashLoad/horse-cors
```

Sample Horse Server
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
      ARes.Send('pong');
    end);
  App.Start;
```
