# horse
Fast, opinionated, minimalist web framework for Delphi

Sample Horse Server
```delphi
uses
  Horse;
var
  App: THorse;
begin
  App := THorse.Create(9000);
  App.Get('/ping',
    procedure(AReq: THorseRequest; ARes: THorseResponse)
    begin
      ARes.Send('pong');
    end);
  App.Start;
```
