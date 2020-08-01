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

## ⚠️ License

`Horse` is free and open-source software licensed under the [MIT License](https://github.com/HashLoad/horse/blob/master/LICENSE).
