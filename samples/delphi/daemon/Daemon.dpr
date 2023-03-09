program Daemon;

{$APPTYPE CONSOLE}
{$R *.res}

uses Horse, System.SysUtils;

begin
  // Need to set "HORSE_DAEMON" compilation directive

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen;
end.
