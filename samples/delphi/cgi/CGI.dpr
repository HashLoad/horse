program CGI;

{$APPTYPE CONSOLE}
{$R *.res}

uses Horse;

begin
  // Need to set "HORSE_CGI" compilation directive

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen;
end.
