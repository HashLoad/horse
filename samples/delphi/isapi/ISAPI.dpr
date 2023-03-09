library ISAPI;

{$R *.res}

uses Horse;

begin
  // Need to set "HORSE_ISAPI" compilation directive

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen;
end.
