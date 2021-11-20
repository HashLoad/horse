program Console;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Horse, System.SysUtils, System.DateUtils;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LDate: TDateTime;
    begin
      LDate := Req.Headers.AsDateTime('data');
      Res.Send(LDate.ToISO8601);
    end);

  THorse.Listen(9000,
    procedure(Horse: THorse)
    begin
      Writeln(Format('Server is runing on %s:%d', [Horse.Host, Horse.Port]));
    end);
end.
