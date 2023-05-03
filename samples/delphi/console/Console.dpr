program Console;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Horse,
  System.SysUtils;

begin
  THorse.KeepConnectionAlive := True;

  {$IFDEF MSWINDOWS}
  IsConsole := False;
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(Req.Query.Field('str').AsString + ' ' + Req.Query.Field('inteiro').AsInteger.ToString);
    end);

  THorse.Listen(9000,
    procedure
    begin
      Writeln(Format('Server is runing on %s:%d', [THorse.Host, THorse.Port]));
      Readln;
    end);
end.
