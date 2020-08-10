program FCGI;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

uses
  Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('pong');
end;

begin

  THorse.Get('/ping', GetPing);
  // { Uncomment the port setting here if you want to run the
  // FastCGI application stand-alone (e.g. for NGINX) }
  // THorse.Port:=9050; // For example
  THorse.Listen;

end.
