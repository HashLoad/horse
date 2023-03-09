program FCGI;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

uses
  Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  // Need to set "HORSE_FCGI" compilation directive
  THorse.Get('/ping', GetPing);
  THorse.Listen;
end.
