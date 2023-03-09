program CGI;

{$MODE DELPHI}{$H+}

uses Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  // Need to set "HORSE_CGI" compilation directive
  THorse.Get('/ping', GetPing);
  THorse.Listen;
end.
