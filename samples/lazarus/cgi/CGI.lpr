program CGI;

{$MODE DELPHI}{$H+}

uses
  Horse, SysUtils;

procedure GetPing(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('pong');
end;

begin
  THorse.Get('/ping', GetPing);

  THorse.Listen;
end.
