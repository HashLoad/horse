program TesteHorse;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Horse, Horse.Controller, Horse.Commons, ControllerTeste;

procedure TesteJwt(AReq: THorseRequest; ARes: THorseResponse; ANext: TNextProc);
begin
  ANext();
end;

begin
  THorse.AddCallback(TesteJwt).Map(TControllerTeste,'/users', mtGet, 'ListUsers');
  THorse.AddCallback(TesteJwt).Map(TControllerTeste,'/users/:id', mtGet, 'GetUserById');

  THorse.Listen(9001);
end. 
