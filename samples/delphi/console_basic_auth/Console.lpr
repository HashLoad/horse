program Console;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Horse,
  Horse.BasicAuthentication;

const
  cVersionAPI = 'v1';
  cAPIName = 'xpos';

function CheckLogin(const AUsername, APassword: string): Boolean;
begin
  Result := False;
end;

procedure DoLogin(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('DoLogin');
end;

procedure DoLogout(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('DoLogout');
end;


procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Ping');
end;

begin
  THorse
    .Group
    .Prefix(cVersionAPI+'/'+cAPIName)
    .Delete('/logout', DoLogout)
    .AddCallback(HorseBasicAuthentication(CheckLogin))
    .Get('/login',DoLogin);
  THorse.Listen(9000);
end.
