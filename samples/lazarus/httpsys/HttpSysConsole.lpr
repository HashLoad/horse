program HttpSysConsole;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  fpjson,
  Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Pong from HTTP.sys (Lazarus)!');
end;

procedure GetUsers(Req: THorseRequest; Res: THorseResponse);
var
  LJSON: TJSONObject;
  LUserId: string;
  LUserName: string;
begin
  LUserId := Req.Params.Items['id'];
  LUserName := Req.Query.Items['nome'];
  if LUserName = '' then
    LUserName := 'Visitante';

  LJSON := TJSONObject.Create;
  try
    LJSON.Add('id', LUserId);
    LJSON.Add('nome', LUserName);
    LJSON.Add('provedor', 'HTTP.sys');
    LJSON.Add('mensagem', 'Exemplo de integracao funcionando perfeitamente!');
    
    Res.Send(LJSON.AsJSON);
  finally
    LJSON.Free;
  end;
end;

procedure OnListen;
begin
  Writeln('--------------------------------------------------');
  Writeln(' Servidor Horse HTTP.sys Iniciado (Lazarus/FPC)');
  Writeln(Format(' Escutando em: http://%s:%d/', [THorse.Host, THorse.Port]));
  Writeln('--------------------------------------------------');
  Writeln(' Rotas disponiveis para teste:');
  Writeln('  - GET http://localhost:9095/ping');
  Writeln('  - GET http://localhost:9095/users/123?nome=Regys');
  Writeln('--------------------------------------------------');
  Writeln(' Pressione [Enter] para encerrar.');
  Readln;
end;

begin
  THorse.Get('/ping', GetPing);
  THorse.Get('/users/:id', GetUsers);
  THorse.Listen(9095, 'localhost', OnListen);
end.
