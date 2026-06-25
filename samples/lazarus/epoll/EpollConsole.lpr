program EpollConsole;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  fpjson,
  Horse;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Pong from Epoll (Lazarus)!');
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
    LJSON.Add('provedor', 'Epoll (Linux)');
    LJSON.Add('mensagem', 'Exemplo de integracao funcionando perfeitamente!');
    
    Res.Send(LJSON.AsJSON);
  finally
    LJSON.Free;
  end;
end;

procedure OnListen;
begin
  Writeln('--------------------------------------------------');
  Writeln(' Servidor Horse Epoll Iniciado (Lazarus/FPC)');
  Writeln(Format(' Escutando em: http://%s:%d/', [THorse.Host, THorse.Port]));
  Writeln('--------------------------------------------------');
  Writeln(' Rotas disponiveis para teste:');
  Writeln('  - GET http://localhost:9095/ping');
  Writeln('  - GET http://localhost:9095/users/123?nome=Regys');
  Writeln('--------------------------------------------------');
  Writeln(' Pressione Ctrl+C para encerrar.');
end;

begin
  THorse.Get('/ping', GetPing);
  THorse.Get('/users/:id', GetUsers);
  THorse.Listen(9095, '0.0.0.0', OnListen);
end.
