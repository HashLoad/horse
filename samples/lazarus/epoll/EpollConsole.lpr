program EpollConsole;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  fpjson,
  Horse;

const
  HTML_ROOT = 
    '<!DOCTYPE html><html><head><meta charset="utf-8"><title>Horse Server</title>' +
    '<style>' +
    'body { font-family: system-ui, -apple-system, sans-serif; background: #0f172a; color: #f8fafc; padding: 2rem; max-width: 600px; margin: 0 auto; }' +
    'h1 { color: #38bdf8; border-bottom: 2px solid #334155; padding-bottom: 0.5rem; }' +
    'ul { list-style: none; padding: 0; }' +
    'li { margin: 1rem 0; background: #1e293b; padding: 1rem; border-radius: 8px; border: 1px solid #334155; }' +
    'span.method { font-weight: bold; padding: 0.25rem 0.5rem; border-radius: 4px; font-size: 0.85rem; margin-right: 0.5rem; display: inline-block; width: 60px; text-align: center; }' +
    'span.get { background: #0284c7; color: #fff; }' +
    'span.post { background: #16a34a; color: #fff; }' +
    'span.put { background: #ca8a04; color: #fff; }' +
    'span.patch { background: #9333ea; color: #fff; }' +
    'span.delete { background: #dc2626; color: #fff; }' +
    'a { color: #38bdf8; text-decoration: none; font-family: monospace; font-size: 1.1rem; }' +
    'a:hover { text-decoration: underline; }' +
    '.desc { margin-top: 0.5rem; font-size: 0.9rem; color: #94a3b8; }' +
    '</style></head><body>' +
    '<h1>Horse Server API &mdash; Lazarus Epoll (Linux)</h1>' +
    '<p>Bem-vindo ao servidor Horse! Use os links abaixo para testar as rotas:</p>' +
    '<ul>' +
    '<li><span class="method get">GET</span><a href="/ping">/ping</a><div class="desc">Retorna a resposta simples de ping (pong)</div></li>' +
    '<li><span class="method get">GET</span><a href="/users/123?nome=Regys">/users/123?nome=Regys</a><div class="desc">Retorna JSON a partir do ID e QueryParam</div></li>' +
    '<li><span class="method post">POST</span><a href="#" onclick="fetch(''/users'', {method: ''POST''}).then(r => r.json()).then(j => alert(JSON.stringify(j)))">/users</a><div class="desc">Executa requisição POST e exibe o JSON resultante</div></li>' +
    '<li><span class="method put">PUT</span><a href="#" onclick="fetch(''/users/123'', {method: ''PUT''}).then(r => r.json()).then(j => alert(JSON.stringify(j)))">/users/123</a><div class="desc">Executa requisição PUT e exibe o JSON resultante</div></li>' +
    '<li><span class="method patch">PATCH</span><a href="#" onclick="fetch(''/users/123'', {method: ''PATCH''}).then(r => r.json()).then(j => alert(JSON.stringify(j)))">/users/123</a><div class="desc">Executa requisição PATCH e exibe o JSON resultante</div></li>' +
    '<li><span class="method delete">DELETE</span><a href="#" onclick="fetch(''/users/123'', {method: ''DELETE''}).then(r => r.json()).then(j => alert(JSON.stringify(j)))">/users/123</a><div class="desc">Executa requisição DELETE e exibe o JSON resultante</div></li>' +
    '</ul></body></html>';

procedure GetRoot(Req: THorseRequest; Res: THorseResponse);
begin
  Res.ContentType('text/html; charset=utf-8');
  Res.Send(HTML_ROOT);
end;

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

procedure PostUsers(Req: THorseRequest; Res: THorseResponse);
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.Add('status', 'created');
    LJSON.Add('action', 'POST');
    Res.Send(LJSON.AsJSON).Status(201);
  finally
    LJSON.Free;
  end;
end;

procedure PostUpload(Req: THorseRequest; Res: THorseResponse);
var
  LContent: string;
begin
  LContent := Req.Body;
  Res.Send('Tamanho: ' + IntToStr(Length(LContent)) + ' / Inicio: ' + Copy(LContent, 1, 50));
end;

procedure PutUsers(Req: THorseRequest; Res: THorseResponse);
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.Add('id', Req.Params.Items['id']);
    LJSON.Add('status', 'updated');
    LJSON.Add('action', 'PUT');
    Res.Send(LJSON.AsJSON);
  finally
    LJSON.Free;
  end;
end;

procedure PatchUsers(Req: THorseRequest; Res: THorseResponse);
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.Add('id', Req.Params.Items['id']);
    LJSON.Add('status', 'patched');
    LJSON.Add('action', 'PATCH');
    Res.Send(LJSON.AsJSON);
  finally
    LJSON.Free;
  end;
end;

procedure DeleteUsers(Req: THorseRequest; Res: THorseResponse);
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.Add('id', Req.Params.Items['id']);
    LJSON.Add('status', 'deleted');
    LJSON.Add('action', 'DELETE');
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
  Writeln('  - GET    http://localhost:9095/ping');
  Writeln('  - GET    http://localhost:9095/users/123?nome=Regys');
  Writeln('  - POST   http://localhost:9095/users');
  Writeln('  - POST   http://localhost:9095/upload');
  Writeln('  - PUT    http://localhost:9095/users/123');
  Writeln('  - PATCH  http://localhost:9095/users/123');
  Writeln('  - DELETE http://localhost:9095/users/123');
  Writeln('--------------------------------------------------');
  Writeln(' Pressione Ctrl+C para encerrar.');
end;

begin
  THorse.Get('/', GetRoot);
  THorse.Get('/ping', GetPing);
  THorse.Get('/users/:id', GetUsers);
  THorse.Post('/users', PostUsers);
  THorse.Post('/upload', PostUpload);
  THorse.Put('/users/:id', PutUsers);
  THorse.Patch('/users/:id', PatchUsers);
  THorse.Delete('/users/:id', DeleteUsers);
  THorse.Listen(9095, '0.0.0.0', OnListen);
  while THorse.IsRunning do
    Sleep(1000);
end.
