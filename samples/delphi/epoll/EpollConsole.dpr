program EpollConsole;

{$APPTYPE CONSOLE}

// Habilita o novo provider Epoll nativo de Linux
{$DEFINE HORSE_PROVIDER_EPOLL}

uses
  Horse,
  System.SysUtils,
  System.JSON;

begin
  ReportMemoryLeaksOnShutdown := True;

  // 1. Rota de ping simples
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  // 2. Rota complexa retornando JSON e processando parâmetros
  THorse.Get('/users/:id',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LJSON: TJSONObject;
      LUserId: string;
      LUserName: string;
    begin
      LUserId := Req.Params.Items['id'];
      LUserName := Req.Query.Items['nome'];
      if LUserName.IsEmpty then
        LUserName := 'Visitante';

      LJSON := TJSONObject.Create;
      try
        LJSON.AddPair('id', LUserId);
        LJSON.AddPair('nome', LUserName);
        LJSON.AddPair('provedor', 'Epoll (Linux)');
        LJSON.AddPair('mensagem', 'Exemplo de integracao funcionando perfeitamente!');
        
        Res.Send(LJSON.ToJSON);
      finally
        LJSON.Free;
      end;
    end);

  // Escuta na porta 9095 em 0.0.0.0 (acessível de fora do container Docker)
  THorse.Listen(9095, '0.0.0.0',
    procedure
    begin
      Writeln('--------------------------------------------------');
      Writeln(' Servidor Horse Epoll Iniciado (Linux)');
      Writeln(Format(' Escutando em: http://%s:%d/', [THorse.Host, THorse.Port]));
      Writeln('--------------------------------------------------');
      Writeln(' Rotas disponiveis para teste:');
      Writeln('  - GET http://localhost:9095/ping');
      Writeln('  - GET http://localhost:9095/users/123?nome=Regys');
      Writeln('--------------------------------------------------');
      Writeln(' Pressione Ctrl+C para encerrar.');
    end);
end.
