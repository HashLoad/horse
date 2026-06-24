program HttpSys;

{$APPTYPE CONSOLE}

// Habilita o provider HTTP.sys nativo do Windows
{$DEFINE HORSE_PROVIDER_HTTPSYS}

uses
  Horse,
  System.SysUtils,
  System.JSON;

begin
  ReportMemoryLeaksOnShutdown := True;

  // Rota simples de ping
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  // Rota para demonstrar leitura de parâmetros de rota, query string e envio de JSON
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
        LJSON.AddPair('provedor', 'HTTP.sys');
        LJSON.AddPair('mensagem', 'Exemplo de integração funcionando perfeitamente!');
        
        Res.Send(LJSON.ToJSON);
      finally
        LJSON.Free;
      end;
    end);

  // Escuta em localhost:9095 (evita necessidade de executar como Administrador no Windows HTTP.sys)
  THorse.Listen(9095, 'localhost',
    procedure
    begin
      Writeln('--------------------------------------------------');
      Writeln(' Servidor Horse HTTP.sys Iniciado Localmente');
      Writeln(Format(' Escutando em: http://%s:%d/', [THorse.Host, THorse.Port]));
      Writeln('--------------------------------------------------');
      Writeln(' Rotas disponiveis para teste:');
      Writeln('  - GET http://localhost:9095/ping');
      Writeln('  - GET http://localhost:9095/users/123?nome=Regys');
      Writeln('--------------------------------------------------');
      Writeln(' Pressione [Enter] para encerrar.');
      Readln;
    end);
end.
