program ConsoleAdvancedRouting;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse,
  Horse.Commons;

begin
  // Permite testar o Radix Router passando o parâmetro --radix
  if FindCmdLineSwitch('radix', True) then
  begin
    Writeln(' -> Ativando Roteador Radix de alta performance...');
    THorse.UseRadixRouter;
  end
  else
  begin
    Writeln(' -> Usando Roteador Tree padrão...');
  end;

  // 1. Rota Estática
  THorse.Get('/users/new',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('Criando um novo usuario. Rota estatica (/users/new) chamada com sucesso.');
    end);

  // 2. Rota Paramétrica Numérica com restrição de Regex
  THorse.Get('/users/:id(\d+)',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('Dados do usuario ID (Numerico): ' + Req.Params.Items['id']);
    end);

  // 3. Rota com Parâmetro Opcional
  THorse.Get('/users/:id?',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      if Req.Params.Items['id'].IsEmpty then
        Res.Send('Lista de todos os usuarios (parametro opcional ausente).')
      else
        Res.Send('Dados do usuario ID (Texto/Opcional): ' + Req.Params.Items['id']);
    end);

  // Inicializa a escuta do servidor na porta 9000
  THorse.Listen(9000,
    procedure
    begin
      Writeln('Servidor ativo na porta 9000.');
      Writeln('----------------------------------------------------');
      Writeln('Rotas disponiveis para teste (use curl):');
      Writeln(' 1. Rota estatica:      curl http://localhost:9000/users/new');
      Writeln(' 2. Regex (numerico):   curl http://localhost:9000/users/123');
      Writeln(' 3. Opcional (texto):   curl http://localhost:9000/users/john');
      Writeln(' 4. Opcional (vazio):   curl http://localhost:9000/users');
      Writeln('----------------------------------------------------');
      Writeln('Pressione ENTER para parar o servidor...');
    end);

  Readln;
end.
