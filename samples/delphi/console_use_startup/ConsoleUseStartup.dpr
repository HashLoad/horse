program ConsoleUseStartup;

{$APPTYPE CONSOLE}

uses
  Horse,
  System.SysUtils;

type
  // Classe de inicialização estruturada (estilo ASP.NET Core Startup)
  THorseStartup = class(TInterfacedObject, IHorseStartup)
  public
    procedure Configure(const AInstance: THorseInstance);
  end;

{ THorseStartup }

procedure THorseStartup.Configure(const AInstance: THorseInstance);
begin
  Writeln(' -> Executando bootstrapping da classe de Startup...');

  // 1. Registro de ganchos locais da instância
  AInstance.AddBeforeListen(
    procedure(APort: Integer)
    begin
      Writeln(Format('[Instance Hook] Servidor sera iniciado na porta: %d', [APort]));
    end);

  // 2. Registro de rotas e middlewares
  AInstance.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  Writeln(' -> Configuracao de rotas e middlewares concluida!');
end;

var
  LStartup: IHorseStartup;

begin
  Writeln('=== Horse UseStartup Demo App ===');

  // Criamos o objeto de inicialização e associamos à interface
  LStartup := THorseStartup.Create;

  // Injetamos a inicialização e escutamos na porta 9000
  THorse
    .UseStartup(LStartup)
    .Listen(9000,
      procedure
      begin
        Writeln('Servidor Horse escutando em http://localhost:9000');
        Writeln('Envie um GET para http://localhost:9000/ping');
        Writeln('Pressione ENTER para encerrar...');
      end);

  Readln;
end.
