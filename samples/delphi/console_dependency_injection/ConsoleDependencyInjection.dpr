program ConsoleDependencyInjection;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Horse,
  Horse.Commons,
  System.SysUtils;

type
  TMyService = class
  private
    FId: string;
  public
    constructor Create(const AId: string);
    destructor Destroy; override;
    function GetMessage: string;
  end;

{ TMyService }

constructor TMyService.Create(const AId: string);
begin
  inherited Create;
  FId := AId;
  Writeln(Format('[TMyService] Instanciado com ID: %s', [FId]));
end;

destructor TMyService.Destroy;
begin
  Writeln(Format('[TMyService] Destruído com ID: %s (Escopo limpo)', [FId]));
  inherited Destroy;
end;

function TMyService.GetMessage: string;
begin
  Result := 'Olá de um Serviço Contextual Injetado! ID: ' + FId;
end;

begin
  THorse.Get('/resolve',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LService: TMyService;
    begin
      // Cria e injeta o serviço no escopo do Request.
      // O Horse assumirá a posse do objeto e o destruirá automaticamente ao final do request.
      LService := TMyService.Create('InstanciaDireta');
      Req.Services.Add(TMyService, LService);
      Next();
    end,
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LService: TMyService;
    begin
      // Resolve o serviço injetado
      LService := TMyService(Req.Services.Resolve(TMyService));
      Res.Send(LService.GetMessage);
    end);

  THorse.Get('/lazy',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      // Registra a fábrica sem instanciar o objeto ainda (Lazy Loading)
      Req.Services.AddFactory(TMyService,
        function: TObject
        begin
          Result := TMyService.Create('LazyFactory');
        end);
      Next();
    end,
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LService: TMyService;
    begin
      Writeln('-> Resolvendo o serviço lazy...');
      // O objeto só será criado agora, no momento do Resolve!
      LService := TMyService(Req.Services.Resolve(TMyService));
      Res.Send(LService.GetMessage);
    end);

  Writeln('Servidor Horse rodando na porta 9000 com Injeção de Dependências...');
  Writeln('Acesse /resolve ou /lazy');
  THorse.Listen(9000);
end.
