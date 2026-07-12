program ConsoleDependencyInjection;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Horse,
  Horse.Commons,
  SysUtils;

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
  THorse.Get('/resolve', [
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LService: TMyService;
    begin
      LService := TMyService.Create('InstanciaDireta');
      Req.Services.Add(TMyService, LService);
      Next();
    end,
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LService: TMyService;
    begin
      LService := TMyService(Req.Services.Resolve(TMyService));
      Res.Send(LService.GetMessage);
    end]);

  THorse.Get('/lazy',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LService: TMyService;
    begin
      Req.Services.AddFactory(TMyService,
        function: TObject
        begin
          Result := TMyService.Create('LazyFactory');
        end);
      
      Writeln('-> Resolvendo o serviço lazy...');
      LService := TMyService(Req.Services.Resolve(TMyService));
      Res.Send(LService.GetMessage);
    end);

  Writeln('Servidor Horse rodando na porta 9000 com Injeção de Dependências...');
  Writeln('Acesse /resolve ou /lazy');
  THorse.Listen(9000);
end.
