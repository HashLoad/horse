program ConsoleErrorHandler;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Horse,
  System.SysUtils,
  System.JSON;

begin
  THorse.OnError(
    procedure(const ARequest: THorseRequest; const AResponse: THorseResponse; const AException: Exception)
    var
      LJSON: TJSONObject;
    begin
      Writeln('Log de Erro Capturado: ' + AException.Message);
      LJSON := TJSONObject.Create;
      try
        LJSON.AddPair('error', AException.Message);
        LJSON.AddPair('timestamp', DateTimeToStr(Now));
        AResponse.Send(LJSON.ToJSON).Status(THTTPStatus.InternalServerError);
      except
        LJSON.Free;
        raise;
      end;
    end);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  THorse.Get('/error',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      raise Exception.Create('Um erro de teste inesperado no servidor!');
    end);

  THorse.Listen(9000,
    procedure(Horse: THorse)
    begin
      Writeln('Servidor executando em http://localhost:' + Horse.Port.ToString);
      Writeln('Teste /error para ver o manipulador global de erros em acao.');
    end);
end.
