program ConsoleRouteMiddlewares;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}
{$IFDEF MSWINDOWS}
{$R *.res}
{$ENDIF}

uses
  {$IFDEF UNIX}
    cthreads,
  {$ENDIF}
  Horse,
  {$IFDEF FPC}
    SysUtils;
  {$ELSE}
    System.SysUtils;
  {$ENDIF}

// Middlewares Locais
procedure MiddlewareAutenticacao(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  if Req.Headers['Authorization'] <> 'token-valido' then
  begin
    Res.Send('Nao autorizado').Status(THTTPStatus.Unauthorized);
    Exit;
  end;
  Next;
end;

procedure MiddlewareLogAcesso(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  Writeln(Format('[LOG] Acesso a rota "%s" realizada com sucesso.', [Req.RawWebRequest.PathInfo]));
  Next;
end;

// Handler Final da Rota
procedure DoGetAdmin(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Dados administrativos acessados com sucesso!');
end;

procedure DoGetRelatorio(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Dados do relatorio V1 acessados!');
end;

procedure DoListen;
begin
  Writeln(Format('Servidor de Exemplo rodando na porta %d...', [THorse.Port]));
  Writeln('Experimente testar via cURL ou Postman:');
  Writeln('  1. GET http://localhost:9000/ping');
  Writeln('  2. GET http://localhost:9000/admin/dados (Sem Header - Bloqueado)');
  Writeln('  3. GET http://localhost:9000/admin/dados (Com Header Authorization: token-valido - Sucesso)');
  Writeln('  4. GET http://localhost:9000/api/v1/relatorios (Com Header Authorization: token-valido - Grupo)');
end;

begin
  {$IFDEF MSWINDOWS}
    IsConsole := True;
    ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  // 1. Rota Publica Comum (Sem Middlewares Locais)
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  // 2. Rota Estatica com Cadeia de Middlewares por Rota (Open Array)
  THorse.Get('/admin/dados', [MiddlewareAutenticacao, MiddlewareLogAcesso], DoGetAdmin);

  // 3. Rota Fluente em Grupo de Rota com Cadeia de Middlewares
  THorse.Group.Prefix('/api/v1')
    .Route('/relatorios')
      .Get([MiddlewareAutenticacao, MiddlewareLogAcesso], DoGetRelatorio);

  // Inicia o Servidor
  THorse.Listen(9000, DoListen);

  while THorse.IsRunning do
    Sleep(1000);
end.
