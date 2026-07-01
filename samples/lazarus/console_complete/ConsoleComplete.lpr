program ConsoleComplete;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  Horse,
  Horse.Commons;

// Declaração dos Handlers
procedure LogMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Writeln(Format('[LOG] Requisicao Recebida: %s %s', [Req.Method, Req.RawWebRequest.PathInfo]));
  Next();
end;

procedure CorsMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.AddHeader('Access-Control-Allow-Origin', '*');
  Res.AddHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS, PATCH, QUERY');
  Res.AddHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization, X-Requested-With');
  
  if Req.Method = 'OPTIONS' then
  begin
    Res.Status(THTTPStatus.NoContent).Send('');
    Exit;
  end;
  
  Next();
end;

procedure GetPing(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('pong');
end;

procedure GetResourceById(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  LId: string;
  LQuery: string;
  LAuth: string;
begin
  LId := Req.Params['id'];
  LQuery := Req.Query['q'];
  LAuth := Req.Headers['authorization'];
  Res.Send(Format('{"id":"%s", "query":"%s", "auth":"%s"}', [LId, LQuery, LAuth]));
end;

procedure PostResource(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('POST OK: ' + Req.Body).Status(THTTPStatus.Created);
end;

procedure PutResource(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send(Format('{"status":"Updated", "id":"%s"}', [Req.Params['id']]));
end;

procedure PatchResource(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send(Format('{"status":"Patched", "id":"%s"}', [Req.Params['id']]));
end;

procedure DeleteResource(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send(Format('{"status":"Deleted", "id":"%s"}', [Req.Params['id']]));
end;

procedure QuerySearch(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('SEARCH RESULT FOR: ' + Req.Body);
end;

procedure PostUpload(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  if Req.RawWebRequest.Files.Count > 0 then
  begin
    Res.Send(Format('{"status":"Upload OK", "filename":"%s", "size":%d}',
      [Req.RawWebRequest.Files[0].FileName, Req.RawWebRequest.Files[0].Stream.Size]));
  end
  else
  begin
    Res.Send('{"error":"Nenhum arquivo enviado"}').Status(THTTPStatus.BadRequest);
  end;
end;

procedure GetErrorTrigger(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  raise EHorseException.Create.Status(THTTPStatus.BadRequest).Error('{"error":"Erro de Negocio Simulado"}');
end;

procedure ServerListen;
begin
  Writeln(Format('Servidor Console Completo do Horse ativo em http://localhost:%d', [THorse.Port]));
end;

begin
  {$IFDEF MSWINDOWS}
    {$IFNDEF FPC}
    IsConsole := True;
    ReportMemoryLeaksOnShutdown := True;
    {$ENDIF}
  {$ENDIF}

  // Middleware global de log das requisicoes recebidas
  THorse.Use(LogMiddleware);
  THorse.Use(CorsMiddleware);

  // 1. GET /ping -> ping-pong tradicional
  THorse.Get('/ping', GetPing);

  // 2. GET /resource/:id -> Testando parametros, query strings e headers case-insensitive
  THorse.Get('/resource/:id', GetResourceById);

  // 3. POST /resource -> Testando body em envio normal
  THorse.Post('/resource', PostResource);

  // 4. PUT /resource/:id -> Testando atualizacao completa
  THorse.Put('/resource/:id', PutResource);

  // 5. PATCH /resource/:id -> Testando atualizacao parcial
  THorse.Patch('/resource/:id', PatchResource);

  // 6. DELETE /resource/:id -> Testando remocao
  THorse.Delete('/resource/:id', DeleteResource);

  // 7. QUERY /search -> Novo verbo QUERY enviando payload complexo de pesquisa
  THorse.Query('/search', QuerySearch);

  // 8. POST /upload -> Testando Upload multipart/form-data real
  THorse.Post('/upload', PostUpload);

  // 9. GET /error-trigger -> Testando tratamento de excecoes limpas e customizadas
  THorse.Get('/error-trigger', GetErrorTrigger);

  THorse.Listen(9086, ServerListen);
end.
