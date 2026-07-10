program ConsoleComplete;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  System.Classes,
  System.SysUtils,
  Horse,
  Horse.Commons;

begin
  {$IFDEF MSWINDOWS}
  IsConsole := True;
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  // Middleware global de log das requisicoes recebidas
  THorse.Use(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Writeln(Format('[LOG] Requisicao Recebida: %s %s', [Req.Method, Req.RawWebRequest.PathInfo]));
      Next();
    end);

  // 1. GET /ping -> ping-pong tradicional
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('pong');
    end);

  // 2. GET /resource/:id -> Testando parametros, query strings e headers case-insensitive
  THorse.Get('/resource/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    var
      LId: string;
      LQuery: string;
      LAuth: string;
    begin
      LId := Req.Params['id'];
      LQuery := Req.Query['q'];
      LAuth := Req.Headers['authorization']; // Case-insensitive header!
      Res.Send(Format('{"id":"%s", "query":"%s", "auth":"%s"}', [LId, LQuery, LAuth]));
    end);

  // 3. POST /resource -> Testando body em envio normal
  THorse.Post('/resource',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('POST OK: ' + Req.Body).Status(THTTPStatus.Created);
    end);

  // 4. PUT /resource/:id -> Testando atualizacao completa
  THorse.Put('/resource/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send(Format('{"status":"Updated", "id":"%s"}', [Req.Params['id']]));
    end);

  // 5. PATCH /resource/:id -> Testando atualizacao parcial
  THorse.Patch('/resource/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send(Format('{"status":"Patched", "id":"%s"}', [Req.Params['id']]));
    end);

  // 6. DELETE /resource/:id -> Testando remocao
  THorse.Delete('/resource/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send(Format('{"status":"Deleted", "id":"%s"}', [Req.Params['id']]));
    end);

  // 7. QUERY /search -> Novo verbo QUERY enviando payload complexo de pesquisa
  THorse.Query('/search',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('SEARCH RESULT FOR: ' + Req.Body);
    end);

  // 8. POST /upload -> Testando Upload multipart/form-data real
  THorse.Post('/upload',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
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
    end);

  // 9. GET /error-trigger -> Testando tratamento de excecoes limpas e customizadas
  THorse.Get('/error-trigger',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      raise EHorseException.Create.Status(THTTPStatus.BadRequest).Error('{"error":"Erro de Negocio Simulado"}');
    end);

  // 10. GET /clientes -> Rota exata para testar prioridade de wildcard (*)
  THorse.Get('/clientes',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('clientes');
    end);

  // 11. GET /pessoas -> Rota exata para testar prioridade de wildcard (*)
  THorse.Get('/pessoas',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('pessoas');
    end);

  // Teste de integridade para a issue #357 (Group + Route + End + Put)
  THorse.Group.Prefix('/api')
    .Delete('/test1/:id',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        Res.Send('delete1');
      end)
    .Route('/test2')
      .Get(
        procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
        begin
          Res.Send('get2');
        end)
      .Post(
        procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
        begin
          Res.Send('post2');
        end)
    .&End
    .Put('/teste3',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        Res.Send('put3');
      end);

  // 12. GET * -> Rota wildcard genérica (coringão)
  THorse.Get('*',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('coringao');
    end);

  // 13. GET /av-trigger -> Simula Access Violation (AV) de propósito
  THorse.Get('/av-trigger',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    var
      LPointer: PInteger;
    begin
      LPointer := nil;
      LPointer^ := 42;
    end);

  // 14. GET /stack-trigger -> Simula estouro de pilha (Stack Overflow)
  THorse.Get('/stack-trigger',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    var
      LRecurse: TProc;
    begin
      LRecurse := procedure
        begin
          LRecurse();
        end;
      LRecurse();
    end);

  THorse.Listen(9085,
    procedure
    begin
      Writeln(Format('Servidor Console Completo do Horse ativo em http://localhost:%d', [THorse.Port]));
    end);
end.
