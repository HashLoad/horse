unit Horse.Indy.TestRoutes;

(*
  Horse — Shared Integration Test Routes
  (Indy Console / IOCP / HttpSys / Epoll)
  =========================================
  Registers the same 35-route surface exercised by HorseIndyTestClient.dpr.
  Used by all four provider-specific server programs so each can run against
  the same test client without duplicating route code.

  Providers that use this unit:
    HorseIndyTestServer.dpr    — default Console provider (Indy / TIdHTTPServer)
    HorseIOCPTestServer.dpr    — HORSE_PROVIDER_IOCP   (Windows custom IOCP)
    HorseHttpSysTestServer.dpr — HORSE_PROVIDER_HTTPSYS (Windows HTTP.sys)
    HorseEpollTestServer.dpr   — HORSE_PROVIDER_EPOLL  (Linux epoll)

  Route surface (32 provider-agnostic + 3 streaming stubs):
    GET    /ping
    GET    /methods/get
    POST   /methods/post
    PUT    /methods/put/:id
    DELETE /methods/delete/:id
    PATCH  /methods/patch/:id
    HEAD   /methods/head
    GET    /params/path/:id
    GET    /params/query
    GET    /cookies/set          — two Set-Cookie headers (FIX-HEADER-DUP)
    GET    /cookies/echo
    POST   /upload               — multipart/form-data
    GET    /download             — Content-Disposition
    GET    /headers/echo         — X-Test-Header round-trip
    POST   /methods/post (empty) — nil-body path
    POST   /echo/body            — large body + pool reset sequence
    GET    /params/multi/:a/:b   — two path params
    GET    /does/not/exist       — 404
    GET    /status/:code         — explicit status
    GET    /response/large       — 65536-byte body
    GET    /raw/webrequest       — RawWebRequest adapter surface (PATCH-REQ-8)
    ALL    /raw/cors             — OPTIONS preflight + GET branch
    GET    /raw/webresponse      — Res.RawWebResponse.SetCustomHeader (PATCH-RES-6)
    POST   /echo/body-twice      — PATCH-REQ-9 double-read idempotency
    GET    /compat/rawbody       — COMPAT-1 shadow-field precedence
    POST   /pool/burst           — concurrent body isolation
    GET    /stream/pull          — 501 (not implemented on this transport)
    GET    /stream/content-type  — 501
    GET    /stream/empty         — 501

  Dual-compiler: compiles on both Delphi (dcc64/dcc32, Windows + Linux) and
  Lazarus/FPC (fpc) via {$IF DEFINED(FPC)} guards.
*)

{$IF DEFINED(FPC)}{$MODE DELPHI}{$H+}{$ENDIF}

interface

const
  TEST_PORT           = 9010;
  LARGE_RESPONSE_SIZE = 65536;

procedure RegisterTestRoutes;

implementation

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Classes,
  fpHTTP,
  HTTPDefs,
{$ELSE}
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
{$ENDIF}
  Horse,
  Horse.Commons,
  Horse.Response,
  Horse.Core.Param,
  Horse.Core.Param.Field;

type
  TRawWebReq = {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
  TRawWebRes = {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};

{ ── JSON helpers ──────────────────────────────────────────────────────────── }

function JE(const S: string): string;
begin
  Result := StringReplace(S,     '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
end;

function JB(const B: Boolean): string;
begin
  if B then Result := 'true' else Result := 'false';
end;

{ Streaming test handlers — callbacks for the upstream push API
  Res.SendStream(IHorseStreamWriter). Method-of-object form (THorseStreamProc =
  procedure(const AWriter) of object) so it compiles on BOTH Delphi and FPC —
  upstream's anonymous SendStream overload is Delphi-only (IFNDEF FPC guard).
  Per-request state is method-local (no threadvars needed): SendStream runs the
  callback synchronously, and each concurrent stream gets its own stack frame.
  The base class THorseStreamWriterBase does the chunk framing / SSE branching;
  these handlers only Write payloads and honour IsConnected. }
type
  TStreamRoutes = class
    procedure Pull(const AWriter: IHorseStreamWriter);
    procedure ContentTyped(const AWriter: IHorseStreamWriter);
    procedure Empty(const AWriter: IHorseStreamWriter);
  end;

var
  GStreamRoutes: TStreamRoutes;

procedure TStreamRoutes.Pull(const AWriter: IHorseStreamWriter);
var
  LCount: Integer;
begin
  for LCount := 0 to 4 do
  begin
    if not AWriter.IsConnected then Break;
    Sleep(120); // spacing so incremental arrival is observable
    AWriter.Write(Format('chunk-%d;', [LCount]));
  end;
end;

procedure TStreamRoutes.ContentTyped(const AWriter: IHorseStreamWriter);
var
  LCount: Integer;
begin
  for LCount := 0 to 2 do
  begin
    if not AWriter.IsConnected then Break;
    AWriter.Write(Format('ct-chunk-%d;', [LCount]));
  end;
end;

procedure TStreamRoutes.Empty(const AWriter: IHorseStreamWriter);
begin
  { No writes — upstream's Close still commits a complete (empty) response. }
end;

{ ── Route registration ───────────────────────────────────────────────────── }

procedure RegisterTestRoutes;
begin

  // ── Health ────────────────────────────────────────────────────────────────
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/plain').Send('pong');
    end
  );

  // ── HTTP method probes ────────────────────────────────────────────────────

  THorse.Get('/methods/get',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/json; charset=utf-8')
         .Send('{"method":"GET"}');
    end
  );

  THorse.Post('/methods/post',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"method":"POST","body":"%s"}', [JE(Req.Body)]));
    end
  );

  THorse.Put('/methods/put/:id',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"method":"PUT","id":"%s"}', [JE(Req.Params['id'])]));
    end
  );

  THorse.Delete('/methods/delete/:id',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"method":"DELETE","id":"%s"}', [JE(Req.Params['id'])]));
    end
  );

  THorse.Patch('/methods/patch/:id',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"method":"PATCH","id":"%s"}', [JE(Req.Params['id'])]));
    end
  );

  THorse.Head('/methods/head',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.AddHeader('X-Head-Ok', 'true');
    end
  );

  // ── Path & query params ───────────────────────────────────────────────────

  THorse.Get('/params/path/:id',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"id":"%s"}', [JE(Req.Params['id'])]));
    end
  );

  THorse.Get('/params/query',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"name":"%s","value":"%s"}',
           [JE(Req.Query['name']), JE(Req.Query['value'])]));
    end
  );

  THorse.Get('/params/multi/:a/:b',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"a":"%s","b":"%s"}',
           [JE(Req.Params['a']), JE(Req.Params['b'])]));
    end
  );

  // ── Cookies ───────────────────────────────────────────────────────────────

  THorse.Get('/cookies/set',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.AddHeader('Set-Cookie', 'session=abc123; Path=/');
      Res.AddHeader('Set-Cookie', 'user=tester; Path=/');
      Res.ContentType('application/json; charset=utf-8')
         .Send('{"status":"cookies set"}');
    end
  );

  THorse.Get('/cookies/echo',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"session":"%s","user":"%s"}',
           [JE(Req.Cookie['session']), JE(Req.Cookie['user'])]));
    end
  );

  // ── File upload (multipart/form-data) ─────────────────────────────────────
  // Note: IOCP / HttpSys / Epoll use custom HTTP parsers whose multipart
  // decoding coverage is provider-specific.  The test client accepts 200 or
  // a structured 400 from this route.
  THorse.Post('/upload',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LStream: TStream;
      LName:   string;
    begin
      LStream := Req.ContentFields.Field('file').AsStream;
      LName   := Req.ContentFields['fieldname'];
      if Assigned(LStream) then
        Res.ContentType('application/json; charset=utf-8')
           .Send(Format('{"received":true,"name":"%s","size":%d}',
             [JE(LName), LStream.Size]))
      else
        Res.Status(THTTPStatus.BadRequest)
           .ContentType('application/json; charset=utf-8')
           .Send('{"received":false,"error":"no file field"}');
    end
  );

  // ── File download ─────────────────────────────────────────────────────────
  THorse.Get('/download',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/plain; charset=utf-8')
         .AddHeader('Content-Disposition', 'attachment; filename="testfile.txt"')
         .Send('Hello from Horse Indy provider test download!');
    end
  );

  // ── Custom header echo ────────────────────────────────────────────────────
  THorse.Get('/headers/echo',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"X-Test-Header":"%s"}',
           [JE(Req.Headers['X-Test-Header'])]));
    end
  );

  // ── Body echo ─────────────────────────────────────────────────────────────
  THorse.Post('/echo/body',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LBody: string;
    begin
      LBody := Req.Body;
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"body":"%s","size":%d}',
           [JE(LBody), Length(TEncoding.UTF8.GetBytes(LBody))]));
    end
  );

  // ── Explicit status code ──────────────────────────────────────────────────
  THorse.Get('/status/:code',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LCode: Integer;
    begin
      LCode := StrToIntDef(Req.Params['code'], 200);
      if (LCode < 100) or (LCode > 599) then
        LCode := 400;
      Res.ContentType('application/json; charset=utf-8')
         .Status(LCode)
         .Send(Format('{"status":%d}', [LCode]));
    end
  );

  // ── Large response ────────────────────────────────────────────────────────
  THorse.Get('/response/large',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/plain; charset=utf-8')
         .Send(StringOfChar('X', LARGE_RESPONSE_SIZE));
    end
  );

  // ── RawWebRequest adapter probe  (PATCH-REQ-8) ───────────────────────────
  // On the Indy Console path RawWebRequest is the real TWebRequest from
  // WebBroker.  On IOCP / HttpSys / Epoll it is a TInterfacedWebRequest
  // backed by the provider's IHorseRawRequest implementation.
  THorse.Get('/raw/webrequest',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LRaw: TRawWebReq;
    begin
      LRaw := Req.RawWebRequest;
      if not Assigned(LRaw) then
      begin
        Res.ContentType('application/json; charset=utf-8').Status(500)
           .Send('{"hasAdapter":false,"error":"RawWebRequest is nil"}');
        Exit;
      end;
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format(
           '{"hasAdapter":true,"method":"%s","host":"%s","pathInfo":"%s",'
         + '"customHeader":"%s","remoteAddrNonEmpty":%s}',
           [JE(LRaw.Method),
            JE(LRaw.Host),
            JE(LRaw.PathInfo),
            JE(LRaw.GetFieldByName('X-Test-Header')),
            JB(LRaw.RemoteAddr <> '')]));
    end
  );

  // ── Horse.CORS-style route  (PATCH-REQ-8 regression) ─────────────────────
  THorse.All('/raw/cors',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LRaw:    TRawWebReq;
      LMethod: string;
    begin
      LRaw := Req.RawWebRequest;
      if not Assigned(LRaw) then
      begin
        Res.ContentType('text/plain').Status(500)
           .Send('raw-cors:nil-adapter');
        Exit;
      end;
      LMethod := LRaw.Method;
      if SameText(LMethod, 'OPTIONS') then
      begin
        Res.AddHeader('Access-Control-Allow-Origin', '*');
        Res.AddHeader('Access-Control-Allow-Methods', 'GET,POST,OPTIONS');
        Res.ContentType('text/plain').Status(THTTPStatus.NoContent).Send('');
        Exit;
      end;
      Res.ContentType('text/plain').Send('cors-route:' + LMethod);
    end
  );

  // ── RawWebResponse adapter probe  (PATCH-RES-6) ──────────────────────────
  THorse.Get('/raw/webresponse',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LRaw: TRawWebRes;
    begin
      LRaw := Res.RawWebResponse;
      if not Assigned(LRaw) then
      begin
        Res.ContentType('application/json; charset=utf-8').Status(500)
           .Send('{"hasAdapter":false,"error":"RawWebResponse is nil"}');
        Exit;
      end;
      LRaw.SetCustomHeader('X-Via-RawResponse', 'PATCH-RES-6-OK');
      Res.AddHeader('X-Via-AddHeader', 'AddHeader-OK');
      Res.ContentType('application/json; charset=utf-8')
         .Send('{"hasAdapter":true}');
    end
  );

  // ── PATCH-REQ-9: double-read idempotency ─────────────────────────────────
  THorse.Post('/echo/body-twice',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LFirst:  string;
      LSecond: string;
    begin
      LFirst  := Req.Body;
      LSecond := Req.Body;
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"first":"%s","second":"%s","equal":%s}',
           [JE(LFirst), JE(LSecond), JB(LFirst = LSecond)]));
    end
  );

  // ── COMPAT-1: shadow-field precedence over RawWebResponse.Content ─────────
  THorse.Get('/compat/rawbody',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LRaw: TRawWebRes;
    begin
      LRaw := Res.RawWebResponse;
      if Assigned(LRaw) then
        LRaw.Content := 'raw-should-not-appear';
      Res.ContentType('text/plain; charset=utf-8').Send('shadow-wins');
    end
  );

  // ── Burst / sequential body-isolation endpoint ────────────────────────────
  THorse.Post('/pool/burst',
    procedure(Req: THorseRequest; Res: THorseResponse)
    var
      LBody: string;
    begin
      LBody := Req.Body;
      Res.ContentType('application/json; charset=utf-8')
         .Send(Format('{"body":"%s","size":%d}',
           [JE(LBody), Length(TEncoding.UTF8.GetBytes(LBody))]));
    end
  );

  // ── Streaming — upstream SendStream (push API) ────────────────────────────
  // Retargeted 2026-07 from the retired pull Res.SendChunked to upstream's
  // Res.SendStream(IHorseStreamWriter). The base class THorseStreamWriterBase
  // frames chunks / branches SSE; the handlers (methods of GStreamRoutes, so
  // this compiles on both Delphi and FPC) only Write payloads and honour
  // IsConnected. Providers register a stream-writer factory: Indy / IOCP /
  // HttpSys / Epoll get it from the upstream merge; CrossSocket gets it from
  // Phase-2's TCrossSocketStreamWriter. On a provider with NO factory,
  // SendStream raises before sending — caught here and turned into the
  // structured 501 the client accepts as the graceful-refusal outcome.
  THorse.Get('/stream/pull',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/plain; charset=utf-8');
      try
        Res.SendStream(GStreamRoutes.Pull);
      except
        on Exception do
          Res.Status(501).ContentType('application/json; charset=utf-8')
             .Send('{"error":"streaming not implemented on this transport"}');
      end;
    end
  );

  THorse.Get('/stream/content-type',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('application/octet-stream');
      try
        Res.SendStream(GStreamRoutes.ContentTyped);
      except
        on Exception do
          Res.Status(501).ContentType('application/json; charset=utf-8')
             .Send('{"error":"streaming not implemented on this transport"}');
      end;
    end
  );

  THorse.Get('/stream/empty',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/plain');
      try
        Res.SendStream(GStreamRoutes.Empty);
      except
        on Exception do
          Res.Status(501).ContentType('application/json; charset=utf-8')
             .Send('{"error":"streaming not implemented on this transport"}');
      end;
    end
  );

end;

initialization
  GStreamRoutes := TStreamRoutes.Create;

finalization
  GStreamRoutes.Free;

end.
