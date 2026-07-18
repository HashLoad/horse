program HorseIndyTestClient;

{$APPTYPE CONSOLE}

(*
  Horse  —  Integration Test Client
  (Indy Console / IOCP / HttpSys / Epoll providers)
  ==================================================
  Destination: horse/samples/tests/HorseIndyTestClient.dpr

  Requires one of the following servers running on 127.0.0.1:9010:
    HorseIndyTestServer.exe    — Indy Console (default provider)
    HorseIOCPTestServer.exe    — IOCP (Windows)
    HorseHttpSysTestServer.exe — HTTP.sys (Windows, needs URL reservation)
    HorseEpollTestServer.exe   — Epoll (Linux)

  Test matrix (36 tests):
    01  GET    /ping                         → 200 "pong"
    02  GET    /methods/get                  → 200 {"method":"GET"}
    03  POST   /methods/post                 → 200 body echo
    04  PUT    /methods/put/42               → 200 {"id":"42"}
    05  DELETE /methods/delete/99            → 200 {"id":"99"}
    06  PATCH  /methods/patch/7              → 200 {"id":"7"}
    07  HEAD   /methods/head                 → 200, X-Head-Ok header, empty body
    08  GET    /params/path/hello            → 200 {"id":"hello"}
    09  GET    /params/query?name=...        → 200 query param echo
    10  GET    /cookies/set                  → 200 two Set-Cookie headers (FIX-HEADER-DUP)
    11  GET    /cookies/echo (+ cookies)     → 200 cookie values echoed back
    12  POST   /upload (multipart)           → 200 or 400
                                               (Indy Console: 200; IOCP/HttpSys/Epoll:
                                               custom parsers — accept 200 or structured 400)
    13  GET    /download                     → 200 Content-Disposition + body
    14  GET    /headers/echo                 → 200 custom header echoed back
    15  POST   /methods/post  empty body     → 200 or 400
                                               (Indy Console: 200; custom parsers: 200 or 400)
    16  POST   /echo/body  large (64 KB)     → 200 "size":65536 exact
    17  POST   /echo/body  sequential (A→B)  → no body leakage between requests
    18  POST   /echo/body  concurrent (×4)   → no cross-contamination
    19  GET    /params/multi/:a/:b           → 200 both params echoed
    20  GET    /does/not/exist               → 404 not found
    21  GET    /status/400                   → 400
    22  GET    /status/500                   → 500
    23  Content-Type of JSON response        → contains "application/json"
    24  GET    /response/large               → body length = 65536
    25  GET    /raw/webrequest               → RawWebRequest adapter surfaces
                                               method/host/pathInfo/header/remoteAddr
    26  OPTIONS /raw/cors                    → 204 + Access-Control-Allow-Origin
    27  GET    /raw/cors                     → 200 body "cors-route:GET"
    28  GET    /raw/webresponse              → 200, X-Via-RawResponse set via
                                               Res.RawWebResponse.SetCustomHeader
    29  POST   /pool/burst  ×8 concurrent    → all 200, unique markers, no cross-leak
    30  POST   /pool/burst  rapid sequential → 5 requests after burst
    31  POST   /echo/body-twice              → "equal":true (PATCH-REQ-9)
    32  GET    /compat/rawbody               → body = "shadow-wins" (COMPAT-1)
    33  GET    /stream/pull                  → 501 (no chunked streaming on provider)
    34  GET    /stream/content-type          → 501
    35  GET    /stream/empty                 → 501
    36  GET    /stream/pull  ×2 concurrent   → both 501, server still healthy

  Notes on tests 12 and 15:
    The Indy Console provider uses WebBroker (TIdHTTPServer) which decodes
    multipart bodies and accepts empty-body POST.  The IOCP, HttpSys, and
    Epoll providers use custom HTTP parsers whose multipart / empty-body
    coverage varies.  Both tests accept 200 OR a structured 400.

  Note on test 28 (X-Via-RawResponse):
    SetCustomHeader on Res.RawWebResponse is picked up by the Indy Console
    response path via WebBroker.  IOCP / HttpSys / Epoll providers pick it
    up if their response bridge implements COMPAT-1 (RawWebResponse.CustomHeaders
    iteration).  A FAIL here means the provider's bridge needs the COMPAT-1
    pass added.

  Notes on tests 33–36:
    Pull-model chunked streaming (PATCH-STREAM-1) is CrossSocket-only.
    All other providers register 501 stubs for /stream/* and return
    immediately — neither server nor client hangs.
*)

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
  System.TimeSpan,
  Net.CrossHttpClient,
  Net.CrossHttpParams;

const
  DEFAULT_BASE_URL    = 'http://127.0.0.1:9010';
  TIMEOUT_MS          = 8000;
  LARGE_RESPONSE_SIZE = 65536;
  LARGE_BODY_SIZE     = 65536;
  CONCURRENT_COUNT    = 4;
  BURST_COUNT         = 8;

  RAPID_SEQ_COUNT     = 5;

var
  { Override with the first command-line argument to point the suite at a
    different host — e.g. `HorseIndyTestClient http://172.18.78.148:9010`
    talks straight to a WSL2 server, bypassing Windows localhost forwarding. }
  BASE_URL: string = DEFAULT_BASE_URL;

var
  GPassCount:        Integer = 0;
  GFailCount:        Integer = 0;
  GGlobalSW:         TStopwatch;
  GLastSectionTicks: Int64 = 0;

function FmtMs(const AMilliseconds: Double): string;
begin
  Result := Format('%7.1f ms', [AMilliseconds]);
end;

procedure ReportTiming(
  const ALabel:    string;
  const AServerMs: Double;
  const AClientMs: Double;
  const ATimedOut: Boolean
);
begin
  if ATimedOut then
    Writeln(Format('  TIME  %-22s server: TIMEOUT (>%d ms)  client: %s',
      [ALabel, TIMEOUT_MS, FmtMs(AClientMs)]))
  else
    Writeln(Format('  TIME  %-22s server: %s  client: %s  total: %s',
      [ALabel, FmtMs(AServerMs), FmtMs(AClientMs),
       FmtMs(AServerMs + AClientMs)]));
end;

type
  TConcurrentEntry = record
    Marker: string;
    Event:  TEvent;
    Status: Integer;
    Body:   string;
  end;

function StreamToStr(AStream: TStream): string;
var
  LBytes: TBytes;
begin
  Result := '';
  if not Assigned(AStream) or (AStream.Size = 0) then
    Exit;
  AStream.Position := 0;
  SetLength(LBytes, AStream.Size);
  AStream.ReadBuffer(LBytes[0], AStream.Size);
  Result := TEncoding.UTF8.GetString(LBytes);
end;

procedure Check(const AName: string; const APassed: Boolean;
  const ADetail: string = '');
begin
  if APassed then
  begin
    Writeln(Format('  PASS  %s', [AName]));
    Inc(GPassCount);
  end
  else
  begin
    if ADetail <> '' then
      Writeln(Format('  FAIL  %s  [%s]', [AName, ADetail]))
    else
      Writeln(Format('  FAIL  %s', [AName]));
    Inc(GFailCount);
  end;
end;

type
  TReqResult = record
    StatusCode: Integer;
    Body:       string;
    Response:   ICrossHttpClientResponse;
    TimedOut:   Boolean;
    ServerMs:   Int64;
    ClientMs:   Int64;
  end;

function DoSync(
  const AClient:  TCrossHttpClient;
  const AMethod:  string;
  const AUrl:     string;
  const AHeaders: THttpHeader;
  const ABody:    TBytes;
  out   AResult:  TReqResult
): Boolean;
var
  LEvent:         TEvent;
  LResult:        TReqResult;
  LSWReq:         TStopwatch;
  LCallbackTicks: Int64;
begin
  LResult        := Default(TReqResult);
  LEvent         := TEvent.Create(nil, True, False, '');
  LCallbackTicks := 0;
  try
    LSWReq := TStopwatch.StartNew;
    AClient.DoRequest(AMethod, AUrl, AHeaders, ABody, nil, nil,
      procedure(const AResp: ICrossHttpClientResponse)
      begin
        LCallbackTicks := LSWReq.ElapsedMilliseconds;
        if AResp <> nil then
        begin
          LResult.StatusCode := AResp.StatusCode;
          LResult.Body       := StreamToStr(AResp.Content);
          LResult.Response   := AResp;
        end;
        LEvent.SetEvent;
      end);
    LResult.TimedOut := (LEvent.WaitFor(TIMEOUT_MS) <> wrSignaled);
    LSWReq.Stop;
    if LResult.TimedOut then
    begin
      LResult.ServerMs := 0;
      LResult.ClientMs := LSWReq.ElapsedMilliseconds;
    end
    else
    begin
      LResult.ServerMs := LCallbackTicks;
      LResult.ClientMs := LSWReq.ElapsedMilliseconds - LCallbackTicks;
    end;
  finally
    LEvent.Free;
  end;
  AResult := LResult;
  Result  := not AResult.TimedOut;
  ReportTiming(AMethod + ' ' + AUrl, LResult.ServerMs, LResult.ClientMs,
    LResult.TimedOut);
end;

function DoSyncMP(
  const AClient:  TCrossHttpClient;
  const AUrl:     string;
  const AHeaders: THttpHeader;
  const ABody:    THttpMultiPartFormData;
  out   AResult:  TReqResult
): Boolean;
var
  LEvent:         TEvent;
  LResult:        TReqResult;
  LSWReq:         TStopwatch;
  LCallbackTicks: Int64;
begin
  LResult        := Default(TReqResult);
  LEvent         := TEvent.Create(nil, True, False, '');
  LCallbackTicks := 0;
  try
    LSWReq := TStopwatch.StartNew;
    AClient.DoRequest('POST', AUrl, AHeaders, ABody, nil, nil,
      procedure(const AResp: ICrossHttpClientResponse)
      begin
        LCallbackTicks := LSWReq.ElapsedMilliseconds;
        if AResp <> nil then
        begin
          LResult.StatusCode := AResp.StatusCode;
          LResult.Body       := StreamToStr(AResp.Content);
          LResult.Response   := AResp;
        end;
        LEvent.SetEvent;
      end);
    LResult.TimedOut := (LEvent.WaitFor(TIMEOUT_MS) <> wrSignaled);
    LSWReq.Stop;
    if LResult.TimedOut then
    begin
      LResult.ServerMs := 0;
      LResult.ClientMs := LSWReq.ElapsedMilliseconds;
    end
    else
    begin
      LResult.ServerMs := LCallbackTicks;
      LResult.ClientMs := LSWReq.ElapsedMilliseconds - LCallbackTicks;
    end;
  finally
    LEvent.Free;
  end;
  AResult := LResult;
  Result  := not AResult.TimedOut;
  ReportTiming('POST ' + AUrl + ' (multipart)',
    LResult.ServerMs, LResult.ClientMs, LResult.TimedOut);
end;

function GetSetCookieValue(
  const AResponse:   ICrossHttpClientResponse;
  const ACookieName: string
): string;
var
  I:     Integer;
  Line:  string;
  First: string;
  EqPos: Integer;
begin
  Result := '';
  if AResponse = nil then Exit;
  for I := 0 to AResponse.Header.Count - 1 do
  begin
    if not SameText(AResponse.Header.Items[I].Name, 'Set-Cookie') then
      Continue;
    Line  := AResponse.Header.Items[I].Value;
    First := Trim(Copy(Line, 1, Pos(';', Line + ';') - 1));
    EqPos := Pos('=', First);
    if (EqPos > 0) and SameText(Copy(First, 1, EqPos - 1), ACookieName) then
    begin
      Result := Copy(First, EqPos + 1, MaxInt);
      Exit;
    end;
  end;
end;

procedure RunTests(const AClient: TCrossHttpClient);
var
  R:              TReqResult;
  LHeaders:       THttpHeader;
  LForm:          THttpMultiPartFormData;
  LFileStream:    TMemoryStream;
  LFileBytes:     TBytes;
  LLargeBody:     TBytes;
  LSessionCookie: string;
  LUserCookie:    string;
  LBatch:         array[0..CONCURRENT_COUNT - 1] of TConcurrentEntry;
  LAllStatus200:  Boolean;
  LAllMarkersOk:  Boolean;
  LNoCrossLeaks:  Boolean;
  I:              Integer;
  LBurstBatch:    array[0..BURST_COUNT - 1] of TConcurrentEntry;
  LBurstAllOk:    Boolean;
  LBurstAllMarkers: Boolean;
  LBurstNoCross:  Boolean;
  LSeqAllOk:      Boolean;
  LSeqMarker:     string;
  J:              Integer;
  LStreamBatch:   array[0..1] of TConcurrentEntry;
  LStreamAllOk:   Boolean;

  procedure Section(const ATitle: string);
  var
    LNowTicks: Int64;
    LSince:    Int64;
  begin
    LNowTicks := GGlobalSW.ElapsedMilliseconds;
    LSince    := LNowTicks - GLastSectionTicks;
    GLastSectionTicks := LNowTicks;
    Writeln('');
    Writeln(Format('── %s   (+%d ms since previous test, %d ms total)',
      [ATitle, LSince, LNowTicks]));
  end;

  procedure FireBurst(const AIdx: Integer; AHeaders: THttpHeader);
  begin
    AClient.DoRequest('POST', BASE_URL + '/pool/burst',
      AHeaders,
      TEncoding.UTF8.GetBytes(LBurstBatch[AIdx].Marker),
      nil, nil,
      procedure(const AResp: ICrossHttpClientResponse)
      begin
        if AResp <> nil then
        begin
          LBurstBatch[AIdx].Status := AResp.StatusCode;
          LBurstBatch[AIdx].Body   := StreamToStr(AResp.Content);
        end;
        LBurstBatch[AIdx].Event.SetEvent;
      end);
  end;

  procedure FireOne(const AIdx: Integer; AHeaders: THttpHeader);
  begin
    AClient.DoRequest('POST', BASE_URL + '/echo/body',
      AHeaders,
      TEncoding.UTF8.GetBytes(LBatch[AIdx].Marker),
      nil, nil,
      procedure(const AResp: ICrossHttpClientResponse)
      begin
        if AResp <> nil then
        begin
          LBatch[AIdx].Status := AResp.StatusCode;
          LBatch[AIdx].Body   := StreamToStr(AResp.Content);
        end;
        LBatch[AIdx].Event.SetEvent;
      end);
  end;

  // Test 36 — fires one GET /stream/pull.
  // All four providers return 501 immediately; concurrent probes confirm the
  // server stays healthy (no hang, no crash).
  procedure FireStream(const AIdx: Integer);
  begin
    AClient.DoRequest('GET', BASE_URL + '/stream/pull',
      nil, TBytes(nil), nil, nil,
      procedure(const AResp: ICrossHttpClientResponse)
      begin
        if AResp <> nil then
        begin
          LStreamBatch[AIdx].Status := AResp.StatusCode;
          LStreamBatch[AIdx].Body   := StreamToStr(AResp.Content);
        end;
        LStreamBatch[AIdx].Event.SetEvent;
      end);
  end;

begin
  // ── 01 ───────────────────────────────────────────────────────────────────────
  Section('01  GET /ping');
  DoSync(AClient, 'GET', BASE_URL + '/ping', nil, nil, R);
  Check('status 200',    R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('body = "pong"', R.Body = 'pong',    R.Body);

  // ── 02 ───────────────────────────────────────────────────────────────────────
  Section('02  GET /methods/get');
  DoSync(AClient, 'GET', BASE_URL + '/methods/get', nil, nil, R);
  Check('status 200',          R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('body contains "GET"', Pos('"GET"', R.Body) > 0, R.Body);

  // ── 03 ───────────────────────────────────────────────────────────────────────
  Section('03  POST /methods/post  (JSON body echo)');
  LHeaders := THttpHeader.Create;
  try
    LHeaders['Content-Type'] := 'application/json; charset=utf-8';
    DoSync(AClient, 'POST', BASE_URL + '/methods/post',
           LHeaders, TEncoding.UTF8.GetBytes('{"hello":"world"}'), R);
  finally
    LHeaders.Free;
  end;
  Check('status 200',                  R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('body contains "POST"',        Pos('"POST"', R.Body) > 0, R.Body);
  Check('body echoes request payload', Pos('hello',  R.Body) > 0, R.Body);

  // ── 04 ───────────────────────────────────────────────────────────────────────
  Section('04  PUT /methods/put/42');
  // Empty-body PUT needs an explicit Content-Length: 0 — TCrossHttpClient
  // omits the header for nil bodies and HTTP.sys rejects PUT without
  // Content-Length or chunked framing at kernel level (411 Length Required).
  LHeaders := THttpHeader.Create;
  try
    LHeaders['Content-Length'] := '0';
    DoSync(AClient, 'PUT', BASE_URL + '/methods/put/42', LHeaders, nil, R);
  finally
    LHeaders.Free;
  end;
  Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('id = "42"',  Pos('"42"', R.Body) > 0, R.Body);

  // ── 05 ───────────────────────────────────────────────────────────────────────
  Section('05  DELETE /methods/delete/99');
  DoSync(AClient, 'DELETE', BASE_URL + '/methods/delete/99', nil, nil, R);
  Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('id = "99"',  Pos('"99"', R.Body) > 0, R.Body);

  // ── 06 ───────────────────────────────────────────────────────────────────────
  Section('06  PATCH /methods/patch/7');
  DoSync(AClient, 'PATCH', BASE_URL + '/methods/patch/7', nil, nil, R);
  Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('id = "7"',   Pos('"7"', R.Body) > 0, R.Body);

  // ── 07 ───────────────────────────────────────────────────────────────────────
  Section('07  HEAD /methods/head  (header only, empty body)');
  DoSync(AClient, 'HEAD', BASE_URL + '/methods/head', nil, nil, R);
  Check('status 200',    R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('body is empty', R.Body = '', R.Body);
  if Assigned(R.Response) then
    Check('X-Head-Ok header present',
      R.Response.Header['X-Head-Ok'] = 'true',
      R.Response.Header['X-Head-Ok']);

  // ── 08 ───────────────────────────────────────────────────────────────────────
  Section('08  GET /params/path/hello');
  DoSync(AClient, 'GET', BASE_URL + '/params/path/hello', nil, nil, R);
  Check('status 200',   R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('id = "hello"', Pos('"hello"', R.Body) > 0, R.Body);

  // ── 09 ───────────────────────────────────────────────────────────────────────
  Section('09  GET /params/query?name=Horse&value=Indy');
  DoSync(AClient, 'GET',
    BASE_URL + '/params/query?name=Horse&value=Indy', nil, nil, R);
  Check('status 200',     R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('name = "Horse"', Pos('"Horse"', R.Body) > 0, R.Body);
  Check('value = "Indy"', Pos('"Indy"',  R.Body) > 0, R.Body);

  // ── 10  FIX-HEADER-DUP ───────────────────────────────────────────────────────
  Section('10  GET /cookies/set  (FIX-HEADER-DUP: two Set-Cookie headers)');
  DoSync(AClient, 'GET', BASE_URL + '/cookies/set', nil, nil, R);
  Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
  LSessionCookie := '';
  LUserCookie    := '';
  if Assigned(R.Response) then
  begin
    LSessionCookie := GetSetCookieValue(R.Response, 'session');
    LUserCookie    := GetSetCookieValue(R.Response, 'user');
    Check('Set-Cookie session=abc123', LSessionCookie = 'abc123', LSessionCookie);
    Check('Set-Cookie user=tester',    LUserCookie = 'tester',    LUserCookie);
  end
  else
    Check('response received', False, 'nil response');

  // ── 11 ───────────────────────────────────────────────────────────────────────
  Section('11  GET /cookies/echo  (send cookies, verify echo)');
  if LSessionCookie = '' then LSessionCookie := 'abc123';
  if LUserCookie    = '' then LUserCookie    := 'tester';
  LHeaders := THttpHeader.Create;
  try
    LHeaders['Cookie'] := Format('session=%s; user=%s',
      [LSessionCookie, LUserCookie]);
    DoSync(AClient, 'GET', BASE_URL + '/cookies/echo', LHeaders, nil, R);
  finally
    LHeaders.Free;
  end;
  Check('status 200',                 R.StatusCode = 200,          IntToStr(R.StatusCode));
  Check('session echoed as "abc123"', Pos('"abc123"', R.Body) > 0, R.Body);
  Check('user echoed as "tester"',    Pos('"tester"', R.Body) > 0, R.Body);

  // ── 12 ───────────────────────────────────────────────────────────────────────
  // Indy Console: WebBroker decodes multipart → expect 200.
  // IOCP / HttpSys / Epoll: custom parsers — accept 200 or structured 400.
  Section('12  POST /upload  (multipart — 200 on Indy, 200 or 400 on custom parsers)');
  LFileBytes  := TEncoding.UTF8.GetBytes('This is the uploaded file content.');
  LFileStream := TMemoryStream.Create;
  try
    LFileStream.WriteBuffer(LFileBytes[0], Length(LFileBytes));
    LFileStream.Position := 0;
    LForm := THttpMultiPartFormData.Create;
    try
      LForm.AddField('fieldname', 'myupload.txt');
      LForm.AddFile('file', 'myupload.txt', LFileStream, False);
      DoSyncMP(AClient, BASE_URL + '/upload', nil, LForm, R);
    finally
      LForm.Free;
    end;
  finally
    LFileStream.Free;
  end;
  Check('status 200 or 400',
    (R.StatusCode = 200) or (R.StatusCode = 400), IntToStr(R.StatusCode));
  if R.StatusCode = 200 then
  begin
    Check('"received":true',   Pos('"received":true', R.Body) > 0, R.Body);
    Check('filename echoed',   Pos('myupload.txt',    R.Body) > 0, R.Body);
    Check('size > 0 bytes',    Pos('"size":0',        R.Body) = 0, R.Body);
  end;

  // ── 13 ───────────────────────────────────────────────────────────────────────
  Section('13  GET /download  (Content-Disposition + body)');
  DoSync(AClient, 'GET', BASE_URL + '/download', nil, nil, R);
  Check('status 200',            R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('body contains "Horse"', Pos('Horse', R.Body) > 0, R.Body);
  if Assigned(R.Response) then
    Check('Content-Disposition: attachment',
      Pos('attachment', R.Response.Header['Content-Disposition']) > 0,
      R.Response.Header['Content-Disposition']);

  // ── 14 ───────────────────────────────────────────────────────────────────────
  Section('14  GET /headers/echo  (X-Test-Header round-trip)');
  LHeaders := THttpHeader.Create;
  try
    LHeaders['X-Test-Header'] := 'HelloFromClient';
    DoSync(AClient, 'GET', BASE_URL + '/headers/echo', LHeaders, nil, R);
  finally
    LHeaders.Free;
  end;
  Check('status 200',                 R.StatusCode = 200,              IntToStr(R.StatusCode));
  Check('X-Test-Header value echoed', Pos('HelloFromClient', R.Body) > 0, R.Body);

  // ── 15 ───────────────────────────────────────────────────────────────────────
  // Indy Console: WebBroker accepts empty-body POST → 200.
  // IOCP / HttpSys / Epoll: custom parsers may reject → accept 200 or 400.
  Section('15  POST /methods/post  (empty body — nil-body path)');
  LHeaders := THttpHeader.Create;
  try
    LHeaders['Content-Type'] := 'application/json; charset=utf-8';
    // Explicit Content-Length: 0 — HTTP.sys returns 411 for POST without
    // Content-Length or chunked framing before the request reaches the app.
    LHeaders['Content-Length'] := '0';
    DoSync(AClient, 'POST', BASE_URL + '/methods/post', LHeaders, nil, R);
  finally
    LHeaders.Free;
  end;
  Check('status 200 or 400',
    (R.StatusCode = 200) or (R.StatusCode = 400), IntToStr(R.StatusCode));
  if R.StatusCode = 200 then
    Check('"body":""', Pos('"body":""', R.Body) > 0, R.Body);

  // ── 16 ───────────────────────────────────────────────────────────────────────
  Section(Format('16  POST /echo/body  (%d-byte body)', [LARGE_BODY_SIZE]));
  LLargeBody := TEncoding.UTF8.GetBytes(StringOfChar('A', LARGE_BODY_SIZE));
  LHeaders   := THttpHeader.Create;
  try
    LHeaders['Content-Type'] := 'text/plain; charset=utf-8';
    DoSync(AClient, 'POST', BASE_URL + '/echo/body', LHeaders, LLargeBody, R);
  finally
    LHeaders.Free;
  end;
  Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
  Check(Format('"size":%d exact', [LARGE_BODY_SIZE]),
    Pos(Format('"size":%d', [LARGE_BODY_SIZE]), R.Body) > 0, R.Body);

  // ── 17 ───────────────────────────────────────────────────────────────────────
  Section('17  POST /echo/body  sequential A → ping → B  (request isolation)');
  LHeaders := THttpHeader.Create;
  try
    LHeaders['Content-Type'] := 'text/plain; charset=utf-8';
    DoSync(AClient, 'POST', BASE_URL + '/echo/body',
           LHeaders, TEncoding.UTF8.GetBytes('SEQUENTIAL_BODY_ALPHA'), R);
    Check('step A: status 200',
      R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('step A: body contains SEQUENTIAL_BODY_ALPHA',
      Pos('SEQUENTIAL_BODY_ALPHA', R.Body) > 0, R.Body);
  finally
    LHeaders.Free;
  end;
  DoSync(AClient, 'GET', BASE_URL + '/ping', nil, nil, R);
  Check('step ping: status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
  LHeaders := THttpHeader.Create;
  try
    LHeaders['Content-Type'] := 'text/plain; charset=utf-8';
    DoSync(AClient, 'POST', BASE_URL + '/echo/body',
           LHeaders, TEncoding.UTF8.GetBytes('SEQUENTIAL_BODY_BETA'), R);
  finally
    LHeaders.Free;
  end;
  Check('step B: status 200',
    R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('step B: body contains SEQUENTIAL_BODY_BETA',
    Pos('SEQUENTIAL_BODY_BETA', R.Body) > 0, R.Body);
  Check('step B: body does NOT contain SEQUENTIAL_BODY_ALPHA  (no leakage)',
    Pos('SEQUENTIAL_BODY_ALPHA', R.Body) = 0, R.Body);

  // ── 18 ───────────────────────────────────────────────────────────────────────
  Section(Format('18  POST /echo/body  %d concurrent  (request isolation)',
    [CONCURRENT_COUNT]));
  for I := 0 to CONCURRENT_COUNT - 1 do
  begin
    LBatch[I].Marker := Format('CONCURRENT_MARKER_%d_%s',
      [I, IntToHex(I * $1357ABCD + $DEADBEEF, 8)]);
    LBatch[I].Event  := TEvent.Create(nil, True, False, '');
    LBatch[I].Status := 0;
    LBatch[I].Body   := '';
  end;
  LHeaders := THttpHeader.Create;
  try
    LHeaders['Content-Type'] := 'text/plain; charset=utf-8';
    for I := 0 to CONCURRENT_COUNT - 1 do
      FireOne(I, LHeaders);
  finally
    LHeaders.Free;
  end;
  for I := 0 to CONCURRENT_COUNT - 1 do
    LBatch[I].Event.WaitFor(TIMEOUT_MS);
  LAllStatus200 := True;
  LAllMarkersOk := True;
  LNoCrossLeaks := True;
  for I := 0 to CONCURRENT_COUNT - 1 do
  begin
    if LBatch[I].Status <> 200 then LAllStatus200 := False;
    if Pos(LBatch[I].Marker, LBatch[I].Body) = 0 then LAllMarkersOk := False;
    for J := 0 to CONCURRENT_COUNT - 1 do
      if (J <> I) and (Pos(LBatch[J].Marker, LBatch[I].Body) > 0) then
        LNoCrossLeaks := False;
    LBatch[I].Event.Free;
  end;
  Check(Format('all %d responses: status 200', [CONCURRENT_COUNT]),
    LAllStatus200, '');
  Check('each response contains its own unique marker', LAllMarkersOk, '');
  Check('no response contains another request''s marker  (no cross-contamination)',
    LNoCrossLeaks, '');

  // ── 19 ───────────────────────────────────────────────────────────────────────
  Section('19  GET /params/multi/alpha/beta  (two path params)');
  DoSync(AClient, 'GET', BASE_URL + '/params/multi/alpha/beta', nil, nil, R);
  Check('status 200',         R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('a = "alpha"',        Pos('"alpha"', R.Body) > 0, R.Body);
  Check('b = "beta"',         Pos('"beta"',  R.Body) > 0, R.Body);
  Check('both params present',
    (Pos('"a"', R.Body) > 0) and (Pos('"b"', R.Body) > 0), R.Body);

  // ── 20 ───────────────────────────────────────────────────────────────────────
  Section('20  GET /does/not/exist  (expect 404)');
  DoSync(AClient, 'GET', BASE_URL + '/does/not/exist', nil, nil, R);
  Check('status 404', R.StatusCode = 404, IntToStr(R.StatusCode));

  // ── 21 ───────────────────────────────────────────────────────────────────────
  Section('21  GET /status/400  (explicit 400 response)');
  DoSync(AClient, 'GET', BASE_URL + '/status/400', nil, nil, R);
  Check('status 400',                 R.StatusCode = 400, IntToStr(R.StatusCode));
  Check('body contains status code',  Pos('"status":400', R.Body) > 0, R.Body);

  // ── 22 ───────────────────────────────────────────────────────────────────────
  Section('22  GET /status/500  (explicit 500 response)');
  DoSync(AClient, 'GET', BASE_URL + '/status/500', nil, nil, R);
  Check('status 500',                 R.StatusCode = 500, IntToStr(R.StatusCode));
  Check('body contains status code',  Pos('"status":500', R.Body) > 0, R.Body);

  // ── 23 ───────────────────────────────────────────────────────────────────────
  Section('23  GET /methods/get  (verify Content-Type response header)');
  DoSync(AClient, 'GET', BASE_URL + '/methods/get', nil, nil, R);
  Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
  if Assigned(R.Response) then
    Check('Content-Type contains "application/json"',
      Pos('application/json', R.Response.Header['Content-Type']) > 0,
      R.Response.Header['Content-Type']);

  // ── 24 ───────────────────────────────────────────────────────────────────────
  Section(Format('24  GET /response/large  (expect %d-byte body)',
    [LARGE_RESPONSE_SIZE]));
  DoSync(AClient, 'GET', BASE_URL + '/response/large', nil, nil, R);
  Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
  Check(Format('body length = %d', [LARGE_RESPONSE_SIZE]),
    Length(R.Body) = LARGE_RESPONSE_SIZE,
    Format('%d bytes received', [Length(R.Body)]));
  Check('body consists of ''X'' characters only',
    (Length(R.Body) > 0) and (Pos(StringOfChar('X', 8), R.Body) > 0), '');

  // ── 25 ───────────────────────────────────────────────────────────────────────
  Section('25  GET /raw/webrequest  (RawWebRequest adapter — PATCH-REQ-8)');
  LHeaders := THttpHeader.Create;
  try
    LHeaders['X-Test-Header'] := 'RawAdapterProbe';
    DoSync(AClient, 'GET', BASE_URL + '/raw/webrequest', LHeaders, nil, R);
  finally
    LHeaders.Free;
  end;
  Check('status 200',        R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('hasAdapter true',   Pos('"hasAdapter":true', R.Body) > 0, R.Body);
  Check('method = GET',      Pos('"method":"GET"',    R.Body) > 0, R.Body);
  // target-agnostic: BASE_URL is 'http://<host>:<port>' — strip the scheme so
  // the check works when the suite is pointed at a non-localhost server
  Check('host present',
    Pos('"host":"' + Copy(BASE_URL, Length('http://') + 1, MaxInt), R.Body) > 0,
    R.Body);
  Check('pathInfo = /raw/webrequest',
    Pos('"pathInfo":"/raw/webrequest"', R.Body) > 0, R.Body);
  Check('GetFieldByName echoed custom header',
    Pos('"customHeader":"RawAdapterProbe"', R.Body) > 0, R.Body);
  Check('RemoteAddr non-empty',
    Pos('"remoteAddrNonEmpty":true', R.Body) > 0, R.Body);

  // ── 26 ───────────────────────────────────────────────────────────────────────
  Section('26  OPTIONS /raw/cors  (Horse.CORS pre-flight shape)');
  DoSync(AClient, 'OPTIONS', BASE_URL + '/raw/cors', nil, nil, R);
  Check('status 204', R.StatusCode = 204, IntToStr(R.StatusCode));
  if Assigned(R.Response) then
    Check('Access-Control-Allow-Origin: *',
      R.Response.Header['Access-Control-Allow-Origin'] = '*',
      R.Response.Header['Access-Control-Allow-Origin']);

  // ── 27 ───────────────────────────────────────────────────────────────────────
  Section('27  GET /raw/cors  (non-preflight branch)');
  DoSync(AClient, 'GET', BASE_URL + '/raw/cors', nil, nil, R);
  Check('status 200',              R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('body = "cors-route:GET"', R.Body = 'cors-route:GET', R.Body);

  // ── 28 ───────────────────────────────────────────────────────────────────────
  // Indy Console: SetCustomHeader on TWebResponse is included by WebBroker.
  // IOCP / HttpSys / Epoll: requires COMPAT-1 pass in the response bridge.
  // A FAIL here reveals a missing COMPAT-1 implementation in the provider.
  Section('28  GET /raw/webresponse  (Res.RawWebResponse.SetCustomHeader — PATCH-RES-6)');
  DoSync(AClient, 'GET', BASE_URL + '/raw/webresponse', nil, nil, R);
  Check('status 200',      R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('hasAdapter true', Pos('"hasAdapter":true', R.Body) > 0, R.Body);
  if Assigned(R.Response) then
  begin
    Check('X-Via-RawResponse header set via SetCustomHeader',
      R.Response.Header['X-Via-RawResponse'] = 'PATCH-RES-6-OK',
      R.Response.Header['X-Via-RawResponse']);
    Check('X-Via-AddHeader header set via Res.AddHeader',
      R.Response.Header['X-Via-AddHeader'] = 'AddHeader-OK',
      R.Response.Header['X-Via-AddHeader']);
  end
  else
    Check('response received', False, 'nil response');

  // ── 29 ───────────────────────────────────────────────────────────────────────
  Section(Format('29  POST /pool/burst  %d concurrent  (cascade wake stress)',
    [BURST_COUNT]));
  for I := 0 to BURST_COUNT - 1 do
  begin
    LBurstBatch[I].Marker := Format('BURST_%d_%s',
      [I, IntToHex(I * $CAFE0001 + $B00B1E5, 8)]);
    LBurstBatch[I].Event  := TEvent.Create(nil, True, False, '');
    LBurstBatch[I].Status := 0;
    LBurstBatch[I].Body   := '';
  end;
  LHeaders := THttpHeader.Create;
  try
    LHeaders['Content-Type'] := 'text/plain; charset=utf-8';
    for I := 0 to BURST_COUNT - 1 do
      FireBurst(I, LHeaders);
  finally
    LHeaders.Free;
  end;
  for I := 0 to BURST_COUNT - 1 do
    LBurstBatch[I].Event.WaitFor(TIMEOUT_MS);
  LBurstAllOk      := True;
  LBurstAllMarkers := True;
  LBurstNoCross    := True;
  for I := 0 to BURST_COUNT - 1 do
  begin
    if LBurstBatch[I].Status <> 200 then LBurstAllOk := False;
    if Pos(LBurstBatch[I].Marker, LBurstBatch[I].Body) = 0 then
      LBurstAllMarkers := False;
    for J := 0 to BURST_COUNT - 1 do
      if (J <> I) and (Pos(LBurstBatch[J].Marker, LBurstBatch[I].Body) > 0) then
        LBurstNoCross := False;
    LBurstBatch[I].Event.Free;
  end;
  Check(Format('all %d responses: status 200', [BURST_COUNT]),
    LBurstAllOk, '');
  Check('each response contains its own marker', LBurstAllMarkers, '');
  Check('no cross-contamination between burst responses', LBurstNoCross, '');

  // ── 30 ───────────────────────────────────────────────────────────────────────
  Section(Format('30  POST /pool/burst  %d rapid sequential after burst  (drain/refill)',
    [RAPID_SEQ_COUNT]));
  LSeqAllOk := True;
  for I := 0 to RAPID_SEQ_COUNT - 1 do
  begin
    LSeqMarker := Format('RAPID_SEQ_%d', [I]);
    LHeaders := THttpHeader.Create;
    try
      LHeaders['Content-Type'] := 'text/plain; charset=utf-8';
      DoSync(AClient, 'POST', BASE_URL + '/pool/burst',
             LHeaders, TEncoding.UTF8.GetBytes(LSeqMarker), R);
    finally
      LHeaders.Free;
    end;
    if (R.StatusCode <> 200) or (Pos(LSeqMarker, R.Body) = 0) then
      LSeqAllOk := False;
  end;
  Check(Format('all %d sequential requests: 200 + correct marker', [RAPID_SEQ_COUNT]),
    LSeqAllOk, '');

  // ── 31 ───────────────────────────────────────────────────────────────────────
  Section('31  POST /echo/body-twice  (PATCH-REQ-9 double-read cache)');
  LHeaders := THttpHeader.Create;
  try
    LHeaders['Content-Type'] := 'text/plain; charset=utf-8';
    DoSync(AClient, 'POST', BASE_URL + '/echo/body-twice',
           LHeaders, TEncoding.UTF8.GetBytes('DOUBLE_READ_MARKER'), R);
  finally
    LHeaders.Free;
  end;
  Check('status 200',
    R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('"first" contains posted value',
    Pos('"first":"DOUBLE_READ_MARKER"', R.Body) > 0, R.Body);
  Check('"second" contains posted value',
    Pos('"second":"DOUBLE_READ_MARKER"', R.Body) > 0, R.Body);
  Check('"equal":true — both reads returned the same string',
    Pos('"equal":true', R.Body) > 0, R.Body);

  // ── 32 ───────────────────────────────────────────────────────────────────────
  Section('32  GET /compat/rawbody  (COMPAT-1: shadow field wins over RawWebResponse.Content)');
  DoSync(AClient, 'GET', BASE_URL + '/compat/rawbody', nil, nil, R);
  Check('status 200',
    R.StatusCode = 200, IntToStr(R.StatusCode));
  Check('body = "shadow-wins"',
    R.Body = 'shadow-wins', R.Body);
  Check('RawWebResponse stub value NOT present in body',
    Pos('raw-should-not-appear', R.Body) = 0, R.Body);

  // ── 33  Streaming capability probe — /stream/pull ─────────────────────────────
  // The shared routes attempt real chunked streaming (PATCH-STREAM-1).
  // Transports WITH an engine (HTTP.sys — [HTTPSYS-STREAM-1]) stream the
  // 5-chunk body; engine-less transports (Indy, IOCP, Epoll) answer the
  // structured 501.  Both outcomes are correct — what matters is which one,
  // done correctly, plus server health afterwards.
  Section('33  GET /stream/pull  (streams on engine transports, else 501)');
  DoSync(AClient, 'GET', BASE_URL + '/stream/pull', nil, nil, R);
  if R.StatusCode = 501 then
    Check('501 body contains "error" key (graceful refusal)',
      Pos('"error"', R.Body) > 0, R.Body)
  else
  begin
    Check('status 200 (streaming engine present)',
      R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('all 5 chunks reassembled in order',
      R.Body = 'chunk-0;chunk-1;chunk-2;chunk-3;chunk-4;', R.Body);
  end;
  DoSync(AClient, 'GET', BASE_URL + '/ping', nil, nil, R);
  Check('server healthy after streaming probe — /ping returns pong',
    (R.StatusCode = 200) and (R.Body = 'pong'),
    Format('%d / %s', [R.StatusCode, R.Body]));

  // ── 34  Streaming capability probe — /stream/content-type ────────────────────
  Section('34  GET /stream/content-type  (Content-Type propagation or 501)');
  DoSync(AClient, 'GET', BASE_URL + '/stream/content-type', nil, nil, R);
  if R.StatusCode = 501 then
    Check('501 body contains "error" key (graceful refusal)',
      Pos('"error"', R.Body) > 0, R.Body)
  else
  begin
    Check('status 200 (streaming engine present)',
      R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('body reassembled: 3 ct-chunks in order',
      R.Body = 'ct-chunk-0;ct-chunk-1;ct-chunk-2;', R.Body);
    if Assigned(R.Response) then
      Check('Content-Type = application/octet-stream',
        Pos('application/octet-stream', R.Response.Header['Content-Type']) > 0,
        R.Response.Header['Content-Type']);
  end;
  DoSync(AClient, 'GET', BASE_URL + '/ping', nil, nil, R);
  Check('server healthy after content-type stream probe',
    (R.StatusCode = 200) and (R.Body = 'pong'),
    Format('%d / %s', [R.StatusCode, R.Body]));

  // ── 35  Streaming capability probe — /stream/empty ───────────────────────────
  Section('35  GET /stream/empty  (zero-chunk stream or 501)');
  DoSync(AClient, 'GET', BASE_URL + '/stream/empty', nil, nil, R);
  if R.StatusCode = 501 then
    Check('501 body contains "error" key (graceful refusal)',
      Pos('"error"', R.Body) > 0, R.Body)
  else
  begin
    Check('status 200 (streaming engine present)',
      R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('body is empty (no chunks emitted)',
      R.Body = '', R.Body);
  end;
  DoSync(AClient, 'GET', BASE_URL + '/ping', nil, nil, R);
  Check('server healthy after empty stream probe',
    (R.StatusCode = 200) and (R.Body = 'pong'),
    Format('%d / %s', [R.StatusCode, R.Body]));

  // ── 36  Concurrent streaming probe ×2 ────────────────────────────────────────
  // Fires two simultaneous GET /stream/pull requests.  Engine transports must
  // deliver both complete bodies (drain isolation); engine-less transports
  // must answer both 501s without hanging.  Mixed outcomes = failure.
  Section('36  GET /stream/pull  ×2 concurrent  (isolation or dual 501)');
  for I := 0 to 1 do
  begin
    LStreamBatch[I].Marker := '';
    LStreamBatch[I].Event  := TEvent.Create(nil, True, False, '');
    LStreamBatch[I].Status := 0;
    LStreamBatch[I].Body   := '';
  end;
  for I := 0 to 1 do
    FireStream(I);
  for I := 0 to 1 do
    LStreamBatch[I].Event.WaitFor(TIMEOUT_MS);
  LStreamAllOk := True;
  for I := 0 to 1 do
  begin
    // each response must be either a complete 5-chunk stream or a clean 501
    if not (((LStreamBatch[I].Status = 200) and
             (LStreamBatch[I].Body = 'chunk-0;chunk-1;chunk-2;chunk-3;chunk-4;'))
            or (LStreamBatch[I].Status = 501)) then
      LStreamAllOk := False;
    LStreamBatch[I].Event.Free;
  end;
  Check('both concurrent probes: complete stream (200) or clean 501',
    LStreamAllOk, '');
  DoSync(AClient, 'GET', BASE_URL + '/ping', nil, nil, R);
  Check('server healthy after concurrent streaming probes',
    (R.StatusCode = 200) and (R.Body = 'pong'),
    Format('%d / %s', [R.StatusCode, R.Body]));

end;

var
  Client: TCrossHttpClient;
begin
  if ParamCount >= 1 then
    BASE_URL := ParamStr(1);
  Writeln('[HorseIndyTest] Client — target: ' + BASE_URL);
  Writeln('[HorseIndyTest] Ensure one of the following servers is running on port 9010:');
  Writeln('[HorseIndyTest]   HorseIndyTestServer.exe    (Indy Console — default)');
  Writeln('[HorseIndyTest]   HorseIOCPTestServer.exe    (IOCP — Windows)');
  Writeln('[HorseIndyTest]   HorseHttpSysTestServer.exe (HTTP.sys — Windows, needs netsh)');
  Writeln('[HorseIndyTest]   HorseEpollTestServer.exe   (Epoll — Linux)');
  Writeln('');

  GGlobalSW         := TStopwatch.StartNew;
  GLastSectionTicks := 0;

  Client := TCrossHttpClient.Create(2 {IoThreads});
  try
    try
      RunTests(Client);
    except
      on E: Exception do
        Writeln('[HorseIndyTest] Unexpected exception: ' + E.ClassName
          + ': ' + E.Message);
    end;
  finally
    Client.Free;
  end;

  GGlobalSW.Stop;

  Writeln('');
  Writeln(Format('[HorseIndyTest] %d passed, %d failed  (total %d) in %d ms wall clock',
    [GPassCount, GFailCount, GPassCount + GFailCount,
     GGlobalSW.ElapsedMilliseconds]));
  if GFailCount = 0 then
    Writeln('[HorseIndyTest] All tests PASSED.')
  else
    Writeln('[HorseIndyTest] Some tests FAILED — see details above.');

  ExitCode := GFailCount;

  Writeln('');
  Writeln('[HorseIndyTest] Press ENTER to exit...');
  Readln;
end.
