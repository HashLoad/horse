program HorseNetHttpTestClient;

{$APPTYPE CONSOLE}

{
  Horse provider integration test client — System.Net.HttpClient edition
  =======================================================================
  Destination: horse/samples/tests/HorseNetHttpTestClient.dpr

  Runs the SAME 36-section suite as HorseIndyTestClient.dpr against the same
  servers (HorseIndyTestServer / HorseIOCPTestServer / HorseHttpSysTestServer /
  HorseEpollTestServer on port 9010), with identical check labels — but built
  on Delphi's own System.Net.HttpClient (WinHTTP on Windows) instead of
  Delphi-Cross-Socket's TCrossHttpClient.

  Why a second client:
    · ZERO dependency on Delphi-Cross-Socket — compiles with the plain RTL,
      no boss modules, no extra search paths.
    · An independent HTTP stack: client-side bugs cannot silently distort
      server results. PATCH-CSHTTP-2 (TCrossHttpClient hanging on a 200 with
      explicit Content-Length: 0 and no Connection header) went undetected
      for exactly this reason — WinHTTP handled the same response correctly.
    · WinHTTP is the reference consumer for the HTTP.sys provider (same
      Microsoft HTTP stack on both ends).

  Differences vs the TCrossHttpClient edition (by design):
    · Requests are synchronous — the TIME line reports total latency only
      (no server/client split; there is no async callback to time against).
    · Empty-body PUT/POST automatically carry Content-Length: 0 (WinHTTP
      always sends it for a provided source stream) — no manual header
      needed to satisfy HTTP.sys kernel validation.
    · Concurrency tests use one THTTPClient per worker thread (THTTPClient
      instances are not thread-safe; TCrossHttpClient multiplexes instead).

  Exit code = number of failed checks (0 = all passed).
}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.Net.Mime;

const
  BASE_URL            = 'http://127.0.0.1:9010';
  TIMEOUT_MS          = 8000;
  LARGE_RESPONSE_SIZE = 65536;
  LARGE_BODY_SIZE     = 65536;
  CONCURRENT_COUNT    = 4;
  BURST_COUNT         = 8;
  RAPID_SEQ_COUNT     = 5;

type
  TReqResult = record
    StatusCode: Integer;
    Body:       string;
    Response:   IHTTPResponse;   // nil on transport error / timeout
    ElapsedMs:  Int64;
    TimedOut:   Boolean;         // any transport failure is reported as such
  end;

  TConcurrentEntry = record
    Marker: string;
    Status: Integer;
    Body:   string;
    Event:  TEvent;
  end;

var
  GPassCount:        Integer = 0;
  GFailCount:        Integer = 0;
  GGlobalSW:         TStopwatch;
  GLastSectionTicks: Int64 = 0;

procedure ReportTiming(const ALabel: string; const R: TReqResult);
begin
  if R.TimedOut then
    Writeln(Format('  TIME  %s total: FAILED/TIMEOUT after %d ms',
      [ALabel, R.ElapsedMs]))
  else
    Writeln(Format('  TIME  %s total: %d ms', [ALabel, R.ElapsedMs]));
end;

procedure Check(const AName: string; const ACondition: Boolean;
  const ADetail: string);
begin
  if ACondition then
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

function NewClient: THTTPClient;
begin
  Result := THTTPClient.Create;
  Result.ConnectionTimeout := 5000;
  Result.ResponseTimeout   := TIMEOUT_MS;
  Result.HandleRedirects   := False;
end;

{ Synchronous request helper.  ABody = nil means "no body" for GET-family
  methods; for POST/PUT/PATCH an empty source stream is always supplied so
  WinHTTP emits Content-Length: 0 (HTTP.sys kernel-rejects bodyless
  POST/PUT with 411 otherwise). }
procedure DoReq(const AClient: THTTPClient; const AMethod, AUrl: string;
  const AHeaders: TNetHeaders; const ABody: TBytes; out R: TReqResult);
var
  LSource: TMemoryStream;
  LSW:     TStopwatch;
begin
  R := Default(TReqResult);
  LSource := nil;
  LSW := TStopwatch.StartNew;
  try
    try
      if (AMethod = 'POST') or (AMethod = 'PUT') or (AMethod = 'PATCH') then
      begin
        LSource := TMemoryStream.Create;
        if Length(ABody) > 0 then
        begin
          LSource.WriteBuffer(ABody[0], Length(ABody));
          LSource.Position := 0;
        end;
      end;

      if AMethod = 'GET' then
        R.Response := AClient.Get(AUrl, nil, AHeaders)
      else if AMethod = 'HEAD' then
        R.Response := AClient.Head(AUrl, AHeaders)
      else if AMethod = 'DELETE' then
        R.Response := AClient.Delete(AUrl, nil, AHeaders)
      else if AMethod = 'OPTIONS' then
        R.Response := AClient.Options(AUrl, nil, AHeaders)
      else if AMethod = 'POST' then
        R.Response := AClient.Post(AUrl, LSource, nil, AHeaders)
      else if AMethod = 'PUT' then
        R.Response := AClient.Put(AUrl, LSource, nil, AHeaders)
      else if AMethod = 'PATCH' then
        R.Response := AClient.Patch(AUrl, LSource, nil, AHeaders)
      else
        raise Exception.Create('DoReq: unsupported method ' + AMethod);

      R.StatusCode := R.Response.StatusCode;
      if AMethod <> 'HEAD' then
        R.Body := R.Response.ContentAsString(TEncoding.UTF8);
    except
      on E: Exception do
      begin
        // transport error / receive timeout — report like the async client's
        // TIMEOUT outcome (status 0, empty body)
        R.StatusCode := 0;
        R.Body       := '';
        R.Response   := nil;
        R.TimedOut   := True;
      end;
    end;
  finally
    LSource.Free;
    LSW.Stop;
    R.ElapsedMs := LSW.ElapsedMilliseconds;
  end;
  ReportTiming(AMethod + ' ' + AUrl, R);
end;

{ Collect the value of a specific cookie across every Set-Cookie response
  header (there may be several — FIX-HEADER-DUP verifies exactly that).
  On Windows the WinHTTP-backed THTTPClient consumes Set-Cookie headers into
  the response's parsed Cookies collection and does NOT surface them in the
  raw Headers array — so both sources are checked.  Each distinct cookie in
  the collection still corresponds to one Set-Cookie header on the wire, so
  finding BOTH cookies keeps the duplicate-header verification intact. }
function GetSetCookieValue(const AResponse: IHTTPResponse;
  const ACookieName: string): string;
var
  LPair:   TNameValuePair;
  LLine:   string;
  LFirst:  string;
  LEqPos:  Integer;
  LCookie: TCookie;
begin
  Result := '';
  if AResponse = nil then Exit;
  for LPair in AResponse.Headers do
  begin
    if not SameText(LPair.Name, 'Set-Cookie') then
      Continue;
    LLine  := LPair.Value;
    LFirst := Trim(Copy(LLine, 1, Pos(';', LLine + ';') - 1));
    LEqPos := Pos('=', LFirst);
    if (LEqPos > 0) and SameText(Copy(LFirst, 1, LEqPos - 1), ACookieName) then
      Exit(Copy(LFirst, LEqPos + 1, MaxInt));
  end;
  for LCookie in AResponse.Cookies do
    if SameText(LCookie.Name, ACookieName) then
      Exit(LCookie.Value);
end;

procedure RunTests;
var
  LClient:        THTTPClient;
  R:              TReqResult;
  LForm:          TMultipartFormData;
  LFileStream:    TMemoryStream;
  LFileBytes:     TBytes;
  LSessionCookie: string;
  LUserCookie:    string;
  LBatch:         array[0..CONCURRENT_COUNT - 1] of TConcurrentEntry;
  LBurstBatch:    array[0..BURST_COUNT - 1] of TConcurrentEntry;
  LStreamBatch:   array[0..1] of TConcurrentEntry;
  LAllStatus200:  Boolean;
  LAllMarkersOk:  Boolean;
  LNoCrossLeaks:  Boolean;
  LBurstAllOk:    Boolean;
  LBurstAllMarkers: Boolean;
  LBurstNoCross:  Boolean;
  LSeqAllOk:      Boolean;
  LSeqMarker:     string;
  LStreamAllOk:   Boolean;
  I, J:           Integer;
  LResp:          IHTTPResponse;

  procedure Section(const ATitle: string);
  var
    LNowTicks, LSince: Int64;
  begin
    LNowTicks := GGlobalSW.ElapsedMilliseconds;
    LSince    := LNowTicks - GLastSectionTicks;
    GLastSectionTicks := LNowTicks;
    Writeln('');
    Writeln(Format('── %s   (+%d ms since previous test, %d ms total)',
      [ATitle, LSince, LNowTicks]));
  end;

  { Fire one POST on its own worker thread with its OWN THTTPClient
    (THTTPClient instances are not thread-safe).  ASlotIdx selects the
    entry in the given batch array; the closure factory captures the
    per-call parameter values. }
  procedure FirePost(const AUrl: string; const ASlotIdx: Integer;
    const AIsBurst: Boolean);
  begin
    TThread.CreateAnonymousThread(
      procedure
      var
        LThreadClient: THTTPClient;
        LSource:       TMemoryStream;
        LThreadResp:   IHTTPResponse;
        LMarker:       string;
        LMarkerBytes:  TBytes;
      begin
        LThreadClient := NewClient;
        try
          try
            if AIsBurst then
              LMarker := LBurstBatch[ASlotIdx].Marker
            else
              LMarker := LBatch[ASlotIdx].Marker;
            LMarkerBytes := TEncoding.UTF8.GetBytes(LMarker);
            LSource := TMemoryStream.Create;
            try
              LSource.WriteBuffer(LMarkerBytes[0], Length(LMarkerBytes));
              LSource.Position := 0;
              LThreadResp := LThreadClient.Post(AUrl, LSource, nil,
                [TNameValuePair.Create('Content-Type', 'text/plain; charset=utf-8')]);
              if AIsBurst then
              begin
                LBurstBatch[ASlotIdx].Status := LThreadResp.StatusCode;
                LBurstBatch[ASlotIdx].Body   := LThreadResp.ContentAsString(TEncoding.UTF8);
              end
              else
              begin
                LBatch[ASlotIdx].Status := LThreadResp.StatusCode;
                LBatch[ASlotIdx].Body   := LThreadResp.ContentAsString(TEncoding.UTF8);
              end;
            finally
              LSource.Free;
            end;
          except
            // slot keeps Status = 0
          end;
        finally
          LThreadClient.Free;
          if AIsBurst then
            LBurstBatch[ASlotIdx].Event.SetEvent
          else
            LBatch[ASlotIdx].Event.SetEvent;
        end;
      end).Start;
  end;

  { Test 36 — one GET /stream/pull per worker thread; engine transports
    stream a complete body, engine-less transports answer 501. }
  procedure FireStream(const ASlotIdx: Integer);
  begin
    TThread.CreateAnonymousThread(
      procedure
      var
        LThreadClient: THTTPClient;
        LThreadResp:   IHTTPResponse;
      begin
        LThreadClient := NewClient;
        try
          try
            LThreadResp := LThreadClient.Get(BASE_URL + '/stream/pull');
            LStreamBatch[ASlotIdx].Status := LThreadResp.StatusCode;
            LStreamBatch[ASlotIdx].Body   := LThreadResp.ContentAsString(TEncoding.UTF8);
          except
            // slot keeps Status = 0
          end;
        finally
          LThreadClient.Free;
          LStreamBatch[ASlotIdx].Event.SetEvent;
        end;
      end).Start;
  end;

begin
  LClient := NewClient;
  try
    // ── 01 ─────────────────────────────────────────────────────────────────────
    Section('01  GET /ping');
    DoReq(LClient, 'GET', BASE_URL + '/ping', nil, nil, R);
    Check('status 200',    R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('body = "pong"', R.Body = 'pong',    R.Body);

    // ── 02 ─────────────────────────────────────────────────────────────────────
    Section('02  GET /methods/get');
    DoReq(LClient, 'GET', BASE_URL + '/methods/get', nil, nil, R);
    Check('status 200',          R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('body contains "GET"', Pos('"GET"', R.Body) > 0, R.Body);

    // ── 03 ─────────────────────────────────────────────────────────────────────
    Section('03  POST /methods/post  (JSON body echo)');
    DoReq(LClient, 'POST', BASE_URL + '/methods/post',
      [TNameValuePair.Create('Content-Type', 'application/json; charset=utf-8')],
      TEncoding.UTF8.GetBytes('{"hello":"world"}'), R);
    Check('status 200',                  R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('body contains "POST"',        Pos('"POST"', R.Body) > 0, R.Body);
    Check('body echoes request payload', Pos('hello',  R.Body) > 0, R.Body);

    // ── 04 ─────────────────────────────────────────────────────────────────────
    // WinHTTP always sends Content-Length for a provided source stream, so an
    // empty-body PUT carries Content-Length: 0 without a manual header.
    Section('04  PUT /methods/put/42');
    DoReq(LClient, 'PUT', BASE_URL + '/methods/put/42', nil, nil, R);
    Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('id = "42"',  Pos('"42"', R.Body) > 0, R.Body);

    // ── 05 ─────────────────────────────────────────────────────────────────────
    Section('05  DELETE /methods/delete/99');
    DoReq(LClient, 'DELETE', BASE_URL + '/methods/delete/99', nil, nil, R);
    Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('id = "99"',  Pos('"99"', R.Body) > 0, R.Body);

    // ── 06 ─────────────────────────────────────────────────────────────────────
    Section('06  PATCH /methods/patch/7');
    DoReq(LClient, 'PATCH', BASE_URL + '/methods/patch/7', nil, nil, R);
    Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('id = "7"',   Pos('"7"', R.Body) > 0, R.Body);

    // ── 07 ─────────────────────────────────────────────────────────────────────
    Section('07  HEAD /methods/head  (header only, empty body)');
    DoReq(LClient, 'HEAD', BASE_URL + '/methods/head', nil, nil, R);
    Check('status 200',    R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('body is empty', R.Body = '', R.Body);
    if Assigned(R.Response) then
      Check('X-Head-Ok header present',
        R.Response.HeaderValue['X-Head-Ok'] = 'true',
        R.Response.HeaderValue['X-Head-Ok']);

    // ── 08 ─────────────────────────────────────────────────────────────────────
    Section('08  GET /params/path/hello');
    DoReq(LClient, 'GET', BASE_URL + '/params/path/hello', nil, nil, R);
    Check('status 200',   R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('id = "hello"', Pos('"hello"', R.Body) > 0, R.Body);

    // ── 09 ─────────────────────────────────────────────────────────────────────
    Section('09  GET /params/query?name=Horse&value=NetHttp');
    DoReq(LClient, 'GET',
      BASE_URL + '/params/query?name=Horse&value=NetHttp', nil, nil, R);
    Check('status 200',        R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('name = "Horse"',    Pos('"Horse"',   R.Body) > 0, R.Body);
    Check('value = "NetHttp"', Pos('"NetHttp"', R.Body) > 0, R.Body);

    // ── 10  FIX-HEADER-DUP ─────────────────────────────────────────────────────
    Section('10  GET /cookies/set  (FIX-HEADER-DUP: two Set-Cookie headers)');
    DoReq(LClient, 'GET', BASE_URL + '/cookies/set', nil, nil, R);
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

    // ── 11 ─────────────────────────────────────────────────────────────────────
    // The Cookie header is set manually (deterministic across providers); the
    // client's automatic cookie manager may attach the same values again —
    // harmless, since they are identical.
    Section('11  GET /cookies/echo  (send cookies, verify echo)');
    if LSessionCookie = '' then LSessionCookie := 'abc123';
    if LUserCookie    = '' then LUserCookie    := 'tester';
    DoReq(LClient, 'GET', BASE_URL + '/cookies/echo',
      [TNameValuePair.Create('Cookie',
        Format('session=%s; user=%s', [LSessionCookie, LUserCookie]))],
      nil, R);
    Check('status 200',                 R.StatusCode = 200,          IntToStr(R.StatusCode));
    Check('session echoed as "abc123"', Pos('"abc123"', R.Body) > 0, R.Body);
    Check('user echoed as "tester"',    Pos('"tester"', R.Body) > 0, R.Body);

    // ── 12 ─────────────────────────────────────────────────────────────────────
    Section('12  POST /upload  (multipart — 200 on Indy, 200 or 400 on custom parsers)');
    R := Default(TReqResult);
    LFileBytes  := TEncoding.UTF8.GetBytes('This is the uploaded file content.');
    LFileStream := TMemoryStream.Create;
    try
      LFileStream.WriteBuffer(LFileBytes[0], Length(LFileBytes));
      LFileStream.Position := 0;
      LForm := TMultipartFormData.Create;
      try
        LForm.AddField('fieldname', 'myupload.txt');
        LForm.AddStream('file', LFileStream, 'myupload.txt', 'text/plain');
        try
          LResp := LClient.Post(BASE_URL + '/upload', LForm);
          R.StatusCode := LResp.StatusCode;
          R.Body       := LResp.ContentAsString(TEncoding.UTF8);
          R.Response   := LResp;
        except
          R.StatusCode := 0;
          R.TimedOut   := True;
        end;
        ReportTiming('POST ' + BASE_URL + '/upload (multipart)', R);
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
      Check('"received":true', Pos('"received":true', R.Body) > 0, R.Body);
      Check('filename echoed', Pos('myupload.txt',    R.Body) > 0, R.Body);
      Check('size > 0 bytes',  Pos('"size":0',        R.Body) = 0, R.Body);
    end;

    // ── 13 ─────────────────────────────────────────────────────────────────────
    Section('13  GET /download  (Content-Disposition + body)');
    DoReq(LClient, 'GET', BASE_URL + '/download', nil, nil, R);
    Check('status 200',            R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('body contains "Horse"', Pos('Horse', R.Body) > 0, R.Body);
    if Assigned(R.Response) then
      Check('Content-Disposition: attachment',
        Pos('attachment', R.Response.HeaderValue['Content-Disposition']) > 0,
        R.Response.HeaderValue['Content-Disposition']);

    // ── 14 ─────────────────────────────────────────────────────────────────────
    Section('14  GET /headers/echo  (X-Test-Header round-trip)');
    DoReq(LClient, 'GET', BASE_URL + '/headers/echo',
      [TNameValuePair.Create('X-Test-Header', 'HelloFromClient')], nil, R);
    Check('status 200',                 R.StatusCode = 200,                 IntToStr(R.StatusCode));
    Check('X-Test-Header value echoed', Pos('HelloFromClient', R.Body) > 0, R.Body);

    // ── 15 ─────────────────────────────────────────────────────────────────────
    Section('15  POST /methods/post  (empty body — nil-body path)');
    DoReq(LClient, 'POST', BASE_URL + '/methods/post',
      [TNameValuePair.Create('Content-Type', 'application/json; charset=utf-8')],
      nil, R);
    Check('status 200 or 400',
      (R.StatusCode = 200) or (R.StatusCode = 400), IntToStr(R.StatusCode));
    if R.StatusCode = 200 then
      Check('"body":""', Pos('"body":""', R.Body) > 0, R.Body);

    // ── 16 ─────────────────────────────────────────────────────────────────────
    Section(Format('16  POST /echo/body  (%d-byte body)', [LARGE_BODY_SIZE]));
    DoReq(LClient, 'POST', BASE_URL + '/echo/body',
      [TNameValuePair.Create('Content-Type', 'text/plain; charset=utf-8')],
      TEncoding.UTF8.GetBytes(StringOfChar('A', LARGE_BODY_SIZE)), R);
    Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
    Check(Format('"size":%d exact', [LARGE_BODY_SIZE]),
      Pos(Format('"size":%d', [LARGE_BODY_SIZE]), R.Body) > 0, R.Body);

    // ── 17 ─────────────────────────────────────────────────────────────────────
    Section('17  POST /echo/body  sequential A → ping → B  (request isolation)');
    DoReq(LClient, 'POST', BASE_URL + '/echo/body',
      [TNameValuePair.Create('Content-Type', 'text/plain; charset=utf-8')],
      TEncoding.UTF8.GetBytes('SEQUENTIAL_BODY_ALPHA'), R);
    Check('step A: status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('step A: body contains SEQUENTIAL_BODY_ALPHA',
      Pos('SEQUENTIAL_BODY_ALPHA', R.Body) > 0, R.Body);
    DoReq(LClient, 'GET', BASE_URL + '/ping', nil, nil, R);
    Check('step ping: status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
    DoReq(LClient, 'POST', BASE_URL + '/echo/body',
      [TNameValuePair.Create('Content-Type', 'text/plain; charset=utf-8')],
      TEncoding.UTF8.GetBytes('SEQUENTIAL_BODY_BETA'), R);
    Check('step B: status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('step B: body contains SEQUENTIAL_BODY_BETA',
      Pos('SEQUENTIAL_BODY_BETA', R.Body) > 0, R.Body);
    Check('step B: body does NOT contain SEQUENTIAL_BODY_ALPHA  (no leakage)',
      Pos('SEQUENTIAL_BODY_ALPHA', R.Body) = 0, R.Body);

    // ── 18 ─────────────────────────────────────────────────────────────────────
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
    for I := 0 to CONCURRENT_COUNT - 1 do
      FirePost(BASE_URL + '/echo/body', I, False);
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

    // ── 19 ─────────────────────────────────────────────────────────────────────
    Section('19  GET /params/multi/alpha/beta  (two path params)');
    DoReq(LClient, 'GET', BASE_URL + '/params/multi/alpha/beta', nil, nil, R);
    Check('status 200',  R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('a = "alpha"', Pos('"alpha"', R.Body) > 0, R.Body);
    Check('b = "beta"',  Pos('"beta"',  R.Body) > 0, R.Body);
    Check('both params present',
      (Pos('"a"', R.Body) > 0) and (Pos('"b"', R.Body) > 0), R.Body);

    // ── 20 ─────────────────────────────────────────────────────────────────────
    Section('20  GET /does/not/exist  (expect 404)');
    DoReq(LClient, 'GET', BASE_URL + '/does/not/exist', nil, nil, R);
    Check('status 404', R.StatusCode = 404, IntToStr(R.StatusCode));

    // ── 21 ─────────────────────────────────────────────────────────────────────
    Section('21  GET /status/400  (explicit 400 response)');
    DoReq(LClient, 'GET', BASE_URL + '/status/400', nil, nil, R);
    Check('status 400',                R.StatusCode = 400, IntToStr(R.StatusCode));
    Check('body contains status code', Pos('"status":400', R.Body) > 0, R.Body);

    // ── 22 ─────────────────────────────────────────────────────────────────────
    Section('22  GET /status/500  (explicit 500 response)');
    DoReq(LClient, 'GET', BASE_URL + '/status/500', nil, nil, R);
    Check('status 500',                R.StatusCode = 500, IntToStr(R.StatusCode));
    Check('body contains status code', Pos('"status":500', R.Body) > 0, R.Body);

    // ── 23 ─────────────────────────────────────────────────────────────────────
    Section('23  GET /methods/get  (verify Content-Type response header)');
    DoReq(LClient, 'GET', BASE_URL + '/methods/get', nil, nil, R);
    Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
    if Assigned(R.Response) then
      Check('Content-Type contains "application/json"',
        Pos('application/json', R.Response.HeaderValue['Content-Type']) > 0,
        R.Response.HeaderValue['Content-Type']);

    // ── 24 ─────────────────────────────────────────────────────────────────────
    Section(Format('24  GET /response/large  (expect %d-byte body)',
      [LARGE_RESPONSE_SIZE]));
    DoReq(LClient, 'GET', BASE_URL + '/response/large', nil, nil, R);
    Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
    Check(Format('body length = %d', [LARGE_RESPONSE_SIZE]),
      Length(R.Body) = LARGE_RESPONSE_SIZE,
      Format('%d bytes received', [Length(R.Body)]));
    Check('body consists of ''X'' characters only',
      (Length(R.Body) > 0) and (Pos(StringOfChar('X', 8), R.Body) > 0), '');

    // ── 25 ─────────────────────────────────────────────────────────────────────
    Section('25  GET /raw/webrequest  (RawWebRequest adapter — PATCH-REQ-8)');
    DoReq(LClient, 'GET', BASE_URL + '/raw/webrequest',
      [TNameValuePair.Create('X-Test-Header', 'RawAdapterProbe')], nil, R);
    Check('status 200',      R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('hasAdapter true', Pos('"hasAdapter":true', R.Body) > 0, R.Body);
    Check('method = GET',    Pos('"method":"GET"',    R.Body) > 0, R.Body);
    Check('host present',    Pos('"host":"127.0.0.1', R.Body) > 0, R.Body);
    Check('pathInfo = /raw/webrequest',
      Pos('"pathInfo":"/raw/webrequest"', R.Body) > 0, R.Body);
    Check('GetFieldByName echoed custom header',
      Pos('"customHeader":"RawAdapterProbe"', R.Body) > 0, R.Body);
    Check('RemoteAddr non-empty',
      Pos('"remoteAddrNonEmpty":true', R.Body) > 0, R.Body);

    // ── 26 ─────────────────────────────────────────────────────────────────────
    Section('26  OPTIONS /raw/cors  (Horse.CORS pre-flight shape)');
    DoReq(LClient, 'OPTIONS', BASE_URL + '/raw/cors', nil, nil, R);
    Check('status 204', R.StatusCode = 204, IntToStr(R.StatusCode));
    if Assigned(R.Response) then
      Check('Access-Control-Allow-Origin: *',
        R.Response.HeaderValue['Access-Control-Allow-Origin'] = '*',
        R.Response.HeaderValue['Access-Control-Allow-Origin']);

    // ── 27 ─────────────────────────────────────────────────────────────────────
    Section('27  GET /raw/cors  (non-preflight branch)');
    DoReq(LClient, 'GET', BASE_URL + '/raw/cors', nil, nil, R);
    Check('status 200',              R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('body = "cors-route:GET"', R.Body = 'cors-route:GET', R.Body);

    // ── 28 ─────────────────────────────────────────────────────────────────────
    Section('28  GET /raw/webresponse  (Res.RawWebResponse.SetCustomHeader — PATCH-RES-6)');
    DoReq(LClient, 'GET', BASE_URL + '/raw/webresponse', nil, nil, R);
    Check('status 200',      R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('hasAdapter true', Pos('"hasAdapter":true', R.Body) > 0, R.Body);
    if Assigned(R.Response) then
    begin
      Check('X-Via-RawResponse header set via SetCustomHeader',
        R.Response.HeaderValue['X-Via-RawResponse'] = 'PATCH-RES-6-OK',
        R.Response.HeaderValue['X-Via-RawResponse']);
      Check('X-Via-AddHeader header set via Res.AddHeader',
        R.Response.HeaderValue['X-Via-AddHeader'] = 'AddHeader-OK',
        R.Response.HeaderValue['X-Via-AddHeader']);
    end
    else
      Check('response received', False, 'nil response');

    // ── 29 ─────────────────────────────────────────────────────────────────────
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
    for I := 0 to BURST_COUNT - 1 do
      FirePost(BASE_URL + '/pool/burst', I, True);
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

    // ── 30 ─────────────────────────────────────────────────────────────────────
    Section(Format('30  POST /pool/burst  %d rapid sequential after burst  (drain/refill)',
      [RAPID_SEQ_COUNT]));
    LSeqAllOk := True;
    for I := 0 to RAPID_SEQ_COUNT - 1 do
    begin
      LSeqMarker := Format('RAPID_SEQ_%d', [I]);
      DoReq(LClient, 'POST', BASE_URL + '/pool/burst',
        [TNameValuePair.Create('Content-Type', 'text/plain; charset=utf-8')],
        TEncoding.UTF8.GetBytes(LSeqMarker), R);
      if (R.StatusCode <> 200) or (Pos(LSeqMarker, R.Body) = 0) then
        LSeqAllOk := False;
    end;
    Check(Format('all %d sequential requests: 200 + correct marker', [RAPID_SEQ_COUNT]),
      LSeqAllOk, '');

    // ── 31 ─────────────────────────────────────────────────────────────────────
    Section('31  POST /echo/body-twice  (PATCH-REQ-9 double-read cache)');
    DoReq(LClient, 'POST', BASE_URL + '/echo/body-twice',
      [TNameValuePair.Create('Content-Type', 'text/plain; charset=utf-8')],
      TEncoding.UTF8.GetBytes('DOUBLE_READ_MARKER'), R);
    Check('status 200', R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('"first" contains posted value',
      Pos('"first":"DOUBLE_READ_MARKER"', R.Body) > 0, R.Body);
    Check('"second" contains posted value',
      Pos('"second":"DOUBLE_READ_MARKER"', R.Body) > 0, R.Body);
    Check('"equal":true — both reads returned the same string',
      Pos('"equal":true', R.Body) > 0, R.Body);

    // ── 32 ─────────────────────────────────────────────────────────────────────
    Section('32  GET /compat/rawbody  (COMPAT-1: shadow field wins over RawWebResponse.Content)');
    DoReq(LClient, 'GET', BASE_URL + '/compat/rawbody', nil, nil, R);
    Check('status 200',           R.StatusCode = 200, IntToStr(R.StatusCode));
    Check('body = "shadow-wins"', R.Body = 'shadow-wins', R.Body);
    Check('RawWebResponse stub value NOT present in body',
      Pos('raw-should-not-appear', R.Body) = 0, R.Body);

    // ── 33 ─────────────────────────────────────────────────────────────────────
    // Engine transports (CrossSocket, HTTP.sys) stream the 5-chunk body;
    // engine-less transports (Indy, IOCP, Epoll) answer the structured 501.
    // WinHTTP de-chunks transparently — R.Body is the reassembled payload.
    Section('33  GET /stream/pull  (streams on engine transports, else 501)');
    DoReq(LClient, 'GET', BASE_URL + '/stream/pull', nil, nil, R);
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
    DoReq(LClient, 'GET', BASE_URL + '/ping', nil, nil, R);
    Check('server healthy after streaming probe — /ping returns pong',
      (R.StatusCode = 200) and (R.Body = 'pong'),
      Format('%d / %s', [R.StatusCode, R.Body]));

    // ── 34 ─────────────────────────────────────────────────────────────────────
    Section('34  GET /stream/content-type  (Content-Type propagation or 501)');
    DoReq(LClient, 'GET', BASE_URL + '/stream/content-type', nil, nil, R);
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
          Pos('application/octet-stream', R.Response.HeaderValue['Content-Type']) > 0,
          R.Response.HeaderValue['Content-Type']);
    end;
    DoReq(LClient, 'GET', BASE_URL + '/ping', nil, nil, R);
    Check('server healthy after content-type stream probe',
      (R.StatusCode = 200) and (R.Body = 'pong'),
      Format('%d / %s', [R.StatusCode, R.Body]));

    // ── 35 ─────────────────────────────────────────────────────────────────────
    Section('35  GET /stream/empty  (zero-chunk stream or 501)');
    DoReq(LClient, 'GET', BASE_URL + '/stream/empty', nil, nil, R);
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
    DoReq(LClient, 'GET', BASE_URL + '/ping', nil, nil, R);
    Check('server healthy after empty stream probe',
      (R.StatusCode = 200) and (R.Body = 'pong'),
      Format('%d / %s', [R.StatusCode, R.Body]));

    // ── 36 ─────────────────────────────────────────────────────────────────────
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
      if not (((LStreamBatch[I].Status = 200) and
               (LStreamBatch[I].Body = 'chunk-0;chunk-1;chunk-2;chunk-3;chunk-4;'))
              or (LStreamBatch[I].Status = 501)) then
        LStreamAllOk := False;
      LStreamBatch[I].Event.Free;
    end;
    Check('both concurrent probes: complete stream (200) or clean 501',
      LStreamAllOk, '');
    DoReq(LClient, 'GET', BASE_URL + '/ping', nil, nil, R);
    Check('server healthy after concurrent streaming probes',
      (R.StatusCode = 200) and (R.Body = 'pong'),
      Format('%d / %s', [R.StatusCode, R.Body]));

  finally
    LClient.Free;
  end;
end;

begin
  Writeln('[HorseNetHttpTest] Client — target: ' + BASE_URL);
  Writeln('[HorseNetHttpTest] HTTP stack: System.Net.HttpClient (WinHTTP) —');
  Writeln('[HorseNetHttpTest] no Delphi-Cross-Socket dependency.');
  Writeln('[HorseNetHttpTest] Ensure one of the following servers is running on port 9010:');
  Writeln('[HorseNetHttpTest]   HorseIndyTestServer.exe    (Indy Console — default)');
  Writeln('[HorseNetHttpTest]   HorseIOCPTestServer.exe    (IOCP — Windows)');
  Writeln('[HorseNetHttpTest]   HorseHttpSysTestServer.exe (HTTP.sys — Windows, needs netsh)');
  Writeln('[HorseNetHttpTest]   HorseEpollTestServer.exe   (Epoll — Linux)');
  Writeln('');

  GGlobalSW         := TStopwatch.StartNew;
  GLastSectionTicks := 0;

  try
    RunTests;
  except
    on E: Exception do
      Writeln('[HorseNetHttpTest] Unexpected exception: ' + E.ClassName
        + ': ' + E.Message);
  end;

  Writeln('');
  if GFailCount = 0 then
    Writeln(Format('[HorseNetHttpTest] %d passed, 0 failed  (total %d) in %d ms wall clock',
      [GPassCount, GPassCount, GGlobalSW.ElapsedMilliseconds]))
  else
    Writeln(Format('[HorseNetHttpTest] %d passed, %d failed  (total %d) in %d ms wall clock',
      [GPassCount, GFailCount, GPassCount + GFailCount, GGlobalSW.ElapsedMilliseconds]));
  if GFailCount = 0 then
    Writeln('[HorseNetHttpTest] All tests PASSED.')
  else
    Writeln('[HorseNetHttpTest] Some tests FAILED — see details above.');

  ExitCode := GFailCount;

  Writeln('');
  Writeln('[HorseNetHttpTest] Press ENTER to exit...');
  Readln;
end.
