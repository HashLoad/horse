program IndyStreamCheck;

{ Streaming (SendStream) over the Indy provider.

  Runs the real Console provider with keep-alive on and reads the wire with a
  raw TCP client, so headers and chunk framing are checked byte for byte:

  - /sse    text/event-stream, three events 200 ms apart;
  - /partes text/plain, three parts;
  - /comum  an ordinary Send, after a streaming response on the same
            connection (keep-alive) and on its own.

  A streaming response must carry no Content-Length and a single
  "Transfer-Encoding: chunked", and its body must decode from the chunks.
  Before this fix the writer sent "Content-Length: 0", so the client
  ended the response at the header (and, chunked, received both headers).

  Exit code 0 and a "PASS:" line mean pass. }

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  IdGlobal, IdTCPClient,
  Horse, Horse.Response, Horse.Provider.Abstract;

const
  TestPort = 19264;
  EventCount = 3;
  EventDelayMs = 200;
  ReadTimeoutMs = 5000;
  CommonBody = 'comum-ok';

type
  THttpResponse = record
    StatusLine: string;
    Headers: TStringList;
    Body: string;
  end;

procedure Check(Condition: Boolean; const MessageText: string);
begin
  if not Condition then
    raise Exception.Create(MessageText);
end;

function HeaderCount(const AHeaders: TStringList; const AName: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AHeaders.Count - 1 do
    if SameText(Trim(AHeaders.Names[I]), AName) then
      Inc(Result);
end;

function HeaderValue(const AHeaders: TStringList; const AName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AHeaders.Count - 1 do
    if SameText(Trim(AHeaders.Names[I]), AName) then
      Exit(Trim(AHeaders.ValueFromIndex[I]));
end;

{ Sends a GET on an open connection and reads one full response: chunked when
  the response says so, by Content-Length otherwise. }
function Request(const AClient: TIdTCPClient; const APath: string): THttpResponse;
var
  Line: string;
  Size: Integer;
begin
  Result.Headers := TStringList.Create;
  Result.Headers.NameValueSeparator := ':';
  Result.Body := '';
  AClient.IOHandler.WriteLn('GET ' + APath + ' HTTP/1.1');
  AClient.IOHandler.WriteLn('Host: localhost');
  AClient.IOHandler.WriteLn('Connection: keep-alive');
  AClient.IOHandler.WriteLn('');
  Result.StatusLine := AClient.IOHandler.ReadLn;
  repeat
    Line := AClient.IOHandler.ReadLn;
    if Line <> '' then
      Result.Headers.Add(Line);
  until Line = '';

  if SameText(HeaderValue(Result.Headers, 'Transfer-Encoding'), 'chunked') then
  begin
    repeat
      Line := Trim(AClient.IOHandler.ReadLn);
      Size := StrToInt('$' + Line);
      if Size > 0 then
      begin
        Result.Body := Result.Body + AClient.IOHandler.ReadString(Size, IndyTextEncoding_UTF8);
        AClient.IOHandler.ReadLn;
      end;
    until Size = 0;
    AClient.IOHandler.ReadLn;
  end
  else if HeaderValue(Result.Headers, 'Content-Length') <> '' then
  begin
    Size := StrToInt(HeaderValue(Result.Headers, 'Content-Length'));
    if Size > 0 then
      Result.Body := AClient.IOHandler.ReadString(Size, IndyTextEncoding_UTF8);
  end;
end;

procedure CheckStreamingHeaders(const AResponse: THttpResponse; const APath: string);
begin
  Check(Pos('200', AResponse.StatusLine) > 0, APath + ': expected HTTP 200, got ' + AResponse.StatusLine);
  Check(HeaderCount(AResponse.Headers, 'Content-Length') = 0,
    APath + ': a streaming response must not carry Content-Length (got "' +
    HeaderValue(AResponse.Headers, 'Content-Length') + '")');
  Check(HeaderCount(AResponse.Headers, 'Transfer-Encoding') = 1,
    APath + ': expected one Transfer-Encoding header, got ' +
    IntToStr(HeaderCount(AResponse.Headers, 'Transfer-Encoding')));
  Check(SameText(HeaderValue(AResponse.Headers, 'Transfer-Encoding'), 'chunked'),
    APath + ': expected Transfer-Encoding: chunked');
end;

function ExpectedEvents: string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to EventCount do
    Result := Result + 'data: evento-' + IntToStr(I) + #10#10;
end;

procedure RegisterRoutes;
begin
  THorse.Get('/sse',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/event-stream');
      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        var
          I: Integer;
        begin
          for I := 1 to EventCount do
          begin
            AWriter.Write('data: evento-' + IntToStr(I) + #10#10);
            Sleep(EventDelayMs);
          end;
        end);
    end);
  THorse.Get('/partes',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/plain');
      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        var
          I: Integer;
        begin
          for I := 1 to EventCount do
            AWriter.Write('parte-' + IntToStr(I));
        end);
    end);
  THorse.Get('/comum',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(CommonBody);
    end);
end;

function NewClient: TIdTCPClient;
begin
  Result := TIdTCPClient.Create(nil);
  Result.Host := '127.0.0.1';
  Result.Port := TestPort;
  Result.ConnectTimeout := 3000;
  Result.ReadTimeout := ReadTimeoutMs;
  Result.Connect;
end;

procedure CheckAll;
var
  Client: TIdTCPClient;
  Response: THttpResponse;
begin
  // CA1, CA2 and CA4: SSE, then an ordinary request on the same connection.
  Client := NewClient;
  try
    Response := Request(Client, '/sse');
    try
      CheckStreamingHeaders(Response, '/sse');
      Check(SameText(HeaderValue(Response.Headers, 'Connection'), 'keep-alive'),
        '/sse: precondition, expected Connection: keep-alive (got "' +
        HeaderValue(Response.Headers, 'Connection') + '")');
      Check(Response.Body = ExpectedEvents, '/sse: body mismatch: "' + Response.Body + '"');
    finally
      Response.Headers.Free;
    end;

    Response := Request(Client, '/comum');
    try
      Check(Pos('200', Response.StatusLine) > 0, '/comum after /sse: expected HTTP 200, got ' + Response.StatusLine);
      Check(Response.Body = CommonBody, '/comum after /sse: body mismatch: "' + Response.Body + '"');
    finally
      Response.Headers.Free;
    end;
  finally
    Client.Free;
  end;

  // CA2 and CA3: chunked plain text.
  Client := NewClient;
  try
    Response := Request(Client, '/partes');
    try
      CheckStreamingHeaders(Response, '/partes');
      Check(Response.Body = 'parte-1parte-2parte-3', '/partes: body mismatch: "' + Response.Body + '"');
    finally
      Response.Headers.Free;
    end;
  finally
    Client.Free;
  end;

  // CA5: an ordinary Send keeps its Content-Length.
  Client := NewClient;
  try
    Response := Request(Client, '/comum');
    try
      Check(Pos('200', Response.StatusLine) > 0, '/comum: expected HTTP 200, got ' + Response.StatusLine);
      Check(HeaderValue(Response.Headers, 'Content-Length') = IntToStr(Length(CommonBody)),
        '/comum: expected Content-Length ' + IntToStr(Length(CommonBody)));
      Check(Response.Body = CommonBody, '/comum: body mismatch: "' + Response.Body + '"');
    finally
      Response.Headers.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure Run;
var
  Ready: TEvent;
  Server: TThread;
  ServerError: string;
begin
  Ready := nil;
  Server := nil;
  try
    ReportMemoryLeaksOnShutdown := True;
    Writeln('Indy ', gsIdVersion, ', compiler ', CompilerVersion:0:1);
    THorse.KeepConnectionAlive := True;
    RegisterRoutes;
    Ready := TEvent.Create(nil, True, False, '');
    Server := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          THorse.Listen(TestPort, '127.0.0.1',
            procedure
            begin
              Ready.SetEvent;
            end);
        except
          on E: Exception do
          begin
            ServerError := E.ClassName + ': ' + E.Message;
            Ready.SetEvent;
          end;
        end;
      end);
    Server.FreeOnTerminate := False;
    Server.Start;
    try
      Check(Ready.WaitFor(5000) = wrSignaled, 'Listener startup timed out');
      Check(ServerError = '', 'Listener failed: ' + ServerError);
      CheckAll;
    finally
      if THorse.IsRunning then
        THorse.StopListen;
      Server.WaitFor;
    end;
    Writeln('PASS: SSE and chunked streaming over Indy, keep-alive and ordinary Send');
  except
    on E: Exception do
    begin
      Writeln('FAIL: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  Server.Free;
  THorseProviderAbstract.OnListen := nil;
  THorseProviderAbstract.OnStopListen := nil;
  THorse.Routes := nil;
  Ready.Free;
end;

begin
  Run;
end.
