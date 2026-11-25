program IocpHttpCheck;

{ HTTP over the IOCP provider.

  Runs the real IOCP provider on 127.0.0.1 and talks raw HTTP over TCP, so
  headers, framing and connection state are checked as they go on the wire.
  The low-level sends of TIocpRawResponse are replaceable
  (TIocpRawResponse.SendFunction and GatherSendFunction); the checks swap them
  to force partial sends and failures, which a blocking loopback socket almost
  never produces on its own. THorseProviderIOCP.ConnectionCount shows whether a
  closed connection left the registry.

  CA1  partial send: with one byte per send, 1 MiB by Send(string), 1 MiB by
       stream and one 64 KiB SSE event arrive whole;
  CA2  failed send: the server closes the connection, the registry count comes
       back, the stream writer reports IsConnected = False, and the next
       request on a new connection is served;
  CA3  Connection: close: 200 requests leave the registry count where it was;
  CA4  exception after SendStream: the client gets the streamed response, no
       byte after it, and the connection is closed;
  CA5  length: an empty stream answers with Content-Length: 0 on a keep-alive
       connection that then serves the next request, and a 204 carries no
       Content-Length;
  CA6  load (a measure, not a negative control): 32 keep-alive clients, 50
       rounds each of 8 MiB by Send(string) and 8 MiB by stream.

  Every check prints PASS or FAIL. Exit code 0 and a final "PASS:" line mean
  every check passed. }

{$APPTYPE CONSOLE}
{ CA6 holds several 8 MiB buffers at once; a 32-bit build needs the larger
  address space. }
{$SETPEFLAGS $20}

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Diagnostics,
  Winapi.WinSock2,
  IdGlobal, IdTCPClient, IdException, IdExceptionCore,
  Horse, Horse.Response, Horse.Provider.IOCP;

const
  TestPort = 19265;
  ReadTimeoutMs = 5000;
  LimitedReadMs = 2000;
  CountWaitMs = 3000;
  ReadBlockSize = 64 * 1024;
  LargeSize = 1024 * 1024;
  SseEventSize = 64 * 1024;
  LoadSize = 8 * 1024 * 1024;
  LoadClients = 32;
  LoadRounds = 50;
  CloseRounds = 200;
  FailAfterBytes = 4096;
  FailSseEvents = 200;
  CommonBody = 'comum-ok';
  StreamedPart = 'parte-1';

type
  TSeamMode = (smOneByte, smFailAfter);

  THttpHead = record
    StatusLine: string;
    Headers: TStringList;
  end;

  TLoadClient = class(TThread)
  private
    FError: string;
  protected
    procedure Execute; override;
  public
    property Error: string read FError;
  end;

var
  LargeText: string;
  LargeBytes: TBytes;
  SseEvent: string;
  SseEventBytes: TBytes;
  LoadText: string;
  LoadBytes: TBytes;
  OriginalSend: TIocpSendFunction;
  OriginalGatherSend: TIocpGatherSendFunction;
  SeamMode: TSeamMode;
  SeamBytesLeft: Integer;
  SeamCalls: Integer;
  WriterDisconnected: TEvent;
  Failures: Integer;

procedure Check(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

function Pattern(ASize, ASeed: Integer): string;
var
  I: Integer;
begin
  SetLength(Result, ASize);
  for I := 1 to ASize do
    Result[I] := Char(Ord('a') + (I + ASeed) mod 26);
end;

{ Seam }

function SeamSend(ASocket: TSocket; const ABuffer; ALength: Integer): Integer;
begin
  TInterlocked.Increment(SeamCalls);
  if SeamMode = smOneByte then
    Exit(OriginalSend(ASocket, ABuffer, 1));
  if SeamBytesLeft <= 0 then
    Exit(SOCKET_ERROR);
  if ALength > SeamBytesLeft then
    ALength := SeamBytesLeft;
  Result := OriginalSend(ASocket, ABuffer, ALength);
  if Result > 0 then
    Dec(SeamBytesLeft, Result);
end;

{ One call carries only the first non-empty buffer, through SeamSend, as a
  partial WSASend would. }
function SeamGatherSend(ASocket: TSocket; const AHeaderBytes, ABodyBytes: TBytes): Integer;
begin
  if Length(AHeaderBytes) > 0 then
    Result := SeamSend(ASocket, AHeaderBytes[0], Length(AHeaderBytes))
  else if Length(ABodyBytes) > 0 then
    Result := SeamSend(ASocket, ABodyBytes[0], Length(ABodyBytes))
  else
    Result := 0;
end;

procedure InstallSeam(AMode: TSeamMode);
begin
  SeamMode := AMode;
  SeamBytesLeft := FailAfterBytes;
  SeamCalls := 0;
  TIocpRawResponse.SendFunction := SeamSend;
  TIocpRawResponse.GatherSendFunction := SeamGatherSend;
end;

procedure RestoreSeam;
begin
  TIocpRawResponse.SendFunction := OriginalSend;
  TIocpRawResponse.GatherSendFunction := OriginalGatherSend;
end;

{ Registry }

{ The count includes the pending accepts, so a check compares it with the
  value read while the server was idle. }
function IdleConnectionCount: Integer;
var
  LPrevious: Integer;
  LDeadline: TStopwatch;
begin
  LDeadline := TStopwatch.StartNew;
  Result := THorseProviderIOCP.ConnectionCount;
  repeat
    Sleep(300);
    LPrevious := Result;
    Result := THorseProviderIOCP.ConnectionCount;
  until (Result = LPrevious) or (LDeadline.ElapsedMilliseconds > CountWaitMs);
end;

procedure CheckCountReturns(ABaseline: Integer; const AContext: string);
var
  LDeadline: TStopwatch;
  LCount: Integer;
begin
  LDeadline := TStopwatch.StartNew;
  repeat
    LCount := THorseProviderIOCP.ConnectionCount;
    if LCount = ABaseline then
      Exit;
    Sleep(20);
  until LDeadline.ElapsedMilliseconds > CountWaitMs;
  raise Exception.CreateFmt('%s: the registry holds %d connections, expected %d',
    [AContext, LCount, ABaseline]);
end;

{ Client }

function NewClient: TIdTCPClient;
begin
  Result := TIdTCPClient.Create(nil);
  Result.Host := '127.0.0.1';
  Result.Port := TestPort;
  Result.ConnectTimeout := 3000;
  Result.ReadTimeout := ReadTimeoutMs;
  Result.Connect;
end;

procedure SendRequest(const AClient: TIdTCPClient; const APath: string; AKeepAlive: Boolean = True);
begin
  AClient.IOHandler.WriteLn('GET ' + APath + ' HTTP/1.1');
  AClient.IOHandler.WriteLn('Host: localhost');
  if AKeepAlive then
    AClient.IOHandler.WriteLn('Connection: keep-alive')
  else
    AClient.IOHandler.WriteLn('Connection: close');
  AClient.IOHandler.WriteLn('');
end;

function ReadHead(const AClient: TIdTCPClient): THttpHead;
var
  LLine: string;
begin
  Result.Headers := TStringList.Create;
  Result.Headers.NameValueSeparator := ':';
  Result.StatusLine := AClient.IOHandler.ReadLn;
  repeat
    LLine := AClient.IOHandler.ReadLn;
    if LLine <> '' then
      Result.Headers.Add(LLine);
  until LLine = '';
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

{ Reads ACount bytes in blocks, so Indy never buffers a whole large body. }
function ReadExact(const AClient: TIdTCPClient; ACount: Integer): TBytes;
var
  LBlock: TIdBytes;
  LOffset, LSize: Integer;
begin
  SetLength(Result, ACount);
  LOffset := 0;
  while LOffset < ACount do
  begin
    LSize := ACount - LOffset;
    if LSize > ReadBlockSize then
      LSize := ReadBlockSize;
    SetLength(LBlock, 0);
    AClient.IOHandler.ReadBytes(LBlock, LSize, False);
    Move(LBlock[0], Result[LOffset], LSize);
    Inc(LOffset, LSize);
  end;
end;

function ReadChunked(const AClient: TIdTCPClient): TBytes;
var
  LSize, LOffset: Integer;
  LChunk: TBytes;
begin
  SetLength(Result, 0);
  repeat
    LSize := StrToInt('$' + Trim(AClient.IOHandler.ReadLn));
    if LSize > 0 then
    begin
      LChunk := ReadExact(AClient, LSize);
      LOffset := Length(Result);
      SetLength(Result, LOffset + LSize);
      Move(LChunk[0], Result[LOffset], LSize);
      AClient.IOHandler.ReadLn;
    end;
  until LSize = 0;
  AClient.IOHandler.ReadLn;
end;

{ Reads chunks until ACount body bytes arrived, without waiting for the last
  chunk: a stream that is still open has not sent it. }
function ReadChunkedPrefix(const AClient: TIdTCPClient; ACount: Integer): TBytes;
var
  LSize, LOffset: Integer;
  LChunk: TBytes;
begin
  SetLength(Result, 0);
  while Length(Result) < ACount do
  begin
    LSize := StrToInt('$' + Trim(AClient.IOHandler.ReadLn));
    Check(LSize > 0, 'the stream ended before the expected bytes arrived');
    LChunk := ReadExact(AClient, LSize);
    LOffset := Length(Result);
    SetLength(Result, LOffset + LSize);
    Move(LChunk[0], Result[LOffset], LSize);
    AClient.IOHandler.ReadLn;
  end;
end;

{ Reads a body delimited by Content-Length or by chunks. }
function ReadBody(const AClient: TIdTCPClient; const AHead: THttpHead; const APath: string): TBytes;
var
  LLength: string;
begin
  if SameText(HeaderValue(AHead.Headers, 'Transfer-Encoding'), 'chunked') then
    Exit(ReadChunked(AClient));
  LLength := HeaderValue(AHead.Headers, 'Content-Length');
  Check(LLength <> '', APath + ': the response is not delimited (no Content-Length, not chunked)');
  Result := ReadExact(AClient, StrToInt(LLength));
end;

procedure CheckBytes(const AActual, AExpected: TBytes; const AContext: string);
begin
  Check(Length(AActual) = Length(AExpected), Format('%s: received %d bytes, expected %d',
    [AContext, Length(AActual), Length(AExpected)]));
  Check((Length(AExpected) = 0) or CompareMem(@AActual[0], @AExpected[0], Length(AExpected)),
    AContext + ': the body differs from the one sent');
end;

{ Passes when the server closes the connection within ALimitMs and sends
  nothing more. }
procedure CheckClosedByServer(const AClient: TIdTCPClient; ALimitMs: Integer; const AContext: string);
var
  LByte: Byte;
begin
  AClient.ReadTimeout := ALimitMs;
  try
    LByte := AClient.IOHandler.ReadByte;
  except
    on E: EIdReadTimeout do
      raise Exception.Create(AContext + ': the server kept the connection open');
    on E: Exception do
      Exit;
  end;
  raise Exception.CreateFmt('%s: byte %d arrived after the response', [AContext, LByte]);
end;

{ Passes when the server closes the connection within ALimitMs; whatever it
  sent before closing is discarded. }
procedure CheckDrainedToClose(const AClient: TIdTCPClient; ALimitMs: Integer; const AContext: string);
begin
  AClient.ReadTimeout := ALimitMs;
  try
    repeat
      AClient.IOHandler.ReadByte;
    until False;
  except
    on E: EIdReadTimeout do
      raise Exception.Create(AContext + ': the server kept the connection open');
    on E: Exception do
      ;
  end;
end;

{ Requests APath and checks the status and the body. }
procedure GetAndCheck(const AClient: TIdTCPClient; const APath: string; const AExpected: TBytes);
var
  LHead: THttpHead;
begin
  SendRequest(AClient, APath);
  LHead := ReadHead(AClient);
  try
    Check(Pos(' 200 ', LHead.StatusLine + ' ') > 0, APath + ': expected HTTP 200, got ' + LHead.StatusLine);
    CheckBytes(ReadBody(AClient, LHead, APath), AExpected, APath);
  finally
    LHead.Headers.Free;
  end;
end;

{ Routes }

procedure RegisterRoutes;
begin
  THorse.Get('/comum',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(CommonBody);
    end);
  THorse.Get('/grande/texto',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(LargeText);
    end);
  THorse.Get('/grande/stream',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(TStream(TBytesStream.Create(LargeBytes)));
    end);
  THorse.Get('/grande/sse',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/event-stream');
      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        begin
          AWriter.Write(SseEvent);
        end);
    end);
  THorse.Get('/falha/sse',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/event-stream');
      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        var
          I: Integer;
        begin
          for I := 1 to FailSseEvents do
          begin
            AWriter.Write('data: ' + Pattern(1024, I) + #10#10);
            if not AWriter.IsConnected then
            begin
              WriterDisconnected.SetEvent;
              Exit;
            end;
          end;
        end);
    end);
  THorse.Get('/stream-e-erro',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/plain');
      Res.SendStream(
        procedure(const AWriter: IHorseStreamWriter)
        begin
          AWriter.Write(StreamedPart);
        end);
      raise Exception.Create('falha depois do stream');
    end);
  THorse.Get('/vazio',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(TStream(TMemoryStream.Create));
    end);
  THorse.Get('/sem-conteudo',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Status(204);
    end);
  THorse.Get('/carga/texto',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(LoadText);
    end);
  THorse.Get('/carga/stream',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(TStream(TBytesStream.Create(LoadBytes)));
    end);
end;

{ Checks }

procedure CheckPartialSend;
var
  LClient: TIdTCPClient;
  LHead: THttpHead;
begin
  InstallSeam(smOneByte);
  try
    LClient := NewClient;
    try
      GetAndCheck(LClient, '/grande/texto', LargeBytes);
      GetAndCheck(LClient, '/grande/stream', LargeBytes);
    finally
      LClient.Free;
    end;

    LClient := NewClient;
    try
      SendRequest(LClient, '/grande/sse');
      LHead := ReadHead(LClient);
      try
        Check(Pos(' 200 ', LHead.StatusLine + ' ') > 0, '/grande/sse: expected HTTP 200, got ' + LHead.StatusLine);
        { The stream writer reads the adapter's content type, which is empty on
          IOCP, so the event may arrive chunked. }
        if SameText(HeaderValue(LHead.Headers, 'Transfer-Encoding'), 'chunked') then
          CheckBytes(ReadChunkedPrefix(LClient, Length(SseEventBytes)), SseEventBytes, '/grande/sse')
        else
          CheckBytes(ReadExact(LClient, Length(SseEventBytes)), SseEventBytes, '/grande/sse');
      finally
        LHead.Headers.Free;
      end;
    finally
      LClient.Free;
    end;

    { Precondition: the responses really went out one byte per send. }
    Check(SeamCalls > 2 * LargeSize, Format('precondition: only %d sends went through the seam', [SeamCalls]));
  finally
    RestoreSeam;
  end;
end;

procedure CheckFailedSend;
var
  LBaseline: Integer;
  LClient: TIdTCPClient;
  LHead: THttpHead;
begin
  LBaseline := IdleConnectionCount;

  InstallSeam(smFailAfter);
  try
    LClient := NewClient;
    try
      SendRequest(LClient, '/grande/texto');
      try
        LHead := ReadHead(LClient);
        try
          ReadBody(LClient, LHead, '/grande/texto');
        finally
          LHead.Headers.Free;
        end;
        raise Exception.Create('/grande/texto: the whole body arrived through a failing send');
      except
        on E: EIdReadTimeout do
          raise Exception.Create('/grande/texto: the server kept the connection open after the failed send');
        on E: EIdException do
          ; { closed by the server, as expected }
      end;
      CheckCountReturns(LBaseline, '/grande/texto');
    finally
      LClient.Free;
    end;

    WriterDisconnected.ResetEvent;
    SeamBytesLeft := FailAfterBytes;
    LClient := NewClient;
    try
      SendRequest(LClient, '/falha/sse');
      Check(WriterDisconnected.WaitFor(CountWaitMs) = wrSignaled,
        '/falha/sse: the stream writer kept reporting IsConnected after the failed send');
      CheckDrainedToClose(LClient, LimitedReadMs, '/falha/sse');
      CheckCountReturns(LBaseline, '/falha/sse');
    finally
      LClient.Free;
    end;
  finally
    RestoreSeam;
  end;

  LClient := NewClient;
  try
    GetAndCheck(LClient, '/comum', TEncoding.UTF8.GetBytes(CommonBody));
  finally
    LClient.Free;
  end;
end;

procedure CheckConnectionClose;
var
  LBaseline: Integer;
  I: Integer;
  LClient: TIdTCPClient;
  LHead: THttpHead;
begin
  LBaseline := IdleConnectionCount;
  for I := 1 to CloseRounds do
  begin
    LClient := NewClient;
    try
      SendRequest(LClient, '/comum', False);
      LHead := ReadHead(LClient);
      try
        Check(SameText(HeaderValue(LHead.Headers, 'Connection'), 'close'),
          'precondition: expected Connection: close, got "' + HeaderValue(LHead.Headers, 'Connection') + '"');
        CheckBytes(ReadBody(LClient, LHead, '/comum'), TEncoding.UTF8.GetBytes(CommonBody), '/comum');
      finally
        LHead.Headers.Free;
      end;
      CheckClosedByServer(LClient, LimitedReadMs, '/comum with Connection: close');
    finally
      LClient.Free;
    end;
  end;
  CheckCountReturns(LBaseline, Format('%d requests with Connection: close', [CloseRounds]));
end;

procedure CheckExceptionAfterHeaders;
var
  LClient: TIdTCPClient;
  LHead: THttpHead;
begin
  LClient := NewClient;
  try
    SendRequest(LClient, '/stream-e-erro');
    LHead := ReadHead(LClient);
    try
      Check(Pos(' 200 ', LHead.StatusLine + ' ') > 0, '/stream-e-erro: expected HTTP 200, got ' + LHead.StatusLine);
      CheckBytes(ReadBody(LClient, LHead, '/stream-e-erro'), TEncoding.UTF8.GetBytes(StreamedPart), '/stream-e-erro');
    finally
      LHead.Headers.Free;
    end;
    CheckClosedByServer(LClient, LimitedReadMs, '/stream-e-erro');
  finally
    LClient.Free;
  end;
end;

procedure CheckLength;
var
  LClient: TIdTCPClient;
  LHead: THttpHead;
begin
  LClient := NewClient;
  try
    LClient.ReadTimeout := LimitedReadMs;
    SendRequest(LClient, '/vazio');
    LHead := ReadHead(LClient);
    try
      Check(Pos(' 200 ', LHead.StatusLine + ' ') > 0, '/vazio: expected HTTP 200, got ' + LHead.StatusLine);
      Check(SameText(HeaderValue(LHead.Headers, 'Connection'), 'keep-alive'),
        'precondition: expected Connection: keep-alive, got "' + HeaderValue(LHead.Headers, 'Connection') + '"');
      Check(HeaderValue(LHead.Headers, 'Content-Length') = '0',
        '/vazio: expected Content-Length: 0, got "' + HeaderValue(LHead.Headers, 'Content-Length') + '"');
    finally
      LHead.Headers.Free;
    end;
    GetAndCheck(LClient, '/comum', TEncoding.UTF8.GetBytes(CommonBody));

    SendRequest(LClient, '/sem-conteudo');
    LHead := ReadHead(LClient);
    try
      Check(Pos(' 204 ', LHead.StatusLine + ' ') > 0, '/sem-conteudo: expected HTTP 204, got ' + LHead.StatusLine);
      Check(HeaderCount(LHead.Headers, 'Content-Length') = 0,
        '/sem-conteudo: a 204 must not carry Content-Length (got "' +
        HeaderValue(LHead.Headers, 'Content-Length') + '")');
    finally
      LHead.Headers.Free;
    end;
    GetAndCheck(LClient, '/comum', TEncoding.UTF8.GetBytes(CommonBody));
  finally
    LClient.Free;
  end;
end;

procedure TLoadClient.Execute;
var
  LClient: TIdTCPClient;
  I: Integer;
begin
  try
    LClient := NewClient;
    try
      for I := 1 to LoadRounds do
      begin
        GetAndCheck(LClient, '/carga/texto', LoadBytes);
        GetAndCheck(LClient, '/carga/stream', LoadBytes);
      end;
    finally
      LClient.Free;
    end;
  except
    on E: Exception do
      FError := E.ClassName + ': ' + E.Message;
  end;
end;

procedure CheckLoad;
var
  LClients: array[1..LoadClients] of TLoadClient;
  I: Integer;
  LWatch: TStopwatch;
  LErrors: string;
begin
  LWatch := TStopwatch.StartNew;
  for I := Low(LClients) to High(LClients) do
    LClients[I] := TLoadClient.Create(False);
  LErrors := '';
  for I := Low(LClients) to High(LClients) do
  begin
    LClients[I].WaitFor;
    if LClients[I].Error <> '' then
      LErrors := LErrors + Format(#13#10'  client %d: %s', [I, LClients[I].Error]);
    LClients[I].Free;
  end;
  Writeln(Format('  load: %d clients x %d rounds x 2 responses of %d bytes in %d ms',
    [LoadClients, LoadRounds, LoadSize, LWatch.ElapsedMilliseconds]));
  Check(LErrors = '', 'load failures:' + LErrors);
end;

procedure RunCheck(const AName: string; ACheck: TProc);
begin
  try
    ACheck;
    Writeln('PASS ', AName);
  except
    on E: Exception do
    begin
      Writeln('FAIL ', AName, ': ', E.ClassName, ': ', E.Message);
      Inc(Failures);
    end;
  end;
end;

procedure WaitForListener;
var
  LDeadline: TStopwatch;
  LClient: TIdTCPClient;
begin
  LDeadline := TStopwatch.StartNew;
  repeat
    try
      LClient := NewClient;
      LClient.Free;
      Exit;
    except
      on E: Exception do
        Sleep(50);
    end;
  until LDeadline.ElapsedMilliseconds > 5000;
  raise Exception.Create('the IOCP listener did not come up on port ' + IntToStr(TestPort));
end;

procedure Run;
var
  LServer: TThread;
  LServerError: string;
begin
  LServer := nil;
  try
    Writeln('IOCP HTTP check, compiler ', CompilerVersion:0:1);
    LargeText := Pattern(LargeSize, 1);
    LargeBytes := TEncoding.UTF8.GetBytes(LargeText);
    SseEvent := 'data: ' + Pattern(SseEventSize, 2) + #10#10;
    SseEventBytes := TEncoding.UTF8.GetBytes(SseEvent);
    LoadText := Pattern(LoadSize, 3);
    LoadBytes := TEncoding.UTF8.GetBytes(LoadText);
    OriginalSend := TIocpRawResponse.SendFunction;
    OriginalGatherSend := TIocpRawResponse.GatherSendFunction;
    WriterDisconnected := TEvent.Create(nil, True, False, '');
    RegisterRoutes;

    LServer := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          THorseProviderIOCP.Listen(TestPort, '127.0.0.1');
        except
          on E: Exception do
            LServerError := E.ClassName + ': ' + E.Message;
        end;
      end);
    LServer.FreeOnTerminate := False;
    LServer.Start;
    try
      WaitForListener;
      Check(LServerError = '', 'listener failed: ' + LServerError);

      RunCheck('CA1 partial send', CheckPartialSend);
      RunCheck('CA2 failed send', CheckFailedSend);
      RunCheck('CA3 Connection: close', CheckConnectionClose);
      RunCheck('CA4 exception after headers', CheckExceptionAfterHeaders);
      RunCheck('CA5 length', CheckLength);
      RunCheck('CA6 load', CheckLoad);
    finally
      if THorseProviderIOCP.IsRunning then
        THorseProviderIOCP.StopListen;
      LServer.WaitFor;
    end;

    Check(Failures = 0, Format('%d check(s) failed', [Failures]));
    Writeln('PASS: IOCP sends whole responses and closes through the registry');
  except
    on E: Exception do
    begin
      Writeln('FAIL: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  LServer.Free;
  WriterDisconnected.Free;
end;

begin
  Run;
end.
