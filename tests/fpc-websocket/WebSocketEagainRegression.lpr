program WebSocketEagainRegression;

{ ============================================================================
  FIX-WS-NONBLOCK / FIX-WS-EINTR — regression test on the FPC + epoll path.

  Requested in review of HashLoad/horse PR #549. Horse's own suite is DUnitX
  and Delphi-only (no .lpr, and Tests.Integration.WebSocket.pas carries no FPC
  guards), so the FPC+epoll path cannot be covered there. This is a standalone
  program instead: it starts a real epoll-backed Horse server, drives it with a
  raw client socket, and asserts the four behaviours named in the review.

  THE DEFECT under test
  ---------------------
  epoll sets O_NONBLOCK on every accepted fd (Horse.Provider.Epoll.pas). On a
  non-blocking socket recv returns -1/EAGAIN to mean "nothing yet" — the normal
  state of an idle WebSocket peer. The transport's read loop treated every
  non-positive result as a disconnect, so it broke on its FIRST iteration,
  roughly a millisecond after the 101, before any frame could arrive. The
  handler returned, the HTTP pipeline resumed, and a stray "HTTP/1.1 200 OK"
  was written onto a socket already handed to WebSocket.

  WHAT EACH CHECK PROVES
  ----------------------
    1  first EAGAIN does not close     immediately after the 101 the server is
                                       SILENT. Pre-fix the loop broke here, the
                                       handler returned, and the HTTP pipeline
                                       wrote a stray 200 onto the socket.
    2  an idle connection stays open    still silent after >4 read ticks (250 ms
                                       each) — proves the loop parks in select()
                                       rather than treating a timeout as a
                                       disconnect.
    3  a later frame reaches OnMessage  a frame sent AFTER that idle period is
                                       echoed — the connection was not merely
                                       intact but still functional.
    4  peer close releases the loop     after an abrupt client close the server
                                       still serves HTTP on the same port,
                                       proving the read loop exited instead of
                                       spinning or hanging.

  WHY CHECKS 1 AND 2 ASSERT SILENCE, NOT "STILL OPEN"
  ---------------------------------------------------
  This was got wrong once and the control run caught it. The first version of
  this test asked "is the socket still open?" — and PASSED against the broken
  transport, because the defect does not close the socket. It hands the socket
  back to the HTTP pipeline, which writes an unsolicited "HTTP/1.1 200 OK".
  The connection is open; it is just talking the wrong protocol.

  A correct server sends nothing at all between the 101 and the client's first
  frame, so silence is the property that actually separates the two builds.

  Verified against upstream/master (pre-fix) with this exact program:
  checks 1 and 2 report UNSOLICITED BYTES, check 3 fails with ~102 bytes of
  HTTP where an echo was expected.

  BUILD AND RUN (Linux, FPC 3.2.2 or trunk)
  -----------------------------------------
    mkdir -p /tmp/ws-out
    fpc -MDelphi -B -dHORSE_PROVIDER_EPOLL \
        -Fu<horse>/src -Fi<horse>/src \
        -FU/tmp/ws-out -FE/tmp/ws-out \
        WebSocketEagainRegression.lpr
    /tmp/ws-out/WebSocketEagainRegression

  -Fi is required as well as -Fu: Horse.Core.Param.Header.pas includes
  Horse.FPC.inc, and an include path is not implied by the unit path.

  -B forces a full rebuild. A cached .ppu of the transport would silently test
  the previous version of the very unit under test.

  TO RUN THE CONTROL (confirm the test still discriminates)
  --------------------------------------------------------
  Point -Fu at a copy of src whose Horse.Provider.Socket.WebSocket.pas is the
  pre-fix version; checks 1-3 must fail. A test that has never been observed
  failing has not earned its place -- and the first draft of this one passed
  4 of 5 against the broken transport.

  Exit code = number of failed checks, so it can gate CI.
  ============================================================================ }

{$MODE DELPHI}{$H+}
{$DEFINE HORSE_PROVIDER_EPOLL}

uses
  {$IFDEF UNIX}
  cthreads,   { must precede every other unit on FPC/Unix — installs the
                pthreads driver before anything can touch TThread }
  {$ENDIF}
  SysUtils,
  Classes,
  Sockets,
  BaseUnix,
  Horse,
  Horse.Core.WebSocket;

const
  TEST_PORT      = 9114;
  READ_TICK_MS   = 250;   { WS_SOCKET_READ_TICK_MS in the transport }
  IDLE_WAIT_MS   = 1200;  { > 4 ticks, so several EAGAIN→select cycles elapse }
  SETTLE_MS      = 120;   { after the 101, before probing liveness }

var
  GPass: Integer = 0;
  GFail: Integer = 0;

procedure Check(const AName: string; const APassed: Boolean; const ADetail: string = '');
begin
  if APassed then
  begin
    Inc(GPass);
    WriteLn('  PASS  ', AName);
  end
  else
  begin
    Inc(GFail);
    if ADetail <> '' then
      WriteLn('  FAIL  ', AName, '  [', ADetail, ']')
    else
      WriteLn('  FAIL  ', AName);
  end;
end;

{ ── Server ─────────────────────────────────────────────────────────────────
  PLAIN unit-scope procedures — not anonymous procedures and not class
  procedures.

  FPC without FUNCTIONREFERENCES cannot bind a closure to THorseCallback, and
  a class procedure does not satisfy it either: THorseCallback and
  IHorseWebSocketConnection.OnMessage are plain procedure variable types, not
  "of object" method pointers. FPC rejects a class procedure with

    Incompatible type for arg no. 1: Got "TWs.class OnMessage(...)",
    expected "<procedure variable type of procedure(...);Register>"

  which is the compiler distinguishing a method pointer (code + Self) from a
  bare code pointer. Plain procedures are the only form that binds on FPC. }

procedure WsOnMessage(const AConnection: IHorseWebSocketConnection;
  const AText: string);
begin
  { try/except per the WebSocket skill: an exception escaping here would kill
    the worker socket thread. }
  try
    AConnection.SendText('echo:' + AText);
  except
    // swallow — a failed echo surfaces as a failed check, not a dead thread
  end;
end;

procedure WsOnConnect(const AConn: IHorseWebSocketConnection);
begin
  AConn.OnMessage := WsOnMessage;
end;

procedure RouteWS(Req: THorseRequest; Res: THorseResponse);
begin
  Res.UpgradeToWebSocket(WsOnConnect);
end;

procedure RoutePing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

type
  TServerThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TServerThread.Execute;
begin
  THorse.Listen(TEST_PORT);
end;

{ ── Raw client helpers ─────────────────────────────────────────────────────
  Deliberately no HTTP/WebSocket client library: the defect lives in how the
  server reacts to silence, so the test must control exactly when bytes are
  sent — including sending none at all. }

function ConnectClient: LongInt;
var
  LAddr: TInetSockAddr;
begin
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then Exit;
  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port   := htons(TEST_PORT);
  LAddr.sin_addr   := StrToNetAddr('127.0.0.1');
  if fpConnect(Result, @LAddr, SizeOf(LAddr)) <> 0 then
  begin
    fpClose(Result);
    Result := -1;
  end;
end;

procedure SendStr(ASock: LongInt; const AText: string);
begin
  if Length(AText) > 0 then
    fpSend(ASock, @AText[1], Length(AText), 0);
end;

{ Waits up to ATimeoutMS for readability. Returns True when the socket became
  readable, False on timeout. }
function WaitReadable(ASock: LongInt; ATimeoutMS: Integer): Boolean;
var
  LSet: TFDSet;
  LTv:  TTimeVal;
begin
  fpFD_ZERO(LSet);
  fpFD_SET(ASock, LSet);
  LTv.tv_sec  := ATimeoutMS div 1000;
  LTv.tv_usec := (ATimeoutMS mod 1000) * 1000;
  Result := fpSelect(ASock + 1, @LSet, nil, nil, @LTv) > 0;
end;

function ReadAvailable(ASock: LongInt; ATimeoutMS: Integer): string;
var
  LBuf: array[0..4095] of Byte;
  LN:   LongInt;
begin
  Result := '';
  if not WaitReadable(ASock, ATimeoutMS) then Exit;
  LN := fpRecv(ASock, @LBuf[0], SizeOf(LBuf), 0);
  if LN > 0 then
  begin
    SetLength(Result, LN);
    Move(LBuf[0], Result[1], LN);
  end;
end;

{ Post-upgrade quiet probe. Does not consume data (MSG_PEEK), so a later check
  can still read whatever arrives.

  Returns:  0 silent and open  — CORRECT: the server is parked in select()
            1 peer closed
            2 unsolicited bytes — THE DEFECT
           -1 error

  Why "silent" and not "open" is the assertion
  --------------------------------------------
  The obvious probe — "is the socket still open?" — does NOT detect this bug,
  and an earlier version of this test wrongly passed against the broken
  transport because of it.

  When the read loop breaks on its first EAGAIN, the upgrade handler returns
  and the ordinary HTTP pipeline resumes on a socket that has already been
  handed to WebSocket. It writes a stray "HTTP/1.1 200 OK ..." response. The
  socket is therefore NOT closed — it is open with unexpected HTTP bytes in it,
  so any "still open" check reports success against the very build it is meant
  to catch.

  A correct server sends NOTHING between the 101 and the client's first frame.
  Anything arriving in that window is the pipeline resuming, which is precisely
  the failure. Hence: assert silence. }
function ProbeQuiet(ASock: LongInt; ATimeoutMS: Integer; out AData: string): Integer;
var
  LBuf: array[0..1023] of Byte;
  LN:   LongInt;
begin
  AData := '';
  if not WaitReadable(ASock, ATimeoutMS) then
    Exit(0);                       { nothing readable → silent, as it should be }
  LN := fpRecv(ASock, @LBuf[0], SizeOf(LBuf), MSG_PEEK);
  if LN = 0 then
    Result := 1                    { orderly shutdown }
  else if LN > 0 then
  begin
    SetLength(AData, LN);
    Move(LBuf[0], AData[1], LN);
    Result := 2;                   { unsolicited — the stray HTTP response }
  end
  else if (fpgeterrno = ESysEAGAIN) or (fpgeterrno = ESysEWOULDBLOCK) then
    Result := 0
  else
    Result := -1;
end;

function ProbeName(AStatus: Integer): string;
begin
  case AStatus of
    0:  Result := 'silent+open';
    1:  Result := 'peer closed';
    2:  Result := 'UNSOLICITED BYTES';
  else  Result := 'error';
  end;
end;

{ A masked single-frame text message. Clients MUST mask (RFC 6455 §5.3). }
function BuildMaskedTextFrame(const AText: string): TBytes;
var
  I, LLen: Integer;
  LMask: array[0..3] of Byte;
begin
  { Initialise before the guard: FPC warns that a managed result may be
    unassigned on the raise path. }
  Result := nil;
  LLen := Length(AText);
  if LLen > 125 then
    raise Exception.Create('test helper only builds short frames');
  SetLength(Result, 6 + LLen);
  LMask[0] := 1; LMask[1] := 2; LMask[2] := 3; LMask[3] := 4;
  Result[0] := $81;                    { FIN + text opcode }
  Result[1] := Byte($80 or LLen);      { MASK + length }
  for I := 0 to 3 do
    Result[2 + I] := LMask[I];
  for I := 1 to LLen do
    Result[5 + I] := Byte(Ord(AText[I])) xor LMask[(I - 1) mod 4];
end;

function DoHandshake(ASock: LongInt): string;
begin
  SendStr(ASock,
    'GET /ws HTTP/1.1'#13#10 +
    'Host: 127.0.0.1:' + IntToStr(TEST_PORT) + #13#10 +
    'Upgrade: websocket'#13#10 +
    'Connection: Upgrade'#13#10 +
    'Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ=='#13#10 +
    'Sec-WebSocket-Version: 13'#13#10#13#10);
  Result := ReadAvailable(ASock, 5000);
end;

{ ── Checks ─────────────────────────────────────────────────────────────── }

procedure RunChecks;
var
  LSock:  LongInt;
  LResp:  string;
  LFrame: TBytes;
  LEcho:  string;
  LSeen:  string;
  LOpen:  Integer;
begin
  WriteLn;
  WriteLn('── 1  first EAGAIN must not close the connection ──────────────');
  LSock := ConnectClient;
  if LSock < 0 then
  begin
    Check('connect to server', False, 'socket/connect failed');
    Exit;
  end;

  LResp := DoHandshake(LSock);
  Check('handshake returns 101 Switching Protocols',
    Pos('101', LResp) > 0, Copy(LResp, 1, 60));

  { The server's read loop has now issued at least one recv and taken EAGAIN.
    Pre-fix the loop broke here, the handler returned, and the HTTP pipeline
    wrote a stray 200 onto the upgraded socket — which is what this detects. }
  Sleep(SETTLE_MS);
  LOpen := ProbeQuiet(LSock, 150, LSeen);
  Check('server silent after the first EAGAIN (no stray HTTP response)',
    LOpen = 0,
    ProbeName(LOpen) + ' :: ' + Copy(LSeen, 1, 48));

  WriteLn;
  WriteLn('── 2  an idle connection stays open ───────────────────────────');
  Sleep(IDLE_WAIT_MS);   { > 4 read ticks — several select() timeouts elapse }
  LOpen := ProbeQuiet(LSock, 150, LSeen);
  Check('still silent and open after ' + IntToStr(IDLE_WAIT_MS) + ' ms idle',
    LOpen = 0,
    ProbeName(LOpen) + ' :: ' + Copy(LSeen, 1, 48));

  WriteLn;
  WriteLn('── 3  a frame sent later reaches OnMessage ────────────────────');
  LFrame := BuildMaskedTextFrame('ping-after-idle');
  fpSend(LSock, @LFrame[0], Length(LFrame), 0);
  LEcho := ReadAvailable(LSock, 5000);
  Check('echo received for a frame sent after the idle period',
    Pos('echo:ping-after-idle', LEcho) > 0,
    'bytes=' + IntToStr(Length(LEcho)));

  WriteLn;
  WriteLn('── 4  peer close releases the read loop ───────────────────────');
  { Abrupt close, no WebSocket close frame — the harsher case. }
  fpClose(LSock);
  Sleep(READ_TICK_MS * 3);   { give the loop more than one tick to notice }

  LSock := ConnectClient;
  if LSock < 0 then
    Check('server still accepts connections after peer close', False, 'connect failed')
  else
  begin
    SendStr(LSock,
      'GET /ping HTTP/1.1'#13#10 +
      'Host: 127.0.0.1:' + IntToStr(TEST_PORT) + #13#10 +
      'Connection: close'#13#10#13#10);
    LResp := ReadAvailable(LSock, 5000);
    Check('server still serves HTTP after an abrupt peer close',
      (Pos('200', LResp) > 0) and (Pos('pong', LResp) > 0),
      Copy(LResp, 1, 60));
    fpClose(LSock);
  end;
end;

var
  LServer: TServerThread;
begin
  WriteLn('FIX-WS-NONBLOCK / FIX-WS-EINTR regression — FPC + epoll');
  WriteLn('Port: ', TEST_PORT);

  THorse.Get('/ws',   RouteWS);
  THorse.Get('/ping', RoutePing);

  LServer := TServerThread.Create(False);
  try
    Sleep(700);   { let the listener bind before the first connect }
    try
      RunChecks;
    except
      on E: Exception do
        Check('unexpected exception', False, E.ClassName + ': ' + E.Message);
    end;
  finally
    try
      THorse.StopListen;
    except
    end;
    LServer.WaitFor;
    LServer.Free;
  end;

  WriteLn;
  WriteLn(Format('%d passed, %d failed', [GPass, GFail]));
  if GFail = 0 then
    WriteLn('All checks PASSED.')
  else
    WriteLn('Some checks FAILED — see above.');
  ExitCode := GFail;
end.
