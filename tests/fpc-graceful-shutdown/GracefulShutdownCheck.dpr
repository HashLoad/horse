program GracefulShutdownCheck;

{ Graceful shutdown of the FPC HTTPApplication provider.

  Usage: GracefulShutdownCheck <port> [drain|in-onlisten]

  drain (default): a worker waits for the server, opens an idle keep-alive
  connection, starts a slow request (1.5 s) and calls StopListenGraceful.
  The check passes when:
  - the slow request in flight answers 200;
  - the idle keep-alive connection sees EOF in under 1 s, while the slow
    request is still draining;
  - StopListenGraceful returns before Listen does;
  - Listen returns at all.

  in-onlisten: StopListenGraceful is called inside the Listen callback, the
  earliest point; Listen must return without serving.

  Exit code 0 means pass. Before this change the provider inherited the
  abstract StopListen, which raises "StopListen not implemented", and Listen
  never returned. }

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  SyncObjs,
  ssockets,
  fphttpclient,
  Horse;

const
  SLOW_MS = 1500;
  IDLE_EOF_LIMIT_MS = 1000;
  READY_TIMEOUT_MS = 10000;
  STOP_TIMEOUT_MS = 5000;

var
  GPort: Integer;
  GStopReturned: LongInt;
  GListenReturnedFirst: LongInt;
  GSlowStatus: Integer;
  GIdleEofMs: Int64;
  GFailure: string;

procedure Slow(Req: THorseRequest; Res: THorseResponse);
begin
  Sleep(SLOW_MS);
  Res.Send('slow-ok');
end;

procedure Ping(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

procedure StopFromThread;
begin
  THorse.StopListenGraceful(STOP_TIMEOUT_MS);
  InterlockedExchange(GStopReturned, 1);
end;

procedure StopInOnListen;
begin
  THorse.StopListenGraceful(STOP_TIMEOUT_MS);
end;

function Url(const APath: string): string;
begin
  Result := Format('http://127.0.0.1:%d%s', [GPort, APath]);
end;

function ServerAnswers: Boolean;
var
  LClient: TFPHTTPClient;
begin
  LClient := TFPHTTPClient.Create(nil);
  try
    LClient.KeepConnection := False;
    LClient.IOTimeout := 500;
    try
      Result := Trim(LClient.Get(Url('/ping'))) = 'pong';
    except
      Result := False;
    end;
  finally
    LClient.Free;
  end;
end;

type
  TSlowRequest = class(TThread)
  protected
    procedure Execute; override;
  end;

  TDrainWorker = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TSlowRequest.Execute;
var
  LClient: TFPHTTPClient;
begin
  LClient := TFPHTTPClient.Create(nil);
  try
    LClient.KeepConnection := False;
    LClient.IOTimeout := 10000;
    try
      LClient.Get(Url('/slow'));
      GSlowStatus := LClient.ResponseStatusCode;
    except
      GSlowStatus := -1;
    end;
  finally
    LClient.Free;
  end;
end;

function OpenIdleKeepAlive: TInetSocket;
const
  REQUEST = 'GET /ping HTTP/1.1'#13#10'Host: 127.0.0.1'#13#10'Connection: keep-alive'#13#10#13#10;
var
  LRequest: AnsiString;
  LBuffer: array[0..1023] of Byte;
  LRead: Integer;
  LReceived: AnsiString;
begin
  Result := TInetSocket.Create('127.0.0.1', GPort);
  Result.IOTimeout := 3000;
  LRequest := REQUEST;
  Result.WriteBuffer(LRequest[1], Length(LRequest));
  LReceived := '';
  repeat
    LRead := Result.Read(LBuffer, SizeOf(LBuffer));
    if LRead > 0 then
      LReceived := LReceived + Copy(PAnsiChar(@LBuffer[0]), 1, LRead);
  until (LRead <= 0) or (Pos('pong', LReceived) > 0);
  if Pos('pong', LReceived) = 0 then
    raise Exception.Create('keep-alive connection did not get its response');
end;

function WaitEof(const ASocket: TInetSocket): Int64;
var
  LBuffer: array[0..255] of Byte;
  LStart: QWord;
begin
  LStart := GetTickCount64;
  ASocket.IOTimeout := 5000;
  while ASocket.Read(LBuffer, SizeOf(LBuffer)) > 0 do;
  Result := GetTickCount64 - LStart;
end;

procedure TDrainWorker.Execute;
var
  LDeadline: QWord;
  LIdle: TInetSocket;
  LSlow: TSlowRequest;
begin
  try
    LDeadline := GetTickCount64 + READY_TIMEOUT_MS;
    while not ServerAnswers do
    begin
      if GetTickCount64 > LDeadline then
        raise Exception.Create('server did not answer');
      Sleep(50);
    end;

    LIdle := OpenIdleKeepAlive;
    try
      LSlow := TSlowRequest.Create(False);
      try
        Sleep(200);
        TThread.CreateAnonymousThread(StopFromThread).Start;
        GIdleEofMs := WaitEof(LIdle);
        LSlow.WaitFor;
      finally
        LSlow.Free;
      end;
    finally
      LIdle.Free;
    end;
  except
    on E: Exception do
      GFailure := E.ClassName + ': ' + E.Message;
  end;
end;

var
  LMode: string;
  LWorker: TDrainWorker;
begin
  GPort := StrToInt(ParamStr(1));
  LMode := ParamStr(2);
  THorse.Get('/ping', Ping);
  THorse.Get('/slow', Slow);

  if LMode = 'in-onlisten' then
  begin
    THorse.Listen(GPort, StopInOnListen);
    WriteLn('in-onlisten: Listen returned');
    Halt(0);
  end;

  LWorker := TDrainWorker.Create(False);
  THorse.Listen(GPort);
  if InterlockedCompareExchange(GStopReturned, 0, 0) = 0 then
    GListenReturnedFirst := 1;
  LWorker.WaitFor;
  LWorker.Free;

  WriteLn(Format('slow=%d idle-eof=%dms listen-before-stop=%d failure=%s',
    [GSlowStatus, GIdleEofMs, GListenReturnedFirst, GFailure]));

  if GFailure <> '' then
    Halt(1);
  if GSlowStatus <> 200 then
    Halt(2);
  if GIdleEofMs >= IDLE_EOF_LIMIT_MS then
    Halt(3);
  if GListenReturnedFirst <> 0 then
    Halt(4);
  WriteLn('drain: passed');
end.
