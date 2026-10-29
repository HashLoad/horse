program ProviderKeepAliveCheck;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  IdTCPClient, IdStack, IdStackConsts,
{$IF DEFINED(LINUX)}
  IdStackVCLPosix,
{$ENDIF}
  Horse, Horse.Provider.Abstract;

{$IF DEFINED(LINUX)}
type
  // Models the native-seconds contract of older Indy in a full HTTP server.
  // The socket-only runner additionally tests the actual old Indy sources.
  TLegacyContractStack = class(TIdStackVCLPosix)
    procedure SetKeepAliveValues(ASocket: TIdStackSocketHandle;
      const AEnabled: Boolean; const ATimeMS, AInterval: Integer); override;
  end;

procedure TLegacyContractStack.SetKeepAliveValues(ASocket: TIdStackSocketHandle;
  const AEnabled: Boolean; const ATimeMS, AInterval: Integer);
begin
  SetSocketOption(ASocket, Id_SOL_SOCKET, Id_SO_KEEPALIVE, Ord(AEnabled));
  if AEnabled then
  begin
    SetSocketOption(ASocket, Id_SOL_TCP, Id_TCP_KEEPIDLE, ATimeMS);
    SetSocketOption(ASocket, Id_SOL_TCP, Id_TCP_KEEPINTVL, AInterval);
  end;
end;
{$ENDIF}

const
  TestPort = 19263;
  Body = 'keepalive-ok';

procedure Run;
var
  Ready: TEvent;
  Server: TThread;
  ServerError: string;
  Client: TIdTCPClient;
  Line: string;
  I: Integer;

procedure Check(Condition: Boolean; const MessageText: string);
begin
  if not Condition then
    raise Exception.Create(MessageText);
end;

begin
  Ready := nil;
  Server := nil;
  try
    ReportMemoryLeaksOnShutdown := True;
{$IF DEFINED(LINUX)}
    if FindCmdLineSwitch('legacy-contract') then
      SetStackClass(TLegacyContractStack);
{$ENDIF}
    THorse.Get('/keepalive',
      procedure(Req: THorseRequest; Res: THorseResponse)
      begin
        Res.Send(Body);
      end);
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
      for I := 1 to 3 do
      begin
        Client := TIdTCPClient.Create(nil);
        try
          Client.Host := '127.0.0.1';
          Client.Port := TestPort;
          Client.ConnectTimeout := 3000;
          Client.ReadTimeout := 3000;
          Client.Connect;
          Client.IOHandler.WriteLn('GET /keepalive HTTP/1.1');
          Client.IOHandler.WriteLn('Host: localhost');
          Client.IOHandler.WriteLn('Connection: close');
          Client.IOHandler.WriteLn('');
          Line := Client.IOHandler.ReadLn;
          Check(Pos('200', Line) > 0, 'Expected HTTP 200: ' + Line);
          repeat
            Line := Client.IOHandler.ReadLn;
          until Line = '';
          Check(Client.IOHandler.ReadString(Length(Body)) = Body, 'Response body mismatch');
        finally
          Client.Free;
        end;
      end;
    finally
      if THorse.IsRunning then
        THorse.StopListen;
      Server.WaitFor;
    end;
    Writeln('PASS: 3 accepted connections and HTTP 200 responses');
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
