program IndyKeepAliveCheck;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  IdGlobal, IdSocketHandle, IdStack, IdStackConsts,
{$IF DEFINED(MSWINDOWS)}
  IdStackWindows,
{$ENDIF}
  Horse.Provider.Indy.Socket;

{$IF DEFINED(MSWINDOWS)}
type
  TRecordingStack = class(TIdStackWindows)
    procedure SetKeepAliveValues(ASocket: TIdStackSocketHandle;
      const AEnabled: Boolean; const ATimeMS, AInterval: Integer); override;
  end;
var
  Calls: Integer;

procedure TRecordingStack.SetKeepAliveValues(ASocket: TIdStackSocketHandle;
  const AEnabled: Boolean; const ATimeMS, AInterval: Integer);
begin
  if not AEnabled or (ATimeMS <> 60000) or (AInterval <> 1000) then
    raise Exception.Create('Windows keep-alive arguments changed');
  Inc(Calls);
  inherited;
end;
{$ENDIF}

var
  Checks: Integer;

procedure Check(Condition: Boolean; const MessageText: string);
begin
  Inc(Checks);
  if not Condition then
    raise Exception.Create(MessageText);
end;

procedure CheckSocket(const Version: TIdIPVersion);
var
  Socket: TIdSocketHandle;
  Value: Integer;
{$IF DEFINED(LINUX)}
  Failed: Boolean;
{$ENDIF}
begin
  Socket := TIdSocketHandle.Create(nil);
  try
    Socket.IPVersion := Version;
    Socket.AllocateSocket;
{$IF DEFINED(LINUX)}
    // Negative control: the old Indy API interprets these as seconds and
    // rejects 60000; newer Indy converts them to the valid 60/1 seconds.
    Failed := False;
    try
      Socket.SetKeepAliveValues(True, 60000, 1000);
    except
      on E: EIdSocketError do
      begin
        Check(E.LastError = 22, 'Unexpected socket error in the old-call control');
        Failed := True;
      end;
    end;
{$IF DEFINED(HORSE_TEST_LEGACY_INDY)}
    Check(Failed, 'Legacy Indy must reproduce Socket Error #22');
{$ELSE}
    Check(not Failed, 'Modern Indy control must accept milliseconds');
{$ENDIF}
{$ENDIF}
    THorseIndySocket.ConfigureKeepAlive(Socket);
    GStack.GetSocketOption(Socket.Handle, Id_SOL_SOCKET, Id_SO_KEEPALIVE, Value);
    Check(Value <> 0, 'SO_KEEPALIVE must be enabled');
{$IF DEFINED(LINUX)}
    GStack.GetSocketOption(Socket.Handle, Id_SOL_TCP, Id_TCP_KEEPIDLE, Value);
    Check(Value = 60, 'TCP_KEEPIDLE must be 60 seconds');
    GStack.GetSocketOption(Socket.Handle, Id_SOL_TCP, Id_TCP_KEEPINTVL, Value);
    Check(Value = 1, 'TCP_KEEPINTVL must be 1 second');
{$ENDIF}
    // Applying the configuration again must remain valid.
    THorseIndySocket.ConfigureKeepAlive(Socket);
    GStack.GetSocketOption(Socket.Handle, Id_SOL_SOCKET, Id_SO_KEEPALIVE, Value);
    Check(Value <> 0, 'Repeated configuration must preserve SO_KEEPALIVE');
  finally
    Socket.Free;
  end;
end;

begin
  try
{$IF DEFINED(MSWINDOWS)}
    SetStackClass(TRecordingStack);
{$ENDIF}
    TIdStack.IncUsage;
    try
      CheckSocket(Id_IPv4);
      CheckSocket(Id_IPv6);
{$IF DEFINED(MSWINDOWS)}
      Check(Calls = 4, 'Windows must delegate every call to Indy');
{$ENDIF}
    finally
      TIdStack.DecUsage;
    end;
    Writeln('PASS: IPv4/IPv6 keep-alive; checks=', Checks);
  except
    on E: Exception do
    begin
      Writeln('FAIL: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
