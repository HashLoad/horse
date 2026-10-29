unit Horse.Provider.Indy.Socket;

interface

{$IF NOT DEFINED(FPC)}
uses
  IdSocketHandle;

type
  THorseIndySocket = class
  public
    class procedure ConfigureKeepAlive(const ABinding: TIdSocketHandle); static;
  end;
{$ENDIF}

implementation

{$IF NOT DEFINED(FPC)}
{$IF DEFINED(LINUX)}
uses
  IdStackConsts;
{$ENDIF}

class procedure THorseIndySocket.ConfigureKeepAlive(const ABinding: TIdSocketHandle);
begin
{$IF DEFINED(LINUX)}
  // Older Indy sends SetKeepAliveValues arguments directly to Linux, while
  // newer Indy converts milliseconds to seconds. Use the native units here
  // so both implementations configure a 60-second idle and 1-second interval.
  ABinding.SetSockOpt(Id_SOL_SOCKET, Id_SO_KEEPALIVE, 1);
  ABinding.SetSockOpt(Id_SOL_TCP, Id_TCP_KEEPIDLE, 60);
  ABinding.SetSockOpt(Id_SOL_TCP, Id_TCP_KEEPINTVL, 1);
{$ELSE}
  ABinding.SetKeepAliveValues(True, 60000, 1000);
{$ENDIF}
end;
{$ENDIF}

end.
