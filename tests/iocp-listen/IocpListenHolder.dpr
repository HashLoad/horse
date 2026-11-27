program IocpListenHolder;

{ Holds a port with the IOCP provider until it is killed.

  Started by Tests.Horse.Provider.IOCP.Listen: CA1b checks that a second IOCP
  is refused on the port this one holds, and CA3b kills it with
  TerminateProcess while a client is connected and listens again on the port.

  Usage: IocpListenHolder <port> }

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse.Provider.IOCP;

begin
  try
    THorseProviderIOCP.Listen(StrToInt(ParamStr(1)), '0.0.0.0', nil, nil);
  except
    on E: Exception do
    begin
      Writeln('FAIL: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
