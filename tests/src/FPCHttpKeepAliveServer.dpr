program FPCHttpKeepAliveServer;

{ Compile with FPC 3.3.1+ and run ../fpc_keepalive_regression.py while this
  server is listening. The provider's keep-alive path is intentionally not
  enabled on FPC 3.2.2. }

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Horse,
  Horse.Commons;

procedure Ping(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('pong');
end;

begin
  THorse.Get('/ping', Ping);
  THorse.Listen(9901, '127.0.0.1');
end.
