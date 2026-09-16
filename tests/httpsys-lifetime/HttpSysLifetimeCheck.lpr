program HttpSysLifetimeCheck;

{$MODE DELPHI}{$H+}

uses
  Classes,
  SysUtils,
  Horse,
  Horse.Provider.HttpSys;

const
  TEST_PORT = 9095;

type
  TListenThread = class(TThread)
  private
    FErrorMessage: string;
  protected
    procedure Execute; override;
  public
    property ErrorMessage: string read FErrorMessage;
  end;

procedure TListenThread.Execute;
begin
  try
    THorse.Listen(TEST_PORT, 'localhost');
  except
    on E: Exception do
      FErrorMessage := E.ClassName + ': ' + E.Message;
  end;
end;

procedure StartAndStop;
var
  LListener: TListenThread;
  LAttempts: Integer;
begin
  LListener := TListenThread.Create(True);
  try
    LListener.FreeOnTerminate := False;
    LListener.Start;

    for LAttempts := 1 to 100 do
    begin
      if THorseProviderHttpSys.IsRunning then
        Break;
      Sleep(50);
    end;

    if not THorseProviderHttpSys.IsRunning then
    begin
      LListener.WaitFor;
      Writeln('HTTP.sys did not start: ', LListener.ErrorMessage);
      Halt(1);
    end;

    THorse.StopListen;
    LListener.WaitFor;
    if LListener.ErrorMessage <> '' then
    begin
      Writeln(LListener.ErrorMessage);
      Halt(1);
    end;
  finally
    LListener.Free;
  end;
end;

begin
  { Exercise cleanup and recreation: stale OVERLAPPED records must neither
    leak after the first stop nor survive into the second listener. }
  StartAndStop;
  StartAndStop;
end.
