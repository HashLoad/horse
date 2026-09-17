program ConsoleStabilityCheck;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  IdHTTP,
  IdHTTPHeaderInfo,
  IdGlobalProtocols,
  Horse,
  Horse.Request,
  Horse.Response,
  Horse.Provider.Console;

const
  TEST_PORT = 19181;
  CLIENT_COUNT = 50;
  REQUESTS_PER_CLIENT = 1500;
  CONNECT_TIMEOUT_MS = 5000;
  READ_TIMEOUT_MS = 10000;

type
  TServerThread = class(TThread)
  private
    FErrorMessage: string;
  protected
    procedure Execute; override;
  public
    property ErrorMessage: string read FErrorMessage;
  end;

  TClientThread = class(TThread)
  private
    FClientIndex: Integer;
    FErrorCount: Integer;
    FMaxLatencyMS: UInt64;
    FFirstError: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const AClientIndex: Integer);
    property ErrorCount: Integer read FErrorCount;
    property MaxLatencyMS: UInt64 read FMaxLatencyMS;
    property FirstError: string read FFirstError;
  end;

procedure Ping(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('ok');
end;

procedure TServerThread.Execute;
begin
  try
    THorse.Listen(TEST_PORT, '127.0.0.1');
  except
    on E: Exception do
      FErrorMessage := E.ClassName + ': ' + E.Message;
  end;
end;

constructor TClientThread.Create(const AClientIndex: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FClientIndex := AClientIndex;
end;

procedure TClientThread.Execute;
var
  LHTTP: TIdHTTP;
  LIteration: Integer;
  LStartedAt: UInt64;
  LElapsed: UInt64;
  LResponse: string;
begin
  LHTTP := TIdHTTP.Create(nil);
  try
    LHTTP.ConnectTimeout := CONNECT_TIMEOUT_MS;
    LHTTP.ReadTimeout := READ_TIMEOUT_MS;
    LHTTP.ProtocolVersion := pv1_1;
    LHTTP.HTTPOptions := LHTTP.HTTPOptions + [hoKeepOrigProtocol];
    LHTTP.Request.Connection := 'keep-alive';

    for LIteration := 1 to REQUESTS_PER_CLIENT do
    begin
      LStartedAt := GetTickCount64;
      try
        LResponse := LHTTP.Get(Format('http://127.0.0.1:%d/stability?client=%d&request=%d',
          [TEST_PORT, FClientIndex, LIteration]));
        if (LHTTP.ResponseCode <> 200) or (LResponse <> 'ok') then
          raise Exception.CreateFmt('status=%d body="%s"',
            [LHTTP.ResponseCode, LResponse]);
      except
        on E: Exception do
        begin
          Inc(FErrorCount);
          if FFirstError = '' then
            FFirstError := Format('client=%d request=%d %s: %s',
              [FClientIndex, LIteration, E.ClassName, E.Message]);
          LHTTP.Disconnect;
        end;
      end;

      LElapsed := GetTickCount64 - LStartedAt;
      if LElapsed > FMaxLatencyMS then
        FMaxLatencyMS := LElapsed;
    end;
  finally
    LHTTP.Free;
  end;
end;

var
  LServer: TServerThread;
  LClients: TObjectList<TClientThread>;
  LClient: TClientThread;
  LIndex: Integer;
  LAttempts: Integer;
  LErrors: Integer;
  LMaxLatencyMS: UInt64;
  LFirstError: string;
begin
  THorse.Get('/stability', Ping);
  LServer := TServerThread.Create(True);
  LClients := TObjectList<TClientThread>.Create(True);
  try
    LServer.FreeOnTerminate := False;
    LServer.Start;
    for LAttempts := 1 to 100 do
    begin
      if THorseProvider.IsRunning then
        Break;
      Sleep(50);
    end;
    if not THorseProvider.IsRunning then
    begin
      LServer.WaitFor;
      Writeln('Server failed to start: ', LServer.ErrorMessage);
      Halt(1);
    end;

    for LIndex := 1 to CLIENT_COUNT do
      LClients.Add(TClientThread.Create(LIndex));
    for LClient in LClients do
      LClient.Start;
    for LClient in LClients do
      LClient.WaitFor;

    LErrors := 0;
    LMaxLatencyMS := 0;
    LFirstError := '';
    for LClient in LClients do
    begin
      Inc(LErrors, LClient.ErrorCount);
      if LClient.MaxLatencyMS > LMaxLatencyMS then
        LMaxLatencyMS := LClient.MaxLatencyMS;
      if (LFirstError = '') and (LClient.FirstError <> '') then
        LFirstError := LClient.FirstError;
    end;

    Writeln(Format('clients=%d requests=%d errors=%d max_latency_ms=%d',
      [CLIENT_COUNT, CLIENT_COUNT * REQUESTS_PER_CLIENT, LErrors, LMaxLatencyMS]));
    if LErrors <> 0 then
    begin
      Writeln('First error: ', LFirstError);
      Halt(1);
    end;
  finally
    if THorseProvider.IsRunning then
      THorse.StopListen;
    LServer.WaitFor;
    LClients.Free;
    LServer.Free;
  end;
end.
