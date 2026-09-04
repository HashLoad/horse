program WebSocketEpollRegression;

{$MODE DELPHI}{$H+}

uses
  cthreads,
  SysUtils,
  Classes,
  SyncObjs,
  Sockets,
  BaseUnix,
  Horse.Core.WebSocket,
  Horse.Provider.Socket.WebSocket;

const
  TEST_TIMEOUT_MS = 3000;
  TEST_IDLE_MS = 500;

type
  TTransportReadThread = class(TThread)
  private
    FTransport: IHorseWebSocketTransport;
    FFinished: TEvent;
    FReadCount: Integer;
    FData: TBytes;
  protected
    procedure Execute; override;
  public
    constructor Create(const ATransport: IHorseWebSocketTransport);
    destructor Destroy; override;
    property FinishedEvent: TEvent read FFinished;
    property ReadCount: Integer read FReadCount;
    property Data: TBytes read FData;
  end;

constructor TTransportReadThread.Create(
  const ATransport: IHorseWebSocketTransport);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FTransport := ATransport;
  FFinished := TEvent.Create(nil, True, False, '');
  SetLength(FData, 16);
end;

destructor TTransportReadThread.Destroy;
begin
  FFinished.Free;
  inherited;
end;

procedure TTransportReadThread.Execute;
begin
  try
    FReadCount := FTransport.Read(FData, Length(FData));
  finally
    FFinished.SetEvent;
  end;
end;

procedure Check(const ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure SetNonBlocking(const ASocket: cint);
var
  LFlags: cint;
begin
  LFlags := fpFcntl(ASocket, F_GETFL, 0);
  Check(LFlags >= 0, 'Nao foi possivel ler as flags do socket');
  Check(fpFcntl(ASocket, F_SETFL, LFlags or O_NONBLOCK) = 0,
    'Nao foi possivel configurar o socket como non-blocking');
end;

procedure TestInitialEagainAndLaterData;
const
  PAYLOAD: array[0..3] of Byte = (Ord('p'), Ord('i'), Ord('n'), Ord('g'));
var
  LSockets: array[0..1] of cint;
  LTransport: IHorseWebSocketTransport;
  LReader: TTransportReadThread;
begin
  Check(fpSocketPair(AF_UNIX, SOCK_STREAM, 0, @LSockets[0]) = 0,
    'Nao foi possivel criar o socket pair');
  try
    SetNonBlocking(LSockets[0]);
    LTransport := THorseWebSocketSocketTransport.Create(LSockets[0]);
    LReader := TTransportReadThread.Create(LTransport);
    try
      LReader.Start;

      { Longer than the transport select tick: an idle non-blocking socket must
        remain connected instead of treating EAGAIN as a closed peer. }
      Sleep(TEST_IDLE_MS);
      Check(LReader.FinishedEvent.WaitFor(0) <> wrSignaled,
        'EAGAIN encerrou a leitura enquanto o peer permanecia conectado');
      Check(LTransport.IsConnected,
        'O transporte marcou um socket ocioso como desconectado');

      Check(fpSend(LSockets[1], @PAYLOAD[0], Length(PAYLOAD), 0) = Length(PAYLOAD),
        'Nao foi possivel enviar o payload de teste');
      Check(LReader.FinishedEvent.WaitFor(TEST_TIMEOUT_MS) = wrSignaled,
        'A leitura nao recebeu os dados enviados depois do EAGAIN');
      Check(LReader.ReadCount = Length(PAYLOAD),
        'A leitura retornou uma quantidade inesperada de bytes');
      Check(CompareByte(LReader.Data[0], PAYLOAD[0], Length(PAYLOAD)) = 0,
        'A leitura retornou um payload diferente do enviado');
    finally
      LTransport.Close;
      LReader.WaitFor;
      LReader.Free;
      LTransport := nil;
    end;
  finally
    fpClose(LSockets[1]);
  end;
end;

procedure TestCloseUnblocksPendingRead;
var
  LSockets: array[0..1] of cint;
  LTransport: IHorseWebSocketTransport;
  LReader: TTransportReadThread;
begin
  Check(fpSocketPair(AF_UNIX, SOCK_STREAM, 0, @LSockets[0]) = 0,
    'Nao foi possivel criar o socket pair');
  try
    SetNonBlocking(LSockets[0]);
    LTransport := THorseWebSocketSocketTransport.Create(LSockets[0]);
    LReader := TTransportReadThread.Create(LTransport);
    try
      LReader.Start;
      Sleep(TEST_IDLE_MS);
      Check(LReader.FinishedEvent.WaitFor(0) <> wrSignaled,
        'A leitura terminou antes do fechamento solicitado');

      LTransport.Close;
      Check(LReader.FinishedEvent.WaitFor(TEST_TIMEOUT_MS) = wrSignaled,
        'Close nao desbloqueou a leitura pendente');
      Check(LReader.ReadCount = 0,
        'A leitura desbloqueada por Close deveria retornar zero');
      Check(not LTransport.IsConnected,
        'O transporte permaneceu conectado depois de Close');
    finally
      LTransport.Close;
      LReader.WaitFor;
      LReader.Free;
      LTransport := nil;
    end;
  finally
    fpClose(LSockets[1]);
  end;
end;

begin
  try
    TestInitialEagainAndLaterData;
    TestCloseUnblocksPendingRead;
    Writeln('WEBSOCKET EPOLL REGRESSION TEST: SUCCESS');
    ExitCode := 0;
  except
    on E: Exception do
    begin
      Writeln('WEBSOCKET EPOLL REGRESSION TEST: FAILED: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
