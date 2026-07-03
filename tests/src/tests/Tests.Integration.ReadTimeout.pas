unit Tests.Integration.ReadTimeout;

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, System.SysUtils, System.Classes,
  System.Threading, IdTCPClient, System.Net.HttpClient, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationReadTimeout = class
  private
    const TEST_PORT = 9099;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestReadTimeoutClosesConnection;
  end;

implementation

uses
  System.Diagnostics;

{ TTestIntegrationReadTimeout }

procedure TTestIntegrationReadTimeout.SetupFixture;
begin
  // Configura o ReadTimeout do Horse (em milissegundos para o Indy)
  // O Indy herda este valor e aplica nas conexões aceitas
  THorse.ReadTimeout := 1000; // 1 segundo

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('pong');
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(TEST_PORT);
    end).Start;

  Sleep(1500);
end;

procedure TTestIntegrationReadTimeout.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(500);
end;

procedure TTestIntegrationReadTimeout.TestReadTimeoutClosesConnection;
var
  LConnected: Boolean;
  LDisconnected: Boolean;
  LClient: TIdTCPClient;
  LStopWatch: TStopwatch;
begin
  LDisconnected := False;
  LClient := TIdTCPClient.Create(nil);
  try
    LClient.Host := '127.0.0.1';
    LClient.Port := TEST_PORT;
    LClient.ConnectTimeout := 2000; 
    
    LClient.Connect;
    LConnected := LClient.Connected;
    Assert.IsTrue(LConnected, 'Should connect to the server');

    // Conectou. Agora ficamos inativos sem enviar nenhuma requisição HTTP.
    // O servidor deve nos desconectar após o ReadTimeout (1 segundo).
    LStopWatch := TStopwatch.StartNew;
    while (LStopWatch.ElapsedMilliseconds < 3000) and (not LDisconnected) do
    begin
      try
        // CheckForDataOnSource verifica se há dados no socket com timeout de 100ms.
        // Se a conexão cair, LClient.Connected será atualizado para False
        // ou levantará uma exceção de socket.
        LClient.IOHandler.CheckForDataOnSource(100);
        if not LClient.Connected then
        begin
          LDisconnected := True;
          Break;
        end;
      except
        LDisconnected := True;
        Break;
      end;
      Sleep(100);
    end;

    Assert.IsTrue(LDisconnected, 'Server should actively disconnect the connection after the read timeout.');
    Assert.IsTrue(LStopWatch.ElapsedMilliseconds >= 900, 'Server should wait approximately the read timeout limit.');
  finally
    LClient.Free;
  end;
end;

initialization
{$IFDEF MSWINDOWS}
  TDUnitX.RegisterTestFixture(TTestIntegrationReadTimeout);
{$ENDIF}

end.
