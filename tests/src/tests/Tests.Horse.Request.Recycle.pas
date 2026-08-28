unit Tests.Horse.Request.Recycle;

interface

uses
  DUnitX.TestFramework, Horse.Request, Horse.Response, Horse.Core.Param, System.SysUtils,
  System.Generics.Collections,
  {$IF DEFINED(FPC)} HTTPApp {$ELSE} Web.HTTPApp {$ENDIF}, Horse.Commons;

type
  [TestFixture]
  TTestHorseRequestRecycle = class
  private
    FRequest: THorseRequest;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRequestRecyclingAndStateIsolation;
    [Test]
    procedure TestRequestArenaRecyclingAndObjectLifetime;
    [Test]
    procedure TestResponseSendBytesOverload;
    [Test]
    procedure TestRequestStateAndMatchedRouteCycle;

    { FIX-WS-8441: IsWebSocket detection and Clear-reset coverage }
    [Test]
    procedure TestIsWebSocketDefaultFalse;
    [Test]
    procedure TestIsWebSocketDetectedByUpgradeHeader;
    [Test]
    procedure TestIsWebSocketDetectedByExplicitSignal;
    [Test]
    procedure TestIsWebSocketResetByClear;
  end;

implementation

{ TTestHorseRequestRecycle }

procedure TTestHorseRequestRecycle.Setup;
begin
  FRequest := THorseRequest.Create(nil);
end;

procedure TTestHorseRequestRecycle.TearDown;
begin
  FRequest.Free;
end;

procedure TTestHorseRequestRecycle.TestRequestRecyclingAndStateIsolation;
var
  LHeadersDict: TDictionary<string, string>;
  LParamsDict: TDictionary<string, string>;
  LQueryDict: TDictionary<string, string>;
begin
  FRequest.Populate('POST', mtPost, '/api/test', 'application/json', '127.0.0.1');
  
  LHeadersDict := FRequest.Headers.Dictionary;
  LParamsDict := FRequest.Params.Dictionary;
  LQueryDict := FRequest.Query.Dictionary;
  
  LHeadersDict.AddOrSetValue('X-Test-Header', 'Value1');
  LParamsDict.AddOrSetValue('id', '100');
  LQueryDict.AddOrSetValue('filter', 'active');

  Assert.AreEqual('Value1', FRequest.Headers.Dictionary.Items['X-Test-Header']);
  Assert.AreEqual('100', FRequest.Params.Dictionary.Items['id']);
  Assert.AreEqual('active', FRequest.Query.Dictionary.Items['filter']);

  // Executa o Clear para reciclar
  FRequest.Clear;

  // Os dicionários ainda devem estar instanciados
  Assert.IsNotNull(FRequest.Headers, 'Headers wrapper should remain allocated');
  Assert.IsNotNull(FRequest.Params, 'Params wrapper should remain allocated');
  Assert.IsNotNull(FRequest.Query, 'Query wrapper should remain allocated');

  // E os dicionários devem estar vazios
  Assert.AreEqual<Integer>(0, FRequest.Headers.Dictionary.Count, 'Headers should be empty post-recycle');
  Assert.AreEqual<Integer>(0, FRequest.Params.Dictionary.Count, 'Params should be empty post-recycle');
  Assert.AreEqual<Integer>(0, FRequest.Query.Dictionary.Count, 'Query should be empty post-recycle');

  // Segunda requisição reutilizando a mesma instância
  FRequest.Populate('GET', mtGet, '/api/other', 'text/html', '192.168.0.1');
  
  FRequest.Headers.Dictionary.AddOrSetValue('X-Test-Header', 'Value2');
  FRequest.Params.Dictionary.AddOrSetValue('id', '200');
  FRequest.Query.Dictionary.AddOrSetValue('filter', 'inactive');

  Assert.AreEqual('Value2', FRequest.Headers.Dictionary.Items['X-Test-Header']);
  Assert.AreEqual('200', FRequest.Params.Dictionary.Items['id']);
  Assert.AreEqual('inactive', FRequest.Query.Dictionary.Items['filter']);
  Assert.AreEqual<Integer>(1, FRequest.Headers.Dictionary.Count);
  Assert.AreEqual<Integer>(1, FRequest.Params.Dictionary.Count);
  Assert.AreEqual<Integer>(1, FRequest.Query.Dictionary.Count);
end;

type
  TTestArenaObject = class
  private
    FDestroyedFlag: PBoolean;
  public
    constructor Create(ADestroyedFlag: PBoolean);
    destructor Destroy; override;
  end;

constructor TTestArenaObject.Create(ADestroyedFlag: PBoolean);
begin
  inherited Create;
  FDestroyedFlag := ADestroyedFlag;
  FDestroyedFlag^ := False;
end;

destructor TTestArenaObject.Destroy;
begin
  FDestroyedFlag^ := True;
  inherited;
end;

procedure TTestHorseRequestRecycle.TestRequestArenaRecyclingAndObjectLifetime;
var
  LObjectDestroyed: Boolean;
  LTestObj: TTestArenaObject;
begin
  LObjectDestroyed := False;
  Assert.IsNotNull(FRequest.Arena, 'Arena should be lazy-created');
  LTestObj := TTestArenaObject.Create(@LObjectDestroyed);
  FRequest.Arena.RegisterObject(LTestObj);
  Assert.IsFalse(LObjectDestroyed, 'Object should be active');
  FRequest.Clear;
  Assert.IsTrue(LObjectDestroyed, 'Object should be automatically freed by Arena during request recycle');
end;

procedure TTestHorseRequestRecycle.TestResponseSendBytesOverload;
var
  LResponse: THorseResponse;
  LBytes: TBytes;
begin
  LResponse := THorseResponse.Create(nil);
  try
    SetLength(LBytes, 4);
    LBytes[0] := 1;
    LBytes[1] := 2;
    LBytes[2] := 3;
    LBytes[3] := 4;

    LResponse.Send(LBytes);
    Assert.AreEqual<Integer>(4, Length(LResponse.BodyBytes));
    Assert.AreEqual<Byte>(1, LResponse.BodyBytes[0]);
    Assert.AreEqual<Byte>(2, LResponse.BodyBytes[1]);
    Assert.AreEqual<Byte>(3, LResponse.BodyBytes[2]);
    Assert.AreEqual<Byte>(4, LResponse.BodyBytes[3]);
  finally
    LResponse.Free;
  end;
end;

procedure TTestHorseRequestRecycle.TestRequestStateAndMatchedRouteCycle;
var
  LDestroyed: Boolean;
  LTestObj: TTestArenaObject;
begin
  LDestroyed := False;
  FRequest.MatchedRoute := '/my/route/:id';
  LTestObj := TTestArenaObject.Create(@LDestroyed);

  FRequest.State.Add('context', LTestObj);

  Assert.AreEqual('/my/route/:id', FRequest.MatchedRoute);
  Assert.IsNotNull(FRequest.State.Items['context']);
  Assert.IsFalse(LDestroyed);

  // Limpa o request (reciclagem no pool)
  FRequest.Clear;

  Assert.AreEqual('', FRequest.MatchedRoute, 'MatchedRoute should be empty after Clear');
  Assert.AreEqual<Integer>(0, FRequest.State.Count, 'State dictionary should be empty after Clear');
  Assert.IsTrue(LDestroyed, 'State values should be automatically freed after Clear due to doOwnsValues');
end;

{ FIX-WS-8441 ----------------------------------------------------------------
  Four tests covering the provider-signal WebSocket upgrade path:
  1. default is False (no false positives on HTTP/1.1 non-upgrade requests)
  2. detection via the HTTP/1.1 `Upgrade: websocket` header (existing path)
  3. detection via the explicit SetWebSocketUpgrade signal (HTTP/2 / RFC 8441)
  4. FWebSocketUpgrade is reset to False by Clear (pooled-request safety)
---------------------------------------------------------------------------- }

procedure TTestHorseRequestRecycle.TestIsWebSocketDefaultFalse;
begin
  Assert.IsFalse(FRequest.IsWebSocket,
    'IsWebSocket must be False on a freshly created request with no header and no provider signal');
end;

procedure TTestHorseRequestRecycle.TestIsWebSocketDetectedByUpgradeHeader;
begin
  FRequest.Headers.Dictionary.AddOrSetValue('Upgrade', 'websocket');
  Assert.IsTrue(FRequest.IsWebSocket,
    'IsWebSocket must be True when the Upgrade: websocket header is present (HTTP/1.1 path)');
end;

procedure TTestHorseRequestRecycle.TestIsWebSocketDetectedByExplicitSignal;
begin
  FRequest.SetWebSocketUpgrade(True);
  Assert.IsTrue(FRequest.IsWebSocket,
    'IsWebSocket must be True when the provider sets the explicit upgrade signal (HTTP/2 / RFC 8441 path)');
end;

procedure TTestHorseRequestRecycle.TestIsWebSocketResetByClear;
begin
  FRequest.SetWebSocketUpgrade(True);
  Assert.IsTrue(FRequest.IsWebSocket, 'Precondition: upgrade signal must be set before Clear');
  FRequest.Clear;
  Assert.IsFalse(FRequest.IsWebSocket,
    'IsWebSocket must be False after Clear — FWebSocketUpgrade must be reset for safe request recycling');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseRequestRecycle);

end.
