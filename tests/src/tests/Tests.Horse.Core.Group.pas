unit Tests.Horse.Core.Group;

interface

uses
  DUnitX.TestFramework, System.Classes, SysUtils, Horse, RESTRequest4D,
  Tests.CleanupHelper;

type
  [TestFixture]
  TTestHorseCoreGroup = class(TObject)
  public
    [Setup]
    procedure Setup;

    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestGroupRouteEndFluency;
  end;

implementation

procedure TestDoDeleteApi(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('delete1');
end;

procedure TestDoGetApi(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('get2');
end;

procedure TestDoPostApi(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('post2');
end;

procedure TestDoPutApi(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('put3');
end;

procedure TTestHorseCoreGroup.Setup;
begin
  try
    if THorse.IsRunning then
      ClearGlobalState;
  except
  end;

  if not THorse.IsRunning then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        THorse.Group.Prefix('/api')
          .Delete('/test1/:id', TestDoDeleteApi)
          .Route('/test2')
            .Get(TestDoGetApi)
            .Post(TestDoPostApi)
          .&End
          .Put('/teste3', TestDoPutApi);

        THorse.Listen(9099);
      end).Start;
    Sleep(500);
  end;
end;

procedure TTestHorseCoreGroup.TearDownFixture;
begin
  try
    ClearGlobalState;
  except
  end;
end;

procedure TTestHorseCoreGroup.TestGroupRouteEndFluency;
var
  LResponse: IResponse;
begin
  // 1. Validar DELETE /api/test1/:id
  LResponse := TRequest.New.BaseURL('http://localhost:9099/api/test1/123').Delete;
  Assert.AreEqual(200, LResponse.StatusCode);
  Assert.AreEqual('delete1', LResponse.Content.Trim);

  // 2. Validar GET /api/test2 (dentro do Route)
  LResponse := TRequest.New.BaseURL('http://localhost:9099/api/test2').Get;
  Assert.AreEqual(200, LResponse.StatusCode);
  Assert.AreEqual('get2', LResponse.Content.Trim);

  // 3. Validar POST /api/test2 (dentro do Route)
  LResponse := TRequest.New.BaseURL('http://localhost:9099/api/test2').Post;
  Assert.AreEqual(200, LResponse.StatusCode);
  Assert.AreEqual('post2', LResponse.Content.Trim);

  // 4. Validar PUT /api/teste3 (apos o &End do Route no mesmo grupo)
  // Sob o bug atual, esta rota falhara com 404 Not Found porque o prefixo /api foi perdido
  LResponse := TRequest.New.BaseURL('http://localhost:9099/api/teste3').Put;
  Assert.AreEqual(200, LResponse.StatusCode, 'A rota /api/teste3 nao foi localizada sob o prefixo correto.');
  Assert.AreEqual('put3', LResponse.Content.Trim);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseCoreGroup);

end.
