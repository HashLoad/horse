unit Tests.Api;

interface

uses
  DUnitX.TestFramework, System.JSON, RESTRequest4D.Request, System.Classes,
  Controllers.Api, Horse;

type
  [TestFixture]
  TApiTest = class(TObject)
  private
    procedure StartApi;
  public
    [Test]
    procedure TestGet;
    [Test]
    [TestCase('Test01', 'Teste de requisição POST')]
    procedure TestPost(const AValue: string);
    [Test]
    [TestCase('Test01', 'Teste de requisição PUT')]
    procedure TestPut(const AValue: string);
    [Test]
    [TestCase('Test01', '1')]
    procedure TestDelete(const AValue: string);
  end;

implementation

{ TApiTest }

procedure TApiTest.StartApi;
begin
  if (not THorse.IsRunning) then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        IsConsole := False;
        Controllers.Api.Registry;

        THorse.Listen;
        THorse.StopListen;

        IsConsole := True;
        THorse.Listen(9000);
      end).Start;
  end;
end;

procedure TApiTest.TestGet;
var
  LResponse: IResponse;
  LContent: TJSONArray;
begin
  StartApi;
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .Get;

  LContent := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray;

  Assert.AreEqual(9000, THorse.Port);
  Assert.AreEqual('0.0.0.0', THorse.Host);
  Assert.AreEqual(0, THorse.MaxConnections);
  Assert.AreEqual(LResponse.StatusCode, 200);
  Assert.AreEqual(LContent.Count, 3);
end;

procedure TApiTest.TestPost(const AValue: string);
var
  LResponse: IResponse;
  LContent: TJSONObject;
begin
  StartApi;
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .AddBody('{"value": "' + AValue + '"}')
    .Post;

  LContent := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  Assert.AreEqual(LResponse.StatusCode, 201);

  if (not LContent.GetValue('value').Null) then
    Assert.AreEqual(AValue, LContent.GetValue('value').Value)
  else
    Assert.Fail('O retorno não está no formato correto.');
end;

procedure TApiTest.TestPut(const AValue: string);
var
  LResponse: IResponse;
  LContent: TJSONObject;
begin
  StartApi;
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .AddBody('{"value": "' + AValue + '"}')
    .Put;

  LContent := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  Assert.AreEqual(LResponse.StatusCode, 200);

  if (not LContent.GetValue('value').Null) then
    Assert.AreEqual(AValue, LContent.GetValue('value').Value)
  else
    Assert.Fail('O retorno não está no formato correto.');
end;

procedure TApiTest.TestDelete(const AValue: string);
var
  LResponse: IResponse;
  LContent: TJSONObject;
begin
  StartApi;
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test/' + AValue)
    .Accept('application/json')
    .Delete;

  LContent := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  Assert.AreEqual(LResponse.StatusCode, 200);

  if (not LContent.GetValue('value').Null) then
    Assert.AreEqual(AValue, LContent.GetValue('value').Value)
  else
    Assert.Fail('O retorno não está no formato correto.');
end;

initialization
  TDUnitX.RegisterTestFixture(TApiTest);

end.
