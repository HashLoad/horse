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
    procedure TestPost(const Value: string);
    [Test]
    [TestCase('Test01', 'Teste de requisição PUT')]
    procedure TestPut(const Value: string);
    [Test]
    [TestCase('Test01', '1')]
    procedure TestDelete(const Value: string);
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
  response: IResponse;
  content: TJSONArray;
begin
  StartApi;
  response := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .Get;

  content := TJSONObject.ParseJSONValue(response.Content) as TJSONArray;

  Assert.AreEqual(9000, THorse.Port);
  Assert.AreEqual('0.0.0.0', THorse.Host);
  Assert.AreEqual(0, THorse.MaxConnections);
  Assert.AreEqual(response.StatusCode, 200);
  Assert.AreEqual(content.Count, 3);
end;

procedure TApiTest.TestPost(const Value: string);
var
  response: IResponse;
  content: TJSONObject;
begin
  StartApi;
  response := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .AddBody('{"value": "' + Value + '"}')
    .Post;

  content := TJSONObject.ParseJSONValue(response.Content) as TJSONObject;
  Assert.AreEqual(response.StatusCode, 201);

  if (not content.GetValue('value').Null) then
    Assert.AreEqual(Value, content.GetValue('value').Value)
  else
    Assert.Fail('O retorno não está no formato correto.');
end;

procedure TApiTest.TestPut(const Value: string);
var
  response: IResponse;
  content: TJSONObject;
begin
  StartApi;
  response := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .AddBody('{"value": "' + Value + '"}')
    .Put;

  content := TJSONObject.ParseJSONValue(response.Content) as TJSONObject;
  Assert.AreEqual(response.StatusCode, 200);

  if (not content.GetValue('value').Null) then
    Assert.AreEqual(Value, content.GetValue('value').Value)
  else
    Assert.Fail('O retorno não está no formato correto.');
end;

procedure TApiTest.TestDelete(const Value: string);
var
  response: IResponse;
  content: TJSONObject;
begin
  StartApi;
  response := TRequest.New.BaseURL('http://localhost:9000/Api/Test/' + Value)
    .Accept('application/json')
    .Delete;

  content := TJSONObject.ParseJSONValue(response.Content) as TJSONObject;
  Assert.AreEqual(response.StatusCode, 200);

  if (not content.GetValue('value').Null) then
    Assert.AreEqual(Value, content.GetValue('value').Value)
  else
    Assert.Fail('O retorno não está no formato correto.');
end;

initialization
  TDUnitX.RegisterTestFixture(TApiTest);

end.
