unit Tests.Api.Vcl;

interface

uses
  DUnitX.TestFramework, System.JSON, RESTRequest4D.Request, System.Classes,
  Controllers.Api, Horse, Horse.Jhonson, SysUtils;

type
  [TestFixture]
  TApiTest = class(TObject)
  private
    FJSONObject: TJSONObject;
    FJSONArray: TJSONArray;

    procedure CreateApi;
    procedure StartApiListen;
    procedure StartApiListenPort;
    procedure StartApiListenHost;
    procedure StartApiListens;
    procedure StartApiPortListens;
    procedure StopApiListen;
    procedure StopApi;
  public
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestGet;
    [Test]
    [TestCase('Test01', 'POST request test')]
    procedure TestPost(const AValue: string);
    [Test]
    [TestCase('Test01', 'PUT request test')]
    procedure TestPut(const AValue: string);
    [Test]
    [TestCase('Test01', '1')]
    procedure TestDelete(const AValue: string);
    [Test]
    procedure TestGStartApiPortListens;
    [Test]
    procedure TestCreateApi;
    [Test]
    procedure TestToHorse;
  end;

implementation

{ TApiTest }

procedure TApiTest.StartApiListen;
begin
  if (not THorse.IsRunning) then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        THorse
          .Use(Jhonson);

        Controllers.Api.Registry;
        THorse.MaxConnections := 10;
        THorse.Listen;
      end).Start;
  end;
end;

procedure TApiTest.StartApiListenPort;
begin
  if (not THorse.IsRunning) then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        Controllers.Api.Registry;
        THorse.Listen(9000);
      end).Start;
  end;
end;

procedure TApiTest.StartApiListenHost;
begin
  if (not THorse.IsRunning) then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        Controllers.Api.Registry;
        THorse.Listen('0.0.0.0');
      end).Start;
  end;
end;

procedure TApiTest.StartApiListens;
begin
  if (not THorse.IsRunning) then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        Controllers.Api.Registry;
        THorse.Listen(
          procedure(Horse: THorse)
          begin
          end);
      end).Start;
  end;
end;

procedure TApiTest.StartApiPortListens;
begin
  if (not THorse.IsRunning) then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        Controllers.Api.Registry;
        THorse.Listen(9000,
          procedure(Horse: THorse)
          begin
          end);
      end).Start;
  end;
end;

procedure TApiTest.StopApiListen;
begin
  THorse.StopListen;
end;

procedure TApiTest.StopApi;
begin
  // Warnings have been disabled for this segment as the stop has been depreciated.
  {$WARNINGS OFF}
  THorse.StopListen;
  {$WARNINGS ON}
end;

procedure TApiTest.TestGStartApiPortListens;
begin
  StartApiPortListens;
  StopApi;
end;

procedure TApiTest.TestGet;
var
  LResponse: IResponse;
begin
  StartApiListen;
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .Get;

  FJSONArray := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray;

  Assert.AreEqual(9000, THorse.Port);
  Assert.AreEqual('0.0.0.0', THorse.Host);
  Assert.AreEqual(10, THorse.MaxConnections);
  Assert.AreEqual(LResponse.StatusCode, 200);
  Assert.AreEqual(FJSONArray.Count, 3);
  StopApiListen;
end;

procedure TApiTest.TestPost(const AValue: string);
var
  LResponse: IResponse;
begin
  StartApiListenPort;
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .AddBody('{"value": "' + AValue + '"}')
    .Post;

  FJSONObject := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  Assert.AreEqual(LResponse.StatusCode, 201);

  if (not FJSONObject.GetValue('value').Null) then
    Assert.AreEqual(AValue, FJSONObject.GetValue('value').Value)
  else
    Assert.Fail('The return is not without correct format.');
  StopApiListen;
end;

procedure TApiTest.TestPut(const AValue: string);
var
  LResponse: IResponse;
begin
  StartApiListenHost;
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .AddBody('{"value": "' + AValue + '"}')
    .Put;

  FJSONObject := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  Assert.AreEqual(LResponse.StatusCode, 200);

  if (not FJSONObject.GetValue('value').Null) then
    Assert.AreEqual(AValue, FJSONObject.GetValue('value').Value)
  else
    Assert.Fail('The return is not in the correct format.');
  StopApiListen;
end;

procedure TApiTest.TestDelete(const AValue: string);
var
  LResponse: IResponse;
begin
  StartApiListens;
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test/' + AValue)
    .Accept('application/json')
    .Delete;

  FJSONObject := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  Assert.AreEqual(LResponse.StatusCode, 200);

  if (not FJSONObject.GetValue('value').Null) then
    Assert.AreEqual(AValue, FJSONObject.GetValue('value').Value)
  else
    Assert.Fail('The return is not in the correct format.');
  StopApiListen;
end;

procedure TApiTest.CreateApi;
begin
  // Warnings have been disabled for this segment as the create has been depreciated.
  {$WARNINGS OFF}
  THorse.Create;
  {$WARNINGS ON}
end;

procedure TApiTest.TearDown;
begin
  FreeAndNil(FJSONObject);
  FreeAndNil(FJSONArray);
end;

procedure TApiTest.TestCreateApi;
begin
  Assert.WillRaise(CreateApi, Exception, 'The Horse instance has already been created');
end;

procedure TApiTest.TestToHorse;
begin
  Assert.IsNotNull(THorse.ToModule.ToHorse, 'Module instance must not be null');
end;

initialization
  TDUnitX.RegisterTestFixture(TApiTest);

end.
