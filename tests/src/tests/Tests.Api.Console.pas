unit Tests.Api.Console;

{.$DEFINE HORSE_PROVIDER_HTTPSYS}

interface

uses
  DUnitX.TestFramework, System.JSON, RESTRequest4D, System.Classes,
  Controllers.Api, Horse, Horse.Jhonson, SysUtils, Tests.CleanupHelper;

type
  [TestFixture]
  TApiTest = class(TObject)
  private
    FJSONObject: TJSONObject;
    FJSONArray: TJSONArray;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    [TestCase('Test01', 'GET request test')]
    procedure TestGet;

    [Test]
    [TestCase('Test02', 'POST request test')]
    procedure TestPost(const AValue: string);

    [Test]
    [TestCase('Test03', 'PUT request test')]
    procedure TestPut(const AValue: string);

    [Test]
    [TestCase('Test04', 'DELETE request test')]
    procedure TestDelete(const AValue: string);

    [Test]
    [TestCase('Test05', 'PATCH request test')]
    procedure TestPatch(const AValue: string);

    [Test]
    procedure TestMiddlewareAuthUnauthorized;
    [Test]
    procedure TestMiddlewareAuthSuccess;
    [Test]
    procedure TestMiddlewareCorsHeaders;
    [Test]
    procedure TestMiddlewareCorsPreflightOptions;

  end;

implementation

uses
  System.Net.HttpClient, System.Net.URLClient;

procedure TApiTest.TestGet;
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .Get;

  FJSONArray := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray;
  Assert.AreEqual(9000, THorse.Port);
  {$IFDEF HORSE_PROVIDER_HTTPSYS}
  Assert.AreEqual('localhost', THorse.Host);
  {$ELSE}
  Assert.AreEqual('0.0.0.0', THorse.Host);
  {$ENDIF}
  Assert.AreEqual(10, THorse.MaxConnections);
  Assert.AreEqual(LResponse.StatusCode, 200);
  Assert.AreEqual(FJSONArray.Count, 3);
end;

// procedure TApiTest.TestHead(const AValue: string);

procedure TApiTest.TestPatch(const AValue: string);
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test')
    .Accept('application/json')
    .AddBody('{"value": "' + AValue + '"}')
    .Patch;

  FJSONObject := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  Assert.AreEqual(LResponse.StatusCode, 200);

  if (not FJSONObject.GetValue('value').Null) then
    Assert.AreEqual(AValue, FJSONObject.GetValue('value').Value)
  else
    Assert.Fail('The return is not in the correct format.');
end;

procedure TApiTest.TestPost(const AValue: string);
var
  LResponse: IResponse;
begin
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
end;

procedure TApiTest.TestPut(const AValue: string);
var
  LResponse: IResponse;
begin
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
end;

procedure TApiTest.TestDelete(const AValue: string);
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New.BaseURL('http://localhost:9000/Api/Test/' + AValue)
    .Accept('application/json')
    .Delete;

  FJSONObject := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  Assert.AreEqual(LResponse.StatusCode, 200);

  if (not FJSONObject.GetValue('value').Null) then
    Assert.AreEqual(AValue, FJSONObject.GetValue('value').Value)
  else
    Assert.Fail('The return is not in the correct format.');
end;

procedure TApiTest.TestMiddlewareAuthUnauthorized;
var
  LClient: THTTPClient;
  LResponse: IHTTPResponse;
begin
  LClient := THTTPClient.Create;
  try
    LResponse := LClient.Get('http://localhost:9000/Api/Protected');
    Assert.AreEqual(401, LResponse.StatusCode);
    Assert.AreEqual('Unauthorized', LResponse.ContentAsString);
  finally
    LClient.Free;
  end;
end;

procedure TApiTest.TestMiddlewareAuthSuccess;
var
  LClient: THTTPClient;
  LResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
begin
  LClient := THTTPClient.Create;
  try
    SetLength(LHeaders, 1);
    LHeaders[0] := TNetHeader.Create('Authorization', 'Bearer MySecretToken');
    LResponse := LClient.Get('http://localhost:9000/Api/Protected', nil, LHeaders);
    Assert.AreEqual(200, LResponse.StatusCode);
    Assert.AreEqual('SecretData', LResponse.ContentAsString);
  finally
    LClient.Free;
  end;
end;

procedure TApiTest.TestMiddlewareCorsHeaders;
var
  LClient: THTTPClient;
  LResponse: IHTTPResponse;
begin
  LClient := THTTPClient.Create;
  try
    LResponse := LClient.Get('http://localhost:9000/Api/Cors');
    Assert.AreEqual(200, LResponse.StatusCode);
    Assert.AreEqual('CorsData', LResponse.ContentAsString);
    Assert.AreEqual('*', LResponse.HeaderValue['Access-Control-Allow-Origin']);
  finally
    LClient.Free;
  end;
end;

procedure TApiTest.TestMiddlewareCorsPreflightOptions;
var
  LClient: THTTPClient;
  LResponse: IHTTPResponse;
begin
  LClient := THTTPClient.Create;
  try
    LResponse := LClient.Options('http://localhost:9000/Api/Cors');
    Assert.AreEqual(204, LResponse.StatusCode);
    Assert.AreEqual('*', LResponse.HeaderValue['Access-Control-Allow-Origin']);
    Assert.AreEqual('GET, POST, OPTIONS', LResponse.HeaderValue['Access-Control-Allow-Methods']);
  finally
    LClient.Free;
  end;
end;

procedure TApiTest.Setup;
begin
  Writeln('TApiTest.Setup INICIADO!');
  {$IFDEF HORSE_PROVIDER_IOCP}
  Writeln('DIRECTIVE HORSE_PROVIDER_IOCP IS DEFINED!');
  {$ELSE}
  Writeln('DIRECTIVE HORSE_PROVIDER_IOCP IS NOT DEFINED!!!');
  {$ENDIF}
  if (not THorse.IsRunning) then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        THorse
          .Use(Jhonson());

        Controllers.Api.Registry;
        THorse.MaxConnections := 10;
        {$IFDEF HORSE_PROVIDER_HTTPSYS}
        THorse.Host := 'localhost';
        {$ENDIF}
        THorse.Listen;
      end).Start;
    Sleep(500);
  end;
end;

procedure TApiTest.TearDown;
begin
  FreeAndNil(FJSONObject);
  FreeAndNil(FJSONArray);
end;

procedure TApiTest.TearDownFixture;
begin
  ClearGlobalState;
end;

initialization
  TDUnitX.RegisterTestFixture(TApiTest);

end.
