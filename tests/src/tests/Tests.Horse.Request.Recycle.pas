unit Tests.Horse.Request.Recycle;

interface

uses
  DUnitX.TestFramework, Horse.Request, Horse.Core.Param, System.SysUtils,
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

initialization
  TDUnitX.RegisterTestFixture(TTestHorseRequestRecycle);

end.
