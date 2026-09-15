unit Tests.Integration.QueryDecode;

// End-to-end companion to Tests.Horse.Request.DecodeOnce: a real request
// through the default provider must return query and form values decoded
// exactly once.
//
// On this path Horse's own InitializeQuery decodes each value when it stores
// it, and WebBroker decodes ContentFields, so reading the value decoded it a
// SECOND time. A value whose decoded form holds a literal percent sign raised
// EConvertError inside the handler (HTTP 500), and '+' next to one became a
// space. The handler reads each value twice and then through Field, so a
// read-time write-back cannot hide.
//
// URLs are fully percent-encoded on purpose. THTTPClient's TURI keeps an
// existing encoding only when decoding the text would change it, so canonical
// input reaches the server byte-for-byte.

interface

uses
  DUnitX.TestFramework, Horse, Horse.Commons, System.SysUtils, System.Classes,
  System.Threading, System.Net.HttpClient, System.Net.URLClient, Tests.CleanupHelper;

type
  [TestFixture]
  TTestIntegrationQueryDecode = class
  private
    const TEST_PORT = 9126;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    [TestCase('TrailingPercent', '100%25,100%')]
    [TestCase('PercentMidValue', '50%25off,50%off')]
    [TestCase('PlusAndEncodedPercent', 'a%2B%2541,a+%41')]
    procedure TestQueryValueDecodedOnce(AEncoded, AExpected: string);

    [Test]
    [TestCase('TrailingPercent', '100%25,100%')]
    [TestCase('PlusAndEncodedPercent', 'a%2B%2541,a+%41')]
    procedure TestFormFieldDecodedOnce(AEncoded, AExpected: string);
  end;

implementation

{ TTestIntegrationQueryDecode }

procedure TTestIntegrationQueryDecode.SetupFixture;
begin
  THorse.Get('/decode-once/query',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send(Req.Query['v'] + '|' + Req.Query['v'] + '|' + Req.Query.Field('v').AsString);
    end);

  THorse.Put('/decode-once/form',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send(Req.ContentFields['v'] + '|' + Req.ContentFields['v'] + '|' +
        Req.ContentFields.Field('v').AsString);
    end);

  TThread.CreateAnonymousThread(
    procedure
    begin
      // Loopback only: the test server has no reason to be reachable from the network.
      THorse.Listen(TEST_PORT, '127.0.0.1');
    end).Start;

  Sleep(1500);
end;

procedure TTestIntegrationQueryDecode.TearDownFixture;
begin
  ClearGlobalState;
  Sleep(500);
end;

procedure TTestIntegrationQueryDecode.TestQueryValueDecodedOnce(AEncoded, AExpected: string);
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
begin
  LClient := THTTPClient.Create;
  try
    LClient.CustomHeaders['Connection'] := 'close';
    // 127.0.0.1, not localhost: the server binds IPv4 loopback only, and
    // localhost may resolve to ::1 first.
    LRes := LClient.Get(Format('http://127.0.0.1:%d/decode-once/query?v=%s', [TEST_PORT, AEncoded]));

    // A second decode raises inside the handler, which Horse turns into a 500
    // carrying the exception message: show it rather than a bare status.
    Assert.AreEqual(200, LRes.StatusCode, LRes.ContentAsString);
    Assert.AreEqual(AExpected + '|' + AExpected + '|' + AExpected, LRes.ContentAsString,
      'first read | repeated read | Field');
  finally
    LClient.Free;
  end;
end;

procedure TTestIntegrationQueryDecode.TestFormFieldDecodedOnce(AEncoded, AExpected: string);
var
  LClient: THTTPClient;
  LRes: IHTTPResponse;
  LSource: TStringStream;
begin
  LClient := THTTPClient.Create;
  LSource := TStringStream.Create('v=' + AEncoded, TEncoding.UTF8);
  try
    LClient.CustomHeaders['Connection'] := 'close';
    LClient.CustomHeaders['Content-Type'] := 'application/x-www-form-urlencoded; charset=utf-8';
    LRes := LClient.Put(Format('http://127.0.0.1:%d/decode-once/form', [TEST_PORT]), LSource);

    Assert.AreEqual(200, LRes.StatusCode, LRes.ContentAsString);
    Assert.AreEqual(AExpected + '|' + AExpected + '|' + AExpected, LRes.ContentAsString,
      'first read | repeated read | Field');
  finally
    LSource.Free;
    LClient.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestIntegrationQueryDecode);

end.
