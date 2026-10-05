unit Tests.Horse.Provider.RawAdapters;

{ Regression tests for FIX-RAWFIELDS-1.

  TInterfacedWebRequest.Create used to pass the inherited ContentFields property to
  the raw request unconditionally. Reading that property makes TWebRequest parse the
  request body as form data and URL-decode it, whatever the content type — so a JSON
  body holding a literal '%' raised EConvertError FROM THE CONSTRUCTOR, before the
  route or any middleware ran.

  Reported against a live service: a PUT carrying

      "descripcion":"Impuesto al Valor Agregado 13%"

  failed every time, while the same document without the percent sign succeeded.

  WHAT THESE TESTS ASSERT, AND WHAT THEY DELIBERATELY DO NOT. They assert the
  observable contract: a non-form body never reaches PopulateContentFields and never
  raises, and a form body still populates and decodes. They do NOT assert how
  TWebRequest tokenises a body internally — that is RTL behaviour, it differs between
  Delphi versions, and pinning it here would make these tests fail on a change that
  does not affect Horse.

  NOTE for the non-form cases: they must not read req.ContentFields to prove the
  point. Reading it is the very act that triggers the parse, so an assertion like
  "ContentFields.Count = 0" would perform the damage it is checking for. The fake
  records whether it was asked instead. }

interface

{ DELPHI ONLY, deliberately. The defect is in the WebBroker branch of
  TInterfacedWebRequest, which descends from TWebRequest and inherits its lazy
  ContentFields extraction. The FPC branch descends from TRequest (fpHTTP) and has no
  such property read, so there is nothing to regress there — and Horse's DUnitX suite
  does not run under FPC in any case. The unit compiles to nothing rather than being
  excluded from the project, so the FPC build cannot break on a file it ignores. }
{$IF NOT DEFINED(FPC)}

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestRawAdaptersContentFields = class
  public
    [Test]
    [TestCase('Json',              'application/json')]
    [TestCase('JsonCharset',       'application/json; charset=UTF-8')]
    [TestCase('JsonNoSpace',       'application/json;charset=utf-8')]
    [TestCase('TextPlain',         'text/plain')]
    [TestCase('OctetStream',       'application/octet-stream')]
    [TestCase('Grpc',              'application/grpc')]
    [TestCase('Empty',             '')]
    procedure NonFormBodyIsNeverAskedForContentFields(const AContentType: string);

    [Test]
    [TestCase('PrefixedUrlencoded', 'x-application/x-www-form-urlencoded')]
    [TestCase('SuffixedMultipart',  'multipart/form-data-x')]
    [TestCase('MentionedInParam',   'application/json; note=application/x-www-form-urlencoded')]
    procedure LookalikeMediaTypeIsNotTreatedAsForm(const AContentType: string);

    [Test]
    [TestCase('Urlencoded',        'application/x-www-form-urlencoded')]
    [TestCase('UrlencodedCharset', 'application/x-www-form-urlencoded; charset=UTF-8')]
    [TestCase('UrlencodedPadded',  '  application/x-www-form-urlencoded  ')]
    [TestCase('UrlencodedUpper',   'Application/X-WWW-Form-Urlencoded')]
    [TestCase('Multipart',         'multipart/form-data; boundary=----abc123')]
    procedure FormBodyIsStillAskedForContentFields(const AContentType: string);

    [Test]
    procedure JsonBodyWithTrailingPercentDoesNotRaise;

    [Test]
    procedure JsonBodyWithCharsetAndTrailingPercentDoesNotRaise;

    [Test]
    procedure FormBodyStillDecodesContentFieldsExactlyOnce;
  end;

{$IFEND}

implementation

{$IF NOT DEFINED(FPC)}

uses
  System.Classes,
  System.SysUtils,
  Web.HTTPApp,
  Horse.Provider.RawInterfaces,
  Horse.Provider.RawAdapters;

const
  { The reporter's value, verbatim: 30 characters with '%' last. }
  TRIGGER_JSON =
    '{"tributos":[{"codigo":"20",' +
    '"descripcion":"Impuesto al Valor Agregado 13%","valor":2.08}]}';

type
  { Minimal IHorseRawRequest. Every method must be present or the class will not
    compile (E2291); the ones this fixture does not exercise return empty values
    rather than raising, so a future test that touches one gets a wrong answer
    rather than an unrelated exception. }
  TFakeRawRequest = class(TInterfacedObject, IHorseRawRequest)
  private
    FContentType: string;
    FContent: string;
    FContentFieldsAsked: Boolean;
  public
    constructor Create(const AContentType, AContent: string);

    function GetMethod: string;
    function GetProtocolVersion: string;
    function GetURL: string;
    function GetPathInfo: string;
    function GetQueryString: string;
    function GetHost: string;
    function GetRemoteAddr: string;
    function GetServerPort: Integer;
    function GetContentType: string;
    function GetContent: string;
{$IF DEFINED(FPC)}
    function GetContentLength: Integer;
{$ELSEIF CompilerVersion >= 32.0}
    function GetContentLength: Int64;
{$ELSE}
    function GetContentLength: Integer;
{$IFEND}
    function GetFieldByName(const AName: string): string;

    procedure PopulateHeaders(ADest: TStrings);
    procedure PopulateQueryFields(ADest: TStrings);
    procedure PopulateContentFields(ADest: TStrings);
    procedure PopulateCookieFields(ADest: TStrings);

    function ReadBody(var Buffer; Count: Integer): Integer;

    property ContentFieldsAsked: Boolean read FContentFieldsAsked;
  end;

constructor TFakeRawRequest.Create(const AContentType, AContent: string);
begin
  inherited Create;
  FContentType := AContentType;
  FContent := AContent;
  FContentFieldsAsked := False;
end;

function TFakeRawRequest.GetMethod: string;          begin Result := 'PUT'; end;
function TFakeRawRequest.GetProtocolVersion: string; begin Result := 'HTTP/1.1'; end;
function TFakeRawRequest.GetURL: string;             begin Result := '/invoice'; end;
function TFakeRawRequest.GetPathInfo: string;        begin Result := '/invoice'; end;
function TFakeRawRequest.GetQueryString: string;     begin Result := ''; end;
function TFakeRawRequest.GetHost: string;            begin Result := '127.0.0.1'; end;
function TFakeRawRequest.GetRemoteAddr: string;      begin Result := '127.0.0.1'; end;
function TFakeRawRequest.GetServerPort: Integer;     begin Result := 9000; end;
function TFakeRawRequest.GetContentType: string;     begin Result := FContentType; end;
function TFakeRawRequest.GetContent: string;         begin Result := FContent; end;

{$IF DEFINED(FPC)}
function TFakeRawRequest.GetContentLength: Integer;
{$ELSEIF CompilerVersion >= 32.0}
function TFakeRawRequest.GetContentLength: Int64;
{$ELSE}
function TFakeRawRequest.GetContentLength: Integer;
{$IFEND}
begin
  Result := Length(FContent);
end;

function TFakeRawRequest.GetFieldByName(const AName: string): string;
begin
  if SameText(AName, 'Content-Type') then
    Result := FContentType
  else
    Result := '';
end;

procedure TFakeRawRequest.PopulateHeaders(ADest: TStrings);
begin
end;

procedure TFakeRawRequest.PopulateQueryFields(ADest: TStrings);
begin
end;

procedure TFakeRawRequest.PopulateContentFields(ADest: TStrings);
begin
  { The flag is the whole point of this fake: it records that the adapter reached
    this call, which it can only do by first READING the ContentFields property. }
  FContentFieldsAsked := True;
end;

procedure TFakeRawRequest.PopulateCookieFields(ADest: TStrings);
begin
end;

function TFakeRawRequest.ReadBody(var Buffer; Count: Integer): Integer;
begin
  Result := 0;
end;

{ ---------------------------------------------------------------------------- }

procedure TTestRawAdaptersContentFields.NonFormBodyIsNeverAskedForContentFields(
  const AContentType: string);
var
  LFake: TFakeRawRequest;
  LRaw: IHorseRawRequest;
  LReq: TInterfacedWebRequest;
begin
  LFake := TFakeRawRequest.Create(AContentType, TRIGGER_JSON);
  LRaw := LFake;
  LReq := TInterfacedWebRequest.Create(LRaw);
  try
    Assert.IsFalse(LFake.ContentFieldsAsked,
      'ContentFields was populated for ' + AContentType +
      ' - reading that property parses the body as a form and URL-decodes it');
  finally
    LReq.Free;
  end;
end;

procedure TTestRawAdaptersContentFields.LookalikeMediaTypeIsNotTreatedAsForm(
  const AContentType: string);
var
  LFake: TFakeRawRequest;
  LRaw: IHorseRawRequest;
  LReq: TInterfacedWebRequest;
begin
  { A substring test would accept every one of these. The media type must be
    compared exactly, after its parameters are stripped. }
  LFake := TFakeRawRequest.Create(AContentType, TRIGGER_JSON);
  LRaw := LFake;
  LReq := TInterfacedWebRequest.Create(LRaw);
  try
    Assert.IsFalse(LFake.ContentFieldsAsked,
      AContentType + ' is not a form media type and must not be treated as one');
  finally
    LReq.Free;
  end;
end;

procedure TTestRawAdaptersContentFields.FormBodyIsStillAskedForContentFields(
  const AContentType: string);
var
  LFake: TFakeRawRequest;
  LRaw: IHorseRawRequest;
  LReq: TInterfacedWebRequest;
begin
  LFake := TFakeRawRequest.Create(AContentType, 'v=1');
  LRaw := LFake;
  LReq := TInterfacedWebRequest.Create(LRaw);
  try
    Assert.IsTrue(LFake.ContentFieldsAsked,
      AContentType + ' IS a form media type - skipping it would be a regression, ' +
      'not a fix');
  finally
    LReq.Free;
  end;
end;

procedure TTestRawAdaptersContentFields.JsonBodyWithTrailingPercentDoesNotRaise;
begin
  Assert.WillNotRaise(
    procedure
    var
      LRaw: IHorseRawRequest;
      LReq: TInterfacedWebRequest;
    begin
      LRaw := TFakeRawRequest.Create('application/json', TRIGGER_JSON);
      LReq := TInterfacedWebRequest.Create(LRaw);
      LReq.Free;
    end,
    Exception,
    'constructing the adapter for a JSON body holding a literal percent must not ' +
    'raise - this raised EConvertError "Error decoding URL style (%XX)" before ' +
    'FIX-RAWFIELDS-1, from the constructor, before any route ran');
end;

procedure TTestRawAdaptersContentFields.JsonBodyWithCharsetAndTrailingPercentDoesNotRaise;
begin
  { The reporter's client sent 'application/json; charset=utf-8'. A guard that only
    matched the bare media type would have left him broken. }
  Assert.WillNotRaise(
    procedure
    var
      LRaw: IHorseRawRequest;
      LReq: TInterfacedWebRequest;
    begin
      LRaw := TFakeRawRequest.Create('application/json; charset=utf-8', TRIGGER_JSON);
      LReq := TInterfacedWebRequest.Create(LRaw);
      LReq.Free;
    end,
    Exception,
    'a charset parameter must not change which path the body takes');
end;

procedure TTestRawAdaptersContentFields.FormBodyStillDecodesContentFieldsExactlyOnce;
var
  LRaw: IHorseRawRequest;
  LReq: TInterfacedWebRequest;
begin
  { The other half of the fix: a real form body must still be parsed and decoded.
    %25 is an encoded percent, so the decoded value is '100%' - decoded once, not
    twice (twice would raise on the resulting trailing percent). }
  LRaw := TFakeRawRequest.Create('application/x-www-form-urlencoded', 'v=100%25');
  LReq := TInterfacedWebRequest.Create(LRaw);
  try
    Assert.AreEqual('100%', LReq.ContentFields.Values['v'],
      'a genuine form body must still populate and decode ContentFields');
  finally
    LReq.Free;
  end;
end;

{$IFEND}

initialization
{$IF NOT DEFINED(FPC)}
  TDUnitX.RegisterTestFixture(TTestRawAdaptersContentFields);
{$IFEND}

end.
