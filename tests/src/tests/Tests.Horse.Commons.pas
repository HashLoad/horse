unit Tests.Horse.Commons;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestHorseCommons = class(TObject)
  public
    [Test]
    [TestCase('Test01', 'MimeTypes to String')]
    procedure TestMimeTypesToString;

    [Test]
    [TestCase('Test02', 'LHSBrackets to String')]
    procedure TestLHSBracketsTypeToString;

    [Test]
    [TestCase('Test03', 'MethodType to String')]
    procedure TestMethodTypeToString;
  end;

implementation

uses
  Horse.Commons,
  Web.HTTPApp;

procedure TTestHorseCommons.TestLHSBracketsTypeToString;
begin
  Assert.AreEqual('[eq]', TLhsBracketsType.Equal.ToString);
  Assert.AreEqual('[ne]', TLhsBracketsType.NotEqual.ToString);
  Assert.AreEqual('[lt]', TLhsBracketsType.LessThan.ToString);
  Assert.AreEqual('[lte]', TLhsBracketsType.LessThanOrEqual.ToString);
  Assert.AreEqual('[gt]', TLhsBracketsType.GreaterThan.ToString);
  Assert.AreEqual('[gte]', TLhsBracketsType.GreaterThanOrEqual.ToString);
  Assert.AreEqual('[range]', TLhsBracketsType.Range.ToString);
  Assert.AreEqual('[like]', TLhsBracketsType.Like.ToString);
  Assert.AreEqual('[contains]', TLhsBracketsType.Contains.ToString);
  Assert.AreEqual('[startsWith]', TLhsBracketsType.StartsWith.ToString);
  Assert.AreEqual('[endsWith]', TLhsBracketsType.EndsWith.ToString);
end;

procedure TTestHorseCommons.TestMethodTypeToString;
begin
  Assert.AreEqual('Any', TMethodType.mtAny.ToString);
  Assert.AreEqual('Get', TMethodType.mtGet.ToString);
  Assert.AreEqual('Put', TMethodType.mtPut.ToString);
  Assert.AreEqual('Post', TMethodType.mtPost.ToString);
  Assert.AreEqual('Head', TMethodType.mtHead.ToString);
  Assert.AreEqual('Delete', TMethodType.mtDelete.ToString);
  Assert.AreEqual('Patch', TMethodType.mtPatch.ToString);
end;

procedure TTestHorseCommons.TestMimeTypesToString;
begin
  Assert.AreEqual('multipart/form-data', TMimeTypes.MultiPartFormData.ToString);
  Assert.AreEqual('application/x-www-form-urlencoded', TMimeTypes.ApplicationXWWWFormURLEncoded.ToString);
  Assert.AreEqual('application/json', TMimeTypes.ApplicationJSON.ToString);
  Assert.AreEqual('application/octet-stream', TMimeTypes.ApplicationOctetStream.ToString);
  Assert.AreEqual('application/xml', TMimeTypes.ApplicationXML.ToString);
  Assert.AreEqual('application/javascript', TMimeTypes.ApplicationJavaScript.ToString);
  Assert.AreEqual('application/pdf', TMimeTypes.ApplicationPDF.ToString);
  Assert.AreEqual('application/typescript', TMimeTypes.ApplicationTypeScript.ToString);
  Assert.AreEqual('application/zip', TMimeTypes.ApplicationZIP.ToString);
  Assert.AreEqual('text/plain', TMimeTypes.TextPlain.ToString);
  Assert.AreEqual('text/css', TMimeTypes.TextCSS.ToString);
  Assert.AreEqual('text/csv', TMimeTypes.TextCSV.ToString);
  Assert.AreEqual('text/html', TMimeTypes.TextHTML.ToString);
  Assert.AreEqual('image/jpeg', TMimeTypes.ImageJPEG.ToString);
  Assert.AreEqual('image/png', TMimeTypes.ImagePNG.ToString);
  Assert.AreEqual('image/gif', TMimeTypes.ImageGIF.ToString);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseCommons);

end.
