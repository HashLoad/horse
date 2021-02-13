unit Tests.Commons;

interface

uses
  DUnitX.TestFramework, Horse.Commons;

type
  [TestFixture]
  TCommonsTest = class(TObject)
  public
    [Test]
    procedure TestMimeTypesToString;
  end;

implementation

{ TCommonsTest }

procedure TCommonsTest.TestMimeTypesToString;
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
  TDUnitX.RegisterTestFixture(TCommonsTest);

end.
