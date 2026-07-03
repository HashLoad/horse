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

    [Test]
    [TestCase('Test04', 'BufferSlice Creation and Empty')]
    procedure TestBufferSliceCreation;

    [Test]
    [TestCase('Test05', 'BufferSlice Compare String')]
    procedure TestBufferSliceCompareString;

    [Test]
    [TestCase('Test06', 'BufferSlice Compare Slice')]
    procedure TestBufferSliceCompareSlice;

    [Test]
    [TestCase('Test07', 'BufferSlice SubSlice')]
    procedure TestBufferSliceSubSlice;

    [Test]
    [TestCase('Test08', 'BufferSlice IndexOf')]
    procedure TestBufferSliceIndexOf;
  end;

implementation

uses
  {$IF DEFINED(FPC)}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  Web.HTTPApp,
  Horse.Commons;

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
  Assert.AreEqual('Query', TMethodType.mtQuery.ToString);
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

procedure TTestHorseCommons.TestBufferSliceCreation;
var
  LBuf: TBytes;
  LSlice: THorseBufferSlice;
begin
  LBuf := TEncoding.UTF8.GetBytes('GET /ping HTTP/1.1');
  LSlice := THorseBufferSlice.Create(LBuf, 4, 5);
  
  Assert.IsFalse(LSlice.IsEmpty);
  Assert.AreEqual(4, LSlice.Start);
  Assert.AreEqual(5, LSlice.Len);
  Assert.AreEqual('/ping', LSlice.ToString);
  
  LSlice := THorseBufferSlice.Empty;
  Assert.IsTrue(LSlice.IsEmpty);
  Assert.AreEqual(0, LSlice.Start);
  Assert.AreEqual(0, LSlice.Len);
  Assert.AreEqual('', LSlice.ToString);
end;

procedure TTestHorseCommons.TestBufferSliceCompareString;
var
  LBuf: TBytes;
  LSlice: THorseBufferSlice;
begin
  LBuf := TEncoding.UTF8.GetBytes('GET /ping HTTP/1.1');
  LSlice := THorseBufferSlice.Create(LBuf, 0, 3); // 'GET'
  
  Assert.IsTrue(LSlice.Compare('GET', True));
  Assert.IsTrue(LSlice.Compare('get', True));
  Assert.IsTrue(LSlice.Compare('GET', False));
  Assert.IsFalse(LSlice.Compare('get', False));
  Assert.IsFalse(LSlice.Compare('POST', True));
  Assert.IsFalse(LSlice.Compare('GE', True));
  Assert.IsFalse(LSlice.Compare('GETT', True));
end;

procedure TTestHorseCommons.TestBufferSliceCompareSlice;
var
  LBuf1, LBuf2: TBytes;
  LSlice1, LSlice2, LSlice3: THorseBufferSlice;
begin
  LBuf1 := TEncoding.UTF8.GetBytes('GET /ping HTTP/1.1');
  LBuf2 := TEncoding.UTF8.GetBytes('get /health HTTP/1.1');
  
  LSlice1 := THorseBufferSlice.Create(LBuf1, 0, 3); // 'GET'
  LSlice2 := THorseBufferSlice.Create(LBuf2, 0, 3); // 'get'
  LSlice3 := THorseBufferSlice.Create(LBuf1, 4, 5); // '/ping'
  
  Assert.IsTrue(LSlice1.Compare(LSlice2, True));
  Assert.IsFalse(LSlice1.Compare(LSlice2, False));
  Assert.IsFalse(LSlice1.Compare(LSlice3, True));
end;

procedure TTestHorseCommons.TestBufferSliceSubSlice;
var
  LBuf: TBytes;
  LSlice, LSubSlice: THorseBufferSlice;
begin
  LBuf := TEncoding.UTF8.GetBytes('GET /clientes/123/detalhes HTTP/1.1');
  LSlice := THorseBufferSlice.Create(LBuf, 4, 21); // '/clientes/123/detalhes'
  
  LSubSlice := LSlice.SubSlice(1, 8); // 'clientes'
  Assert.AreEqual('clientes', LSubSlice.ToString);
  
  LSubSlice := LSlice.SubSlice(10, 3); // '123'
  Assert.AreEqual('123', LSubSlice.ToString);
  
  Assert.WillRaise(
    procedure
    begin
      LSlice.SubSlice(10, 20); // Extrapola limites do slice original
    end,
    ERangeError
  );
end;

procedure TTestHorseCommons.TestBufferSliceIndexOf;
var
  LBuf: TBytes;
  LSlice: THorseBufferSlice;
begin
  LBuf := TEncoding.UTF8.GetBytes('GET /clientes/123/detalhes HTTP/1.1');
  LSlice := THorseBufferSlice.Create(LBuf, 4, 21); // '/clientes/123/detalhes'
  
  Assert.AreEqual(0, LSlice.IndexOf(Ord('/')));
  Assert.AreEqual(9, LSlice.IndexOf(Ord('/'), 1));
  Assert.AreEqual(13, LSlice.IndexOf(Ord('/'), 10));
  Assert.AreEqual(-1, LSlice.IndexOf(Ord('?')));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseCommons);

end.
