unit Tests.Horse.Core.Grpc;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Horse.Core.Protobuf.Serializer,
  Horse.Core.Protobuf.Rtti,
  Horse.Grpc.Codec;

type
  [TestFixture]
  TTestsHorseCoreGrpc = class
  public
    [Test]
    procedure TestVarintEncodingDecoding;
    [Test]
    procedure TestProtobufWriterAndReader;
    [Test]
    procedure TestGrpcLpmFraming;
  end;

implementation

{ TTestsHorseCoreGrpc }

procedure TTestsHorseCoreGrpc.TestVarintEncodingDecoding;
var
  Stream: TBytesStream;
  Writer: TProtobufWriter;
  Reader: TProtobufReader;
  Val: Integer;
begin
  Stream := TBytesStream.Create;
  Writer := nil;
  Reader := nil;
  try
    Writer := TProtobufWriter.Create(Stream);
    Writer.WriteInt32(1, 150);
    
    Stream.Position := 0;
    Reader := TProtobufReader.Create(Stream);
    
    Assert.IsTrue(Reader.ReadField);
    Assert.AreEqual(1, Reader.Tag);
    Val := Reader.ReadInt32;
    Assert.AreEqual(150, Val);
  finally
    Reader.Free;
    Writer.Free;
    Stream.Free;
  end;
end;

procedure TTestsHorseCoreGrpc.TestProtobufWriterAndReader;
var
  Stream: TBytesStream;
  Writer: TProtobufWriter;
  Reader: TProtobufReader;
  ValStr: string;
begin
  Stream := TBytesStream.Create;
  Writer := nil;
  Reader := nil;
  try
    Writer := TProtobufWriter.Create(Stream);
    Writer.WriteString(2, 'Test gRPC Native');
    
    Stream.Position := 0;
    Reader := TProtobufReader.Create(Stream);
    
    Assert.IsTrue(Reader.ReadField);
    Assert.AreEqual(2, Reader.Tag);
    ValStr := Reader.ReadString;
    Assert.AreEqual('Test gRPC Native', ValStr);
  finally
    Reader.Free;
    Writer.Free;
    Stream.Free;
  end;
end;

procedure TTestsHorseCoreGrpc.TestGrpcLpmFraming;
var
  Payload: TBytes;
  Framed: TBytes;
  OutPayload: TBytes;
  Compressed: Boolean;
  Offset: Integer;
begin
  SetLength(Payload, 4);
  Payload[0] := 1;
  Payload[1] := 2;
  Payload[2] := 3;
  Payload[3] := 4;
  
  Framed := THorseGrpcMessageCodec.Encode(Payload);
  Assert.AreEqual(9, Length(Framed)); // 5 bytes header + 4 bytes payload
  Assert.AreEqual(0, Integer(Framed[0])); // compression flag = 0
  
  Offset := 0;
  Assert.IsTrue(THorseGrpcMessageCodec.TryDecode(Framed, Offset, Compressed, OutPayload));
  Assert.IsFalse(Compressed);
  Assert.AreEqual(4, Length(OutPayload));
  Assert.AreEqual(1, Integer(OutPayload[0]));
  Assert.AreEqual(4, Integer(OutPayload[3]));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestsHorseCoreGrpc);

end.
