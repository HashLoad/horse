unit Tests.Horse.Core.Grpc;

{$M+}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Horse.Core.Protobuf.Serializer,
  Horse.Core.Protobuf.Rtti,
  Horse.Grpc.Codec,
  Horse.Grpc.Attributes,
  Horse.Provider.Grpc;

type
  [ProtoClass]
  TTestUserRequest = class
  private
    FId: Integer;
    FName: string;
  published
    [ProtoMember(1)]
    property id: Integer read FId write FId;
    [ProtoMember(2)]
    property name: string read FName write FName;
  end;

  [GrpcService('DummyService')]
  IDummyService = interface(IInvokable)
    ['{E3A03B57-E9A0-4F8A-A2E4-3A79E228E101}']
    [GrpcMethod('GetDummy')]
    function GetDummy(const AReq: TTestUserRequest): TTestUserRequest;
  end;

  TDummyServiceImpl = class(TInterfacedObject, IDummyService)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function GetDummy(const AReq: TTestUserRequest): TTestUserRequest;
  end;

  [TestFixture]
  TTestsHorseCoreGrpc = class
  public
    [Test]
    procedure TestVarintEncodingDecoding;
    [Test]
    procedure TestProtobufWriterAndReader;
    [Test]
    procedure TestGrpcLpmFraming;
    [Test]
    procedure TestProtobufSerializer;
    [Test]
    procedure TestGrpcRegisterService;
  end;

implementation

{ TDummyServiceImpl }

function TDummyServiceImpl._AddRef: Integer;
begin
  Result := -1;
end;

function TDummyServiceImpl._Release: Integer;
begin
  Result := -1;
end;

function TDummyServiceImpl.GetDummy(const AReq: TTestUserRequest): TTestUserRequest;
begin
  Result := AReq;
end;

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

procedure TTestsHorseCoreGrpc.TestProtobufSerializer;
var
  Req: TTestUserRequest;
  Res: TTestUserRequest;
  Bytes: TBytes;
begin
  Req := TTestUserRequest.Create;
  try
    Req.id := 999;
    Req.name := 'DUnitX Test';
    Bytes := THorseProtobufSerializer.Serialize(Req);
    
    Assert.IsTrue(Length(Bytes) > 0, 'Serialized payload should not be empty');
    
    Res := TTestUserRequest.Create;
    try
      THorseProtobufSerializer.Deserialize(Bytes, Res);
      Assert.AreEqual(999, Res.id, 'Deserialized ID should match');
      Assert.AreEqual('DUnitX Test', Res.name, 'Deserialized name should match');
    finally
      Res.Free;
    end;
  finally
    Req.Free;
  end;
end;

procedure TTestsHorseCoreGrpc.TestGrpcRegisterService;
begin
  THorseGrpcProvider.RegisterService(IDummyService, TDummyServiceImpl);
  Assert.IsTrue(True);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestsHorseCoreGrpc);

end.
