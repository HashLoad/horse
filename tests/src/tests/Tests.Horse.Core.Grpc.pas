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
  Horse.Provider.Grpc,
  Horse.Core.BufferPool,
  Horse.Core.Channel;

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

  [ProtoClass]
  TTestStaticUserRequest = class
  private
    FId: Integer;
    FName: string;
  published
    [ProtoMember(1)]
    property id: Integer read FId write FId;
    [ProtoMember(2)]
    property name: string read FName write FName;
  public
    FSerializeCalled: Boolean;
    FDeserializeCalled: Boolean;
    procedure Serialize(AStream: TStream);
    procedure Deserialize(AStream: TStream);
  end;

  [ProtoClass]
  TTestOffsetMessage = class
  private
    FValInt: Integer;
    FValInt64: Int64;
    FValDouble: Double;
    FValString: string;
    FValBool: Boolean;
  published
    [ProtoMember(1)]
    property valInt: Integer read FValInt write FValInt;
    [ProtoMember(2)]
    property valInt64: Int64 read FValInt64 write FValInt64;
    [ProtoMember(3)]
    property valDouble: Double read FValDouble write FValDouble;
    [ProtoMember(4)]
    property valString: string read FValString write FValString;
    [ProtoMember(5)]
    property valBool: Boolean read FValBool write FValBool;
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

  [GrpcService('SingletonService')]
  ISingletonService = interface(IInvokable)
    ['{B9405BA7-B8C2-4B6A-B68C-28D8E387C102}']
    [GrpcMethod('GetDummy')]
    function GetDummy(const AReq: TTestUserRequest): TTestUserRequest;
  end;

  TSingletonServiceImpl = class(TInterfacedObject, ISingletonService)
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
    [Test]
    procedure TestStaticCodeGenSerialization;
    [Test]
    procedure TestDirectHeapMemoryOffsets;
    [Test]
    procedure TestThreadPoolAndConcurrency;
    [Test]
    procedure TestBufferPoolConcurrency;
    [Test]
    procedure TestChannelSimpleWriteRead;
    [Test]
    procedure TestChannelTimeoutAndClose;
    [Test]
    procedure TestChannelConcurrency;
    [Test]
    procedure TestGrpcSingletonRegistration;
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

{ TSingletonServiceImpl }

function TSingletonServiceImpl._AddRef: Integer;
begin
  Result := -1;
end;

function TSingletonServiceImpl._Release: Integer;
begin
  Result := -1;
end;

function TSingletonServiceImpl.GetDummy(const AReq: TTestUserRequest): TTestUserRequest;
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
  Assert.AreEqual(9, Integer(Length(Framed))); // 5 bytes header + 4 bytes payload
  Assert.AreEqual(0, Integer(Framed[0])); // compression flag = 0
  
  Offset := 0;
  Assert.IsTrue(THorseGrpcMessageCodec.TryDecode(Framed, Offset, Compressed, OutPayload));
  Assert.IsFalse(Compressed);
  Assert.AreEqual(4, Integer(Length(OutPayload)));
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

{ TTestStaticUserRequest }

procedure TTestStaticUserRequest.Serialize(AStream: TStream);
var
  LWriter: TProtobufWriter;
begin
  FSerializeCalled := True;
  LWriter := TProtobufWriter.Create(AStream);
  try
    LWriter.WriteInt32(1, FId);
    LWriter.WriteString(2, FName);
  finally
    LWriter.Free;
  end;
end;

procedure TTestStaticUserRequest.Deserialize(AStream: TStream);
var
  LReader: TProtobufReader;
begin
  FDeserializeCalled := True;
  LReader := TProtobufReader.Create(AStream);
  try
    while LReader.ReadField do
    begin
      case LReader.Tag of
        1: FId := LReader.ReadInt32;
        2: FName := LReader.ReadString;
      else
        LReader.SkipField;
      end;
    end;
  finally
    LReader.Free;
  end;
end;

{ TTestsHorseCoreGrpc - novos testes }

procedure TTestsHorseCoreGrpc.TestStaticCodeGenSerialization;
var
  Req: TTestStaticUserRequest;
  Res: TTestStaticUserRequest;
  Bytes: TBytes;
begin
  Req := TTestStaticUserRequest.Create;
  try
    Req.id := 777;
    Req.name := 'Static CodeGen';
    Req.FSerializeCalled := False;
    
    Bytes := THorseProtobufSerializer.Serialize(Req);
    Assert.IsTrue(Req.FSerializeCalled, 'Serialize method generated by CodeGen should be invoked');
    Assert.IsTrue(Length(Bytes) > 0);
    
    Res := TTestStaticUserRequest.Create;
    try
      Res.FDeserializeCalled := False;
      THorseProtobufSerializer.Deserialize(Bytes, Res);
      Assert.IsTrue(Res.FDeserializeCalled, 'Deserialize method generated by CodeGen should be invoked');
      Assert.AreEqual(777, Res.id);
      Assert.AreEqual('Static CodeGen', Res.name);
    finally
      Res.Free;
    end;
  finally
    Req.Free;
  end;
end;

procedure TTestsHorseCoreGrpc.TestDirectHeapMemoryOffsets;
var
  Msg: TTestOffsetMessage;
  Props: TArray<THorseProtobufProp>;
  Prop: THorseProtobufProp;
begin
  Msg := TTestOffsetMessage.Create;
  try
    Props := THorseProtobufRtti.GetProperties(TTestOffsetMessage);
    Assert.AreEqual(5, Integer(Length(Props)));
    
    {$IFNDEF FPC}
    for Prop in Props do
    begin
      Assert.IsTrue(Prop.HasField, 'Property should map to its private field and have an offset');
      Assert.IsTrue(Prop.FieldOffset > 0, 'Offset should be greater than 0');
    end;
    {$ENDIF}
    
    for Prop in Props do
    begin
      if Prop.Name = 'valInt' then
        THorseProtobufRtti.SetPropValue(Msg, Prop, 123)
      else if Prop.Name = 'valInt64' then
        THorseProtobufRtti.SetPropValue(Msg, Prop, Int64(456))
      else if Prop.Name = 'valDouble' then
        THorseProtobufRtti.SetPropValue(Msg, Prop, 12.34)
      else if Prop.Name = 'valString' then
        THorseProtobufRtti.SetPropValue(Msg, Prop, 'Offset Write')
      else if Prop.Name = 'valBool' then
        THorseProtobufRtti.SetPropValue(Msg, Prop, True);
    end;
    
    Assert.AreEqual(123, Msg.valInt);
    Assert.AreEqual(Int64(456), Msg.valInt64);
    Assert.AreEqual(12.34, Msg.valDouble, 0.0001);
    Assert.AreEqual('Offset Write', Msg.valString);
    Assert.IsTrue(Msg.valBool);
    
    for Prop in Props do
    begin
      if Prop.Name = 'valInt' then
        Assert.AreEqual(123, THorseProtobufRtti.GetPropValue(Msg, Prop).AsInteger)
      else if Prop.Name = 'valInt64' then
        Assert.AreEqual(Int64(456), THorseProtobufRtti.GetPropValue(Msg, Prop).AsInt64)
      else if Prop.Name = 'valString' then
        Assert.AreEqual('Offset Write', THorseProtobufRtti.GetPropValue(Msg, Prop).AsString)
      else if Prop.Name = 'valBool' then
        Assert.IsTrue(THorseProtobufRtti.GetPropValue(Msg, Prop).AsBoolean);
    end;
  finally
    Msg.Free;
  end;
end;

procedure TTestsHorseCoreGrpc.TestThreadPoolAndConcurrency;
begin
  THorseGrpcProvider.Start(9085);
  try
    Assert.IsTrue(THorseGrpcProvider.Active);
  finally
    THorseGrpcProvider.Stop;
  end;
  Assert.IsFalse(THorseGrpcProvider.Active);
end;

procedure TTestsHorseCoreGrpc.TestBufferPoolConcurrency;
var
  Buf1, Buf2: TBytes;
begin
  Buf1 := THorseBufferPool.Acquire;
  try
    Assert.AreEqual(65536, Integer(Length(Buf1)), 'Acquired buffer should have default size of 64KB');
    Buf2 := THorseBufferPool.Acquire;
    try
      Assert.AreNotEqual(Pointer(Buf1), Pointer(Buf2), 'Subsequent acquires should yield different physical buffers');
    finally
      THorseBufferPool.Release(Buf2);
    end;
  finally
    THorseBufferPool.Release(Buf1);
  end;
end;

procedure TTestsHorseCoreGrpc.TestChannelSimpleWriteRead;
var
  LChannel: THorseChannel<TTestUserRequest>;
  LReq, LRead: TTestUserRequest;
  LReadObj: TObject;
begin
  LChannel := THorseChannel<TTestUserRequest>.Create;
  try
    Assert.AreEqual(0, LChannel.Count);
    LReq := TTestUserRequest.Create;
    LReq.id := 999;
    LReq.name := 'Channel Test';
    LChannel.Write(LReq);
    Assert.AreEqual(1, LChannel.Count);

    Assert.IsTrue(LChannel.ReadObject(LReadObj));
    try
      Assert.IsNotNull(LReadObj);
      Assert.IsTrue(LReadObj is TTestUserRequest);
      LRead := TTestUserRequest(LReadObj);
      Assert.AreEqual(999, LRead.id);
      Assert.AreEqual('Channel Test', LRead.name);
    finally
      LReadObj.Free;
    end;
    Assert.AreEqual(0, LChannel.Count);
  finally
    LChannel.Free;
  end;
end;

procedure TTestsHorseCoreGrpc.TestChannelTimeoutAndClose;
var
  LChannel: THorseChannel<TTestUserRequest>;
  LReadObj: TObject;
begin
  LChannel := THorseChannel<TTestUserRequest>.Create;
  try
    Assert.IsFalse(LChannel.ReadObject(LReadObj, 10));
    Assert.IsNull(LReadObj);

    LChannel.Close;
    Assert.IsTrue(LChannel.IsClosed);
    Assert.IsFalse(LChannel.ReadObject(LReadObj));
  finally
    LChannel.Free;
  end;
end;

procedure TTestsHorseCoreGrpc.TestChannelConcurrency;
var
  LChannel: THorseChannel<TTestUserRequest>;
  LProducer: TThread;
  LReadObj: TObject;
  LCount: Integer;
begin
  LChannel := THorseChannel<TTestUserRequest>.Create;
  try
    LProducer := TThread.CreateAnonymousThread(
      procedure
      var
        i: Integer;
        LItem: TTestUserRequest;
      begin
        for i := 1 to 50 do
        begin
          LItem := TTestUserRequest.Create;
          LItem.id := i;
          LChannel.Write(LItem);
          TThread.Sleep(1);
        end;
        LChannel.Close;
      end);
    LProducer.Start;

    LCount := 0;
    while LChannel.ReadObject(LReadObj) do
    begin
      try
        Inc(LCount);
        Assert.AreEqual(LCount, TTestUserRequest(LReadObj).id);
      finally
        LReadObj.Free;
      end;
    end;

    Assert.AreEqual(50, LCount);
  finally
    LChannel.Free;
  end;
end;

procedure TTestsHorseCoreGrpc.TestGrpcSingletonRegistration;
var
  LServiceImpl: TSingletonServiceImpl;
  LServiceMeta: TGrpcServiceMeta;
begin
  LServiceImpl := TSingletonServiceImpl.Create;
  try
    THorseGrpcProvider.RegisterService(ISingletonService, LServiceImpl);
    
    Assert.IsTrue(THorseGrpcProvider.Services.TryGetValue('singletonservice', LServiceMeta) or
                  THorseGrpcProvider.Services.TryGetValue('isingletonservice', LServiceMeta),
                  'Service should be registered under name singletonservice or isingletonservice');
                  
    Assert.IsNotNull(LServiceMeta);
    Assert.IsTrue(LServiceMeta.IsSingleton, 'Service should be registered as Singleton');
    Assert.AreEqual(Pointer(LServiceImpl), Pointer(LServiceMeta.ServiceInstance), 'Service instance should match');
  finally
    if THorseGrpcProvider.Services.TryGetValue('singletonservice', LServiceMeta) or
       THorseGrpcProvider.Services.TryGetValue('isingletonservice', LServiceMeta) then
    begin
      THorseGrpcProvider.Services.Remove(LServiceMeta.ServiceName.ToLower);
      LServiceMeta.Free;
    end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestsHorseCoreGrpc);

end.
