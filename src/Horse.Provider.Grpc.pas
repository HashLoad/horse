unit Horse.Provider.Grpc;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs,
  {$IFNDEF FPC}
  System.Rtti,
  {$ELSE}
  Rtti,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    {$IFNDEF FPC}
    Winapi.WinSock2, Winapi.Windows,
    {$ELSE}
    WinSock2, Windows,
    {$ENDIF}
  {$ELSE}
    {$IFDEF FPC}
    Sockets, BaseUnix,
    {$ELSE}
    Posix.SysSocket, Posix.Unistd, Posix.NetinetIn, Posix.ArpaInet, Posix.SysSelect, Posix.SysTime,
    {$ENDIF}
  {$ENDIF}
  Horse.Grpc.Attributes,
  Horse.Core.Protobuf.Rtti,
  Horse.Core.Protobuf.Serializer,
  Horse.Grpc.Codec,
  Horse.Core.Http2.Framing,
  Horse.Core.Http2.Hpack,
  Horse.Core.Http2.Stream,
  Horse.Core.Http2.Connection,
  Horse.Core.BufferPool,
  Horse.Core.Channel;

type
  {$IFDEF MSWINDOWS}
  THorseSocket = TSocket;
    {$IFNDEF FPC}
    TFDSet = Winapi.WinSock2.TFDSet;
    TTimeVal = Winapi.WinSock2.TTimeVal;
    {$ELSE}
    TFDSet = WinSock2.TFDSet;
    TTimeVal = WinSock2.TTimeVal;
    {$ENDIF}
  {$ELSE}
  THorseSocket = Integer;
    {$IFNDEF FPC}
    TFDSet = Posix.SysSelect.fd_set;
    TTimeVal = Posix.SysTime.timeval;
    {$ENDIF}
  {$ENDIF}

  TGrpcMethodMeta = record
    MethodName: string;
    RttiMethod: TRttiMethod;
    RequestClass: TClass;
    ResponseClass: TClass;
  end;

  TGrpcServiceMeta = class
  public
    ServiceName: string;
    InterfaceGUID: TGUID;
    ServiceImplClass: TClass;
    ServiceInstance: TObject;
    IsSingleton: Boolean;
    Methods: TDictionary<string, TGrpcMethodMeta>;
    constructor Create;
    destructor Destroy; override;
  end;

  THorseGrpcConnectionThread = class(TThread)
  private
    FSocket: THorseSocket;
    FHttp2Conn: THorseHttp2Connection;
    procedure OnOutput(AData: PByte; ALen: Integer);
    procedure OnRequest(AConnection: TObject; AStreamId: Cardinal;
      const AHeaders: TNameValuePairs; const ABody: TBytes);
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: THorseSocket; const AConnectionOptions: THorseHttp2ConnectionOptions);
    destructor Destroy; override;
  end;

  THorseGrpcProvider = class
  private
    class var FServices: TDictionary<string, TGrpcServiceMeta>;
    class var FActive: Boolean;
    class var FPort: Integer;
    class var FListenSocket: THorseSocket;
    class var FListenThread: TThread;
    class var FConnectionQueue: TQueue<THorseSocket>;
    class var FQueueSection: TCriticalSection;
    class var FThreadPool: TList<TThread>;
    class procedure OnHttp2Request(AConnection: TObject; AStreamId: Cardinal;
      const AHeaders: TNameValuePairs; const ABody: TBytes); static;
    class procedure ListenLoop; static;
  public
    class constructor CreateClass;
    class destructor DestroyClass;
    class procedure RegisterService(const AInterface: TGUID; const AServiceImplClass: TClass); overload; static;
    class procedure RegisterService(const AInterface: TGUID; const AServiceInstance: TObject); overload; static;
    class procedure Start(APort: Integer = 9090); static;
    class procedure Stop; static;
    class function ExportProto: string; static;
    class property Active: Boolean read FActive;
    class property Port: Integer read FPort;
    class property Services: TDictionary<string, TGrpcServiceMeta> read FServices;
  end;

const
  {$IFDEF MSWINDOWS}
  InvalidSocketValue = INVALID_SOCKET;
  {$ELSE}
  InvalidSocketValue = -1;
  {$ENDIF}

function SocketRead(ASocket: THorseSocket; ABuffer: Pointer; ALen: Integer): Integer;
function SocketWrite(ASocket: THorseSocket; ABuffer: Pointer; ALen: Integer): Integer;
procedure CloseSocketHandle(ASocket: THorseSocket);
function SocketBind(ASocket: THorseSocket; const APort: Integer): Boolean;
function SocketAccept(ASocket: THorseSocket; out AClientSocket: THorseSocket): Boolean;
function SocketReadyToAccept(ASocket: THorseSocket; ATimeoutMS: Integer): Boolean;
function SocketReadyToRead(ASocket: THorseSocket; ATimeoutMS: Integer): Boolean;
procedure FDSetZero(var AFDSet: TFDSet);
procedure FDSetAdd(ASocket: THorseSocket; var AFDSet: TFDSet);

implementation

function SocketReadyToAccept(ASocket: THorseSocket; ATimeoutMS: Integer): Boolean;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
begin
  FDSetZero(FDSet);
  FDSetAdd(ASocket, FDSet);
  TimeVal.tv_sec := ATimeoutMS div 1000;
  TimeVal.tv_usec := (ATimeoutMS mod 1000) * 1000;
  
  {$IFDEF MSWINDOWS}
  Result := select(0, @FDSet, nil, nil, @TimeVal) > 0;
  {$ELSE}
  Result := select(ASocket + 1, @FDSet, nil, nil, @TimeVal) > 0;
  {$ENDIF}
end;

function SocketReadyToRead(ASocket: THorseSocket; ATimeoutMS: Integer): Boolean;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
begin
  FDSetZero(FDSet);
  FDSetAdd(ASocket, FDSet);
  TimeVal.tv_sec := ATimeoutMS div 1000;
  TimeVal.tv_usec := (ATimeoutMS mod 1000) * 1000;
  
  {$IFDEF MSWINDOWS}
  Result := select(0, @FDSet, nil, nil, @TimeVal) > 0;
  {$ELSE}
  Result := select(ASocket + 1, @FDSet, nil, nil, @TimeVal) > 0;
  {$ENDIF}
end;

procedure FDSetZero(var AFDSet: TFDSet);
begin
  {$IFDEF MSWINDOWS}
  AFDSet.fd_count := 0;
  {$ELSE}
    {$IFDEF FPC}
    FD_ZERO(AFDSet);
    {$ELSE}
    Posix.SysSelect.__FD_ZERO(AFDSet);
    {$ENDIF}
  {$ENDIF}
end;

procedure FDSetAdd(ASocket: THorseSocket; var AFDSet: TFDSet);
begin
  {$IFDEF MSWINDOWS}
  AFDSet.fd_array[AFDSet.fd_count] := ASocket;
  Inc(AFDSet.fd_count);
  {$ELSE}
    {$IFDEF FPC}
    FD_SET(ASocket, AFDSet);
    {$ELSE}
    Posix.SysSelect.__FD_SET(ASocket, AFDSet);
    {$ENDIF}
  {$ENDIF}
end;

function SocketRead(ASocket: THorseSocket; ABuffer: Pointer; ALen: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := recv(ASocket, ABuffer^, ALen, 0);
  {$ELSE}
    {$IFDEF FPC}
    Result := fpRecv(ASocket, ABuffer, ALen, 0);
    {$ELSE}
    Result := recv(ASocket, ABuffer^, ALen, 0);
    {$ENDIF}
  {$ENDIF}
end;

function SocketWrite(ASocket: THorseSocket; ABuffer: Pointer; ALen: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := send(ASocket, ABuffer^, ALen, 0);
  {$ELSE}
    {$IFDEF FPC}
    Result := fpSend(ASocket, ABuffer, ALen, 0);
    {$ELSE}
    Result := send(ASocket, ABuffer^, ALen, 0);
    {$ENDIF}
  {$ENDIF}
end;

procedure CloseSocketHandle(ASocket: THorseSocket);
begin
  {$IFDEF MSWINDOWS}
  closesocket(ASocket);
  {$ELSE}
    {$IFDEF FPC}
    CloseSocket(ASocket);
    {$ELSE}
    Posix.Unistd.__close(ASocket);
    {$ENDIF}
  {$ENDIF}
end;

function SocketBind(ASocket: THorseSocket; const APort: Integer): Boolean;
var
  ServerAddr: sockaddr_in;
begin
  FillChar(ServerAddr, SizeOf(ServerAddr), 0);
  ServerAddr.sin_family := AF_INET;
  ServerAddr.sin_port := htons(APort);
  ServerAddr.sin_addr.s_addr := INADDR_ANY;

  {$IFDEF MSWINDOWS}
    {$IFNDEF FPC}
    Result := bind(ASocket, TSockAddr(ServerAddr), SizeOf(ServerAddr)) = 0;
    {$ELSE}
    Result := WinSock2.bind(ASocket, @ServerAddr, SizeOf(ServerAddr)) = 0;
    {$ENDIF}
  {$ELSE}
    {$IFDEF FPC}
    Result := fpBind(ASocket, @ServerAddr, SizeOf(ServerAddr)) = 0;
    {$ELSE}
    Result := bind(ASocket, Posix.SysSocket.sockaddr(ServerAddr), SizeOf(ServerAddr)) = 0;
    {$ENDIF}
  {$ENDIF}
end;

function SocketAccept(ASocket: THorseSocket; out AClientSocket: THorseSocket): Boolean;
var
  Addr: sockaddr_in;
  {$IFDEF MSWINDOWS}
  AddrLen: Integer;
  {$ELSE}
    {$IFDEF FPC}
    AddrLen: Integer;
    {$ELSE}
    AddrLen: socklen_t;
    {$ENDIF}
  {$ENDIF}
begin
  AddrLen := SizeOf(Addr);
  {$IFDEF MSWINDOWS}
    {$IFNDEF FPC}
    AClientSocket := accept(ASocket, PSockAddr(@Addr), @AddrLen);
    {$ELSE}
    AClientSocket := WinSock2.accept(ASocket, @Addr, @AddrLen);
    {$ENDIF}
    Result := AClientSocket <> INVALID_SOCKET;
  {$ELSE}
    {$IFDEF FPC}
    AClientSocket := fpAccept(ASocket, @Addr, @AddrLen);
    {$ELSE}
    AClientSocket := accept(ASocket, Posix.SysSocket.sockaddr(Addr), AddrLen);
    {$ENDIF}
    Result := AClientSocket <> -1;
  {$ENDIF}
end;

{ TGrpcServiceMeta }

constructor TGrpcServiceMeta.Create;
begin
  inherited Create;
  Methods := TDictionary<string, TGrpcMethodMeta>.Create;
  ServiceInstance := nil;
  IsSingleton := False;
end;

destructor TGrpcServiceMeta.Destroy;
begin
  Methods.Free;
  if IsSingleton and Assigned(ServiceInstance) then
    ServiceInstance.Free;
  inherited;
end;

{ THorseGrpcConnectionThread }

constructor THorseGrpcConnectionThread.Create(ASocket: THorseSocket;
  const AConnectionOptions: THorseHttp2ConnectionOptions);
begin
  inherited Create(True);
  FSocket := ASocket;
  FHttp2Conn := nil;
  FreeOnTerminate := False;
end;

destructor THorseGrpcConnectionThread.Destroy;
begin
  if Assigned(FHttp2Conn) then
    FHttp2Conn.Free;
  inherited;
end;

procedure THorseGrpcConnectionThread.OnRequest(AConnection: TObject; AStreamId: Cardinal;
  const AHeaders: TNameValuePairs; const ABody: TBytes);
begin
  THorseGrpcProvider.OnHttp2Request(AConnection, AStreamId, AHeaders, ABody);
end;

procedure THorseGrpcConnectionThread.OnOutput(AData: PByte; ALen: Integer);
begin
  try
    SocketWrite(FSocket, AData, ALen);
  except
    on E: Exception do
      WriteLn('OnOutput Exception: ', E.Message);
  end;
end;

procedure THorseGrpcConnectionThread.Execute;
var
  LBuffer: TBytes;
  BytesRead: Integer;
  LSocket: THorseSocket;
begin
  LBuffer := THorseBufferPool.Acquire;
  try
    while not Terminated do
    begin
      LSocket := InvalidSocketValue;
      THorseGrpcProvider.FQueueSection.Enter;
      try
        if THorseGrpcProvider.FConnectionQueue.Count > 0 then
          LSocket := THorseGrpcProvider.FConnectionQueue.Dequeue;
      finally
        THorseGrpcProvider.FQueueSection.Leave;
      end;

      if LSocket = InvalidSocketValue then
      begin
        Sleep(5);
        Continue;
      end;

      FSocket := LSocket;
      try
        FHttp2Conn := THorseHttp2Connection.Create(THorseHttp2ConnectionOptions.Default);
        FHttp2Conn.OnOutput := OnOutput;
        FHttp2Conn.OnRequest := OnRequest;
        try
          while not Terminated and (FHttp2Conn.State <> THorseHttp2ConnectionState.csClosed) do
          begin
            BytesRead := SocketRead(FSocket, @LBuffer[0], Length(LBuffer));
            if BytesRead <= 0 then
              Break;
            FHttp2Conn.Feed(@LBuffer[0], BytesRead);
          end;
        finally
          FHttp2Conn.Free;
          FHttp2Conn := nil;
        end;
      finally
        CloseSocketHandle(FSocket);
        FSocket := InvalidSocketValue;
      end;
    end;
  finally
    THorseBufferPool.Release(LBuffer);
  end;
end;

{ THorseGrpcProvider }

class constructor THorseGrpcProvider.CreateClass;
begin
  FServices := TDictionary<string, TGrpcServiceMeta>.Create;
  FActive := False;
  FPort := 0;
  FListenSocket := InvalidSocketValue;
end;

class destructor THorseGrpcProvider.DestroyClass;
var
  Meta: TGrpcServiceMeta;
begin
  for Meta in FServices.Values do
    Meta.Free;
  FServices.Free;
end;

class procedure THorseGrpcProvider.RegisterService(const AInterface: TGUID;
  const AServiceImplClass: TClass);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  IntfType: TRttiInterfaceType;
  Attr: TCustomAttribute;
  ServiceName: string;
  ServiceMeta: TGrpcServiceMeta;
  Method: TRttiMethod;
  MethodMeta: TGrpcMethodMeta;
  Params: TArray<TRttiParameter>;
  T: TRttiType;
begin
  Context := TRttiContext.Create;
  try
    IntfType := nil;
    for T in Context.GetTypes do
    begin
      if (T.TypeKind = tkInterface) and (TRttiInterfaceType(T).GUID = AInterface) then
      begin
        IntfType := TRttiInterfaceType(T);
        Break;
      end;
    end;

    if not Assigned(IntfType) then
      raise Exception.Create('Interface not found in RTTI. Make sure to define it with GUID and {$M+} enabled.');

    ServiceName := '';
    for Attr in IntfType.GetAttributes do
    begin
      if Attr is GrpcServiceAttribute then
      begin
        ServiceName := GrpcServiceAttribute(Attr).ServiceName;
        Break;
      end;
    end;

    if ServiceName = '' then
      ServiceName := IntfType.Name;

    ServiceMeta := TGrpcServiceMeta.Create;
    ServiceMeta.ServiceName := ServiceName;
    ServiceMeta.InterfaceGUID := AInterface;
    ServiceMeta.ServiceImplClass := AServiceImplClass;

    RttiType := Context.GetType(AServiceImplClass);
    if Assigned(RttiType) then
    begin
      for Method in IntfType.GetMethods do
      begin
        for Attr in Method.GetAttributes do
        begin
          if Attr is GrpcMethodAttribute then
          begin
            Params := Method.GetParameters;
            if Length(Params) = 1 then
            begin
              MethodMeta.MethodName := GrpcMethodAttribute(Attr).GrpcMethodName;
              MethodMeta.RttiMethod := RttiType.GetMethod(Method.Name);
              if not Assigned(MethodMeta.RttiMethod) then
                raise Exception.CreateFmt('Method "%s" has no RTTI on implementer class "%s". Ensure it is public/published and compiled with RTTI enabled.', [Method.Name, AServiceImplClass.ClassName]);
              
              if Params[0].ParamType is TRttiInstanceType then
                MethodMeta.RequestClass := TRttiInstanceType(Params[0].ParamType).MetaclassType
              else
                raise Exception.CreateFmt('Request parameter type "%s" is not a class in RTTI. Make sure it is registered/used.', [Params[0].ParamType.Name]);
                 
              if Method.ReturnType is TRttiInstanceType then
                MethodMeta.ResponseClass := TRttiInstanceType(Method.ReturnType).MetaclassType
              else
                raise Exception.CreateFmt('Response return type "%s" is not a class in RTTI. Make sure it is registered/used.', [Method.ReturnType.Name]);

              ServiceMeta.Methods.Add(MethodMeta.MethodName.ToLower, MethodMeta);
            end;
            Break;
          end;
        end;
      end;
    end;

    FServices.Add(ServiceName.ToLower, ServiceMeta);
  finally
    Context.Free;
  end;
end;

class procedure THorseGrpcProvider.RegisterService(const AInterface: TGUID;
  const AServiceInstance: TObject);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  IntfType: TRttiInterfaceType;
  Attr: TCustomAttribute;
  ServiceName: string;
  ServiceMeta: TGrpcServiceMeta;
  Method: TRttiMethod;
  MethodMeta: TGrpcMethodMeta;
  Params: TArray<TRttiParameter>;
  T: TRttiType;
begin
  Context := TRttiContext.Create;
  try
    IntfType := nil;
    for T in Context.GetTypes do
    begin
      if (T.TypeKind = tkInterface) and (TRttiInterfaceType(T).GUID = AInterface) then
      begin
        IntfType := TRttiInterfaceType(T);
        Break;
      end;
    end;

    if not Assigned(IntfType) then
      raise Exception.Create('Interface not found in RTTI. Make sure to define it with GUID and {$M+} enabled.');

    ServiceName := '';
    for Attr in IntfType.GetAttributes do
    begin
      if Attr is GrpcServiceAttribute then
      begin
        ServiceName := GrpcServiceAttribute(Attr).ServiceName;
        Break;
      end;
    end;

    if ServiceName = '' then
      ServiceName := IntfType.Name;

    ServiceMeta := TGrpcServiceMeta.Create;
    ServiceMeta.ServiceName := ServiceName;
    ServiceMeta.InterfaceGUID := AInterface;
    ServiceMeta.ServiceImplClass := AServiceInstance.ClassType;
    ServiceMeta.ServiceInstance := AServiceInstance;
    ServiceMeta.IsSingleton := True;

    RttiType := Context.GetType(AServiceInstance.ClassType);
    if Assigned(RttiType) then
    begin
      for Method in IntfType.GetMethods do
      begin
        for Attr in Method.GetAttributes do
        begin
          if Attr is GrpcMethodAttribute then
          begin
            Params := Method.GetParameters;
            if Length(Params) = 1 then
            begin
              MethodMeta.MethodName := GrpcMethodAttribute(Attr).GrpcMethodName;
              MethodMeta.RttiMethod := RttiType.GetMethod(Method.Name);
              if not Assigned(MethodMeta.RttiMethod) then
                raise Exception.CreateFmt('Method "%s" has no RTTI on implementer class "%s". Ensure it is public/published and compiled with RTTI enabled.', [Method.Name, AServiceInstance.ClassName]);
              
              if Params[0].ParamType is TRttiInstanceType then
                MethodMeta.RequestClass := TRttiInstanceType(Params[0].ParamType).MetaclassType
              else
                raise Exception.CreateFmt('Request parameter type "%s" is not a class in RTTI. Make sure it is registered/used.', [Params[0].ParamType.Name]);
                 
              if Method.ReturnType is TRttiInstanceType then
                MethodMeta.ResponseClass := TRttiInstanceType(Method.ReturnType).MetaclassType
              else
                raise Exception.CreateFmt('Response return type "%s" is not a class in RTTI. Make sure it is registered/used.', [Method.ReturnType.Name]);

              ServiceMeta.Methods.Add(MethodMeta.MethodName.ToLower, MethodMeta);
            end;
            Break;
          end;
        end;
      end;
    end;

    FServices.Add(ServiceName.ToLower, ServiceMeta);
  finally
    Context.Free;
  end;
end;

class procedure THorseGrpcProvider.Start(APort: Integer);
var
  {$IFDEF MSWINDOWS}
  WData: TWSAData;
  {$ENDIF}
  Res: Integer;
  LNumThreads: Integer;
  LThread: TThread;
  i: Integer;
begin
  if FActive then Exit;
  FPort := APort;

  {$IFDEF MSWINDOWS}
  Res := WSAStartup($202, WData);
  if Res <> 0 then
    raise Exception.Create('WSAStartup failed');
  {$ENDIF}

  FListenSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FListenSocket = InvalidSocketValue then
    raise Exception.Create('Failed to create listen socket');

  if not SocketBind(FListenSocket, APort) then
  begin
    CloseSocketHandle(FListenSocket);
    raise Exception.CreateFmt('Failed to bind to port %d', [APort]);
  end;

  Res := listen(FListenSocket, SOMAXCONN);
  if Res = -1 then
  begin
    CloseSocketHandle(FListenSocket);
    raise Exception.Create('Failed to listen on socket');
  end;

  FConnectionQueue := TQueue<THorseSocket>.Create;
  FQueueSection := TCriticalSection.Create;
  FThreadPool := TList<TThread>.Create;

  FActive := True;

  LNumThreads := CPUCount * 4;
  if LNumThreads < 16 then
    LNumThreads := 16;

  for i := 1 to LNumThreads do
  begin
    LThread := THorseGrpcConnectionThread.Create(InvalidSocketValue, THorseHttp2ConnectionOptions.Default);
    LThread.Start;
    FThreadPool.Add(LThread);
  end;

  FListenThread := TThread.CreateAnonymousThread(
    procedure
    begin
      ListenLoop;
    end);
  FListenThread.FreeOnTerminate := False;
  FListenThread.Start;
end;

class procedure THorseGrpcProvider.Stop;
var
  LThread: TThread;
begin
  if not FActive then Exit;
  FActive := False;
  CloseSocketHandle(FListenSocket);

  if Assigned(FThreadPool) then
  begin
    for LThread in FThreadPool do
      LThread.Terminate;
    for LThread in FThreadPool do
    begin
      LThread.WaitFor;
      LThread.Free;
    end;
    FreeAndNil(FThreadPool);
  end;

  if Assigned(FConnectionQueue) then
  begin
    FQueueSection.Enter;
    try
      while FConnectionQueue.Count > 0 do
        CloseSocketHandle(FConnectionQueue.Dequeue);
    finally
      FQueueSection.Leave;
    end;
    FreeAndNil(FConnectionQueue);
  end;
  FreeAndNil(FQueueSection);

  if Assigned(FListenThread) then
  begin
    FListenThread.WaitFor;
    FListenThread.Free;
    FListenThread := nil;
  end;

  {$IFDEF MSWINDOWS}
  WSACleanup;
  {$ENDIF}
end;

class procedure THorseGrpcProvider.ListenLoop;
var
  ClientSocket: THorseSocket;
begin
  while FActive do
  begin
    if SocketReadyToAccept(FListenSocket, 50) then
    begin
      if SocketAccept(FListenSocket, ClientSocket) then
      begin
        FQueueSection.Enter;
        try
          FConnectionQueue.Enqueue(ClientSocket);
        finally
          FQueueSection.Leave;
        end;
      end;
    end;
  end;
end;

class procedure THorseGrpcProvider.OnHttp2Request(AConnection: TObject; AStreamId: Cardinal;
  const AHeaders: TNameValuePairs; const ABody: TBytes);
var
  Path: string;
  Parts: TArray<string>;
  ServiceName: string;
  MethodName: string;
  Svc: TGrpcServiceMeta;
  Method: TGrpcMethodMeta;
  Offset: Integer;
  Compressed: Boolean;
  MsgBytes: TBytes;
  ReqObj: TObject;
  ReqVal: TValue;
  ResObj: TObject;
  ResVal: TValue;
  ServiceInstance: TObject;
  Serialized: TBytes;
  Framed: TBytes;
  ResHeaders: TNameValuePairs;
  Trailers: TNameValuePairs;
  Http2Conn: THorseHttp2Connection;
  LChannel: THorseChannel;
  LItem: TObject;
begin
  Http2Conn := THorseHttp2Connection(AConnection);

  Path := '';
  for Offset := 0 to High(AHeaders) do
  begin
    if AHeaders[Offset].Name = ':path' then
    begin
      Path := AHeaders[Offset].Value;
      Break;
    end;
  end;

  Parts := Path.Split(['/']);
  if Length(Parts) < 3 then
  begin
    SetLength(ResHeaders, 2);
    ResHeaders[0].Name := ':status'; ResHeaders[0].Value := '200';
    ResHeaders[1].Name := 'content-type'; ResHeaders[1].Value := 'application/grpc';

    SetLength(Trailers, 2);
    Trailers[0].Name := 'grpc-status'; Trailers[0].Value := '12';
    Trailers[1].Name := 'grpc-message'; Trailers[1].Value := 'Unimplemented';

    Http2Conn.SendResponse(AStreamId, ResHeaders, nil, False);
    Http2Conn.SendResponse(AStreamId, Trailers, nil, True);
    Exit;
  end;

  ServiceName := Parts[1].ToLower;
  MethodName := Parts[2].ToLower;

  if FServices.TryGetValue(ServiceName, Svc) and Svc.Methods.TryGetValue(MethodName, Method) then
  begin
    Offset := 0;
    if THorseGrpcMessageCodec.TryDecode(ABody, Offset, Compressed, MsgBytes) then
    begin
      ReqObj := Method.RequestClass.Create;
      try
        THorseProtobufSerializer.Deserialize(MsgBytes, ReqObj);

        if Svc.IsSingleton then
          ServiceInstance := Svc.ServiceInstance
        else
          ServiceInstance := Svc.ServiceImplClass.Create;
        try
          TValue.Make(@ReqObj, Method.RequestClass.ClassInfo, ReqVal);
          ResVal := Method.RttiMethod.Invoke(ServiceInstance, [ReqVal]);
          ResObj := ResVal.AsObject;
          try
            SetLength(ResHeaders, 3);
            ResHeaders[0].Name := ':status'; ResHeaders[0].Value := '200';
            ResHeaders[1].Name := 'content-type'; ResHeaders[1].Value := 'application/grpc';
            ResHeaders[2].Name := 'grpc-accept-encoding'; ResHeaders[2].Value := 'identity';

            SetLength(Trailers, 2);
            Trailers[0].Name := 'grpc-status'; Trailers[0].Value := '0';
            Trailers[1].Name := 'grpc-message'; Trailers[1].Value := '';

            if (ResObj <> nil) and (ResObj is THorseChannel) then
            begin
              Http2Conn.SendResponse(AStreamId, ResHeaders, nil, False);
              LChannel := THorseChannel(ResObj);
              try
                while LChannel.ReadObject(LItem) do
                begin
                  try
                    Serialized := THorseProtobufSerializer.Serialize(LItem);
                    Framed := THorseGrpcMessageCodec.Encode(Serialized);
                    Http2Conn.SendResponse(AStreamId, nil, Framed, False);
                  finally
                    LItem.Free;
                  end;
                end;
              finally
                LChannel.Free;
              end;
              Http2Conn.SendResponse(AStreamId, Trailers, nil, True);
            end
            else
            begin
              Serialized := THorseProtobufSerializer.Serialize(ResObj);
              Framed := THorseGrpcMessageCodec.Encode(Serialized);
              Http2Conn.SendResponse(AStreamId, ResHeaders, Framed, False);
              Http2Conn.SendResponse(AStreamId, Trailers, nil, True);
            end;
          finally
            if (ResObj <> nil) and (not (ResObj is THorseChannel)) then
              ResObj.Free;
          end;
        finally
          if not Svc.IsSingleton then
            ServiceInstance.Free;
        end;
      finally
        ReqObj.Free;
      end;
    end;
  end
  else
  begin
    SetLength(ResHeaders, 2);
    ResHeaders[0].Name := ':status'; ResHeaders[0].Value := '200';
    ResHeaders[1].Name := 'content-type'; ResHeaders[1].Value := 'application/grpc';

    SetLength(Trailers, 2);
    Trailers[0].Name := 'grpc-status'; Trailers[0].Value := '12';
    Trailers[1].Name := 'grpc-message'; Trailers[1].Value := 'Service or Method Unimplemented';

    Http2Conn.SendResponse(AStreamId, ResHeaders, nil, False);
    Http2Conn.SendResponse(AStreamId, Trailers, nil, True);
  end;
end;

class function THorseGrpcProvider.ExportProto: string;
var
  Sb: TStringBuilder;
  Svc: TGrpcServiceMeta;
  Mtd: TGrpcMethodMeta;
  Props: TArray<THorseProtobufProp>;
  Prop: THorseProtobufProp;
  MsgClasses: TList<TClass>;
  C: TClass;

  procedure AddMessage(AClass: TClass);
  begin
    if MsgClasses.Contains(AClass) then Exit;
    MsgClasses.Add(AClass);
  end;

  function MapDelphiTypeToProto(hpt: THorsePropType; APropType: TRttiType): string;
  begin
    case hpt of
      hptInt32: Result := 'int32';
      hptInt64: Result := 'int64';
      hptDouble: Result := 'double';
      hptSingle: Result := 'float';
      hptString: Result := 'string';
      hptBool: Result := 'bool';
      hptBytes: Result := 'bytes';
      hptMessage:
        begin
          if APropType is TRttiInstanceType then
            Result := TRttiInstanceType(APropType).MetaclassType.ClassName.Substring(1)
          else
            Result := APropType.Name;
        end;
      else Result := 'string';
    end;
  end;

begin
  Sb := TStringBuilder.Create;
  MsgClasses := TList<TClass>.Create;
  try
    Sb.AppendLine('syntax = "proto3";');
    Sb.AppendLine;

    for Svc in FServices.Values do
    begin
      for Mtd in Svc.Methods.Values do
      begin
        AddMessage(Mtd.RequestClass);
        AddMessage(Mtd.ResponseClass);
      end;
    end;

    for Svc in FServices.Values do
    begin
      Sb.AppendLine('service ' + Svc.ServiceName + ' {');
      for Mtd in Svc.Methods.Values do
      begin
        Sb.AppendLine(Format('  rpc %s (%s) returns (%s);', [
          Mtd.MethodName,
          Mtd.RequestClass.ClassName.Substring(1),
          Mtd.ResponseClass.ClassName.Substring(1)
        ]));
      end;
      Sb.AppendLine('}');
      Sb.AppendLine;
    end;

    for C in MsgClasses do
    begin
      Sb.AppendLine('message ' + C.ClassName.Substring(1) + ' {');
      Props := THorseProtobufRtti.GetProperties(C);
      for Prop in Props do
      begin
        Sb.AppendLine(Format('  %s %s = %d;', [
          MapDelphiTypeToProto(Prop.PropType, Prop.RttiType),
          Prop.Name.ToLower,
          Prop.Tag
        ]));
      end;
      Sb.AppendLine('}');
      Sb.AppendLine;
    end;

    Result := Sb.ToString;
  finally
    MsgClasses.Free;
    Sb.Free;
  end;
end;

end.
