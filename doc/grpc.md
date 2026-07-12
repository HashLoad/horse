# gRPC and HTTP/2 Provider (Native)

Horse includes native support for gRPC and HTTP/2 Cleartext (h2c) transport via `THorseGrpcProvider`. This allows you to build Code-First gRPC APIs, generating the `.proto` file automatically from your Delphi service contracts, or implement high-performance HTTP/2 backends.

---

## 🚀 How it Works

The provider handles the full HTTP/2 frame parsing (HEADERS and DATA), manages Protobuf serialization through custom dynamic RTTI, and executes gRPC requests concurrently on physical TCP socket threads—100% natively without external library dependencies.

---

## 📝 Service Definition (Code-First)

To declare your service, create a unit with the message schema and interfaces decorated with Horse gRPC attributes.

> [!IMPORTANT]
> The message unit **must have the `{$M+}` directive enabled** and all message properties must be declared in the `published` section to ensure correct compiler RTTI offset generation and avoid Access Violations.

```delphi
unit users;

{$M+}

interface

uses
  System.SysUtils,
  Horse.Grpc.Attributes;

type
  TUserRequest = class;
  TUserResponse = class;

  [GrpcMessage]
  TUserRequest = class
  private
    Fid: Integer;
  published
    [ProtoMember(1)]
    property id: Integer read Fid write Fid;
  end;

  [GrpcMessage]
  TUserResponse = class
  private
    Fid: Integer;
    Fname: string;
    Femail: string;
  published
    [ProtoMember(1)]
    property id: Integer read Fid write Fid;
    [ProtoMember(2)]
    property name: string read Fname write Fname;
    [ProtoMember(3)]
    property email: string read Femail write Femail;
  end;

  [GrpcService('users.UserService')]
  IUserService = interface(IInvokable)
    ['{1E4CA3E9-B5E3-4EF1-8B8C-29F869994C47}']
    [GrpcMethod('GetUser')]
    function GetUser(const ARequest: TUserRequest): TUserResponse;
  end;

implementation

end.
```

---

## 🛠️ Service Class Implementation

When implementing your gRPC service interface, disable ARC (automatic reference counting) on the implementing class. This prevents the internal RTTI framework from prematurely releasing the service object during dynamic reflection.

```delphi
type
  TUserServiceImpl = class(TInterfacedObject, IUserService)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function GetUser(const ARequest: TUserRequest): TUserResponse;
  end;

{ TUserServiceImpl }

function TUserServiceImpl._AddRef: Integer;
begin
  Result := -1;
end;

function TUserServiceImpl._Release: Integer;
begin
  Result := -1;
end;

function TUserServiceImpl.GetUser(const ARequest: TUserRequest): TUserResponse;
begin
  Result := TUserResponse.Create;
  Result.id := ARequest.id;
  Result.name := 'John Doe';
  Result.email := 'john.doe@example.com';
end;
```

---

## 🌐 Server Startup & Schema Export

The gRPC Provider allows you to export the corresponding `.proto` schema file automatically on startup, facilitating integration with external clients in other languages (Node.js, Go, Python, C#, etc.).

```delphi
uses
  Horse,
  Horse.Provider.Grpc,
  users;

var
  ServiceImpl: TUserServiceImpl;
begin
  // Register service instances
  ServiceImpl := TUserServiceImpl.Create;
  THorseGrpcProvider.RegisterService(IUserService, ServiceImpl);

  // Optional: Export proto file for external gRPC clients
  THorseGrpcProvider.ExportProto('users.proto');

  WriteLn('Starting gRPC server on port 9090...');
  THorseGrpcProvider.Listen(9090);
end.
```
