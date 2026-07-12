# gRPC and HTTP/2 Provider (Native)

Horse includes native support for gRPC and HTTP/2 Cleartext (h2c) transport via `THorseGrpcProvider`. This allows you to build Code-First gRPC APIs, generating the `.proto` file automatically from your Delphi service contracts, or implement high-performance HTTP/2 backends.

---

## 🚀 How it Works

The provider handles the full HTTP/2 frame parsing (HEADERS and DATA), manages Protobuf serialization through custom dynamic RTTI, and executes gRPC requests concurrently on physical TCP socket threads—100% natively without external library dependencies.

---

## 🧬 Internal Architecture (File Structure)

The native gRPC provider infrastructure is split into modular units with isolated responsibilities (SOLID):

* **[Horse.Provider.Grpc.pas](../src/Horse.Provider.Grpc.pas)**: The core provider unit. Manages the HTTP/2 Cleartext (`h2c`) TCP socket, monitors concurrent connections, and dispatches dynamic gRPC calls.
* **[Horse.Grpc.Attributes.pas](../src/Horse.Grpc.Attributes.pas)**: Defines custom metadata attributes (`[GrpcMessage]`, `[GrpcService]`, `[GrpcMethod]`, `[ProtoMember]`) used for decorating Pascal classes and interfaces.
* **[Horse.Grpc.Codec.pas](../src/Horse.Grpc.Codec.pas)**: Implements the gRPC-standard 5-byte LPM (*Length-Prefixed Message*) framing (1 compression flag byte + 4 payload length bytes).
* **[Horse.Core.Protobuf.Serializer.pas](../src/Horse.Core.Protobuf.Serializer.pas)**: Low-level serialization engine responsible for parsing and writing Google Protobuf binary streams.
* **[Horse.Core.Protobuf.Rtti.pas](../src/Horse.Core.Protobuf.Rtti.pas)**: Thread-safe, hybrid RTTI helper abstraction compatible with Delphi and Lazarus for mapping dynamic class fields to binary buffers.
* **[horse-pb-compiler.dpr](../tools/compiler/horse-pb-compiler.dpr)**: Utility CLI compiler tool that parses standard `.proto` schema files and outputs ready-to-use annotated Pascal units.

---

## 🛠️ CLI Compiler (`horse-pb-compiler`)

If you already have a standard gRPC `.proto` schema file and want to automatically generate the corresponding Delphi unit annotated with Horse attributes and interfaces, use the native compiler tool provided in the repository.

### How to Compile the CLI Compiler
Compile the command-line tool using the Delphi `dcc32` compiler (or `fpc` under Lazarus):

```bash
dcc32.exe tools/compiler/horse-pb-compiler.dpr
```
This generates the `horse-pb-compiler.exe` executable (or `horse-pb-compiler` on Linux).

### How to Use the Compiler
To compile a `.proto` schema file (for example, `users.proto`) to a Delphi/Lazarus unit (`users.pas`), run:

```bash
horse-pb-compiler.exe <input.proto> <output.pas>
```

#### Practical Example:
Given the following `users.proto` schema:
```protobuf
syntax = "proto3";
package users;

message UserRequest {
  int32 id = 1;
}

message UserResponse {
  int32 id = 1;
  string name = 2;
  string email = 3;
}

service UserService {
  rpc GetUser (UserRequest) returns (UserResponse);
}
```

By running `horse-pb-compiler.exe users.proto users.pas`, the corresponding Pascal unit will be generated automatically and decorated with all the necessary gRPC attributes required by Horse.

---

## 🖥️ GUI Compiler (`HorsePbCompilerGui`)

For batch compilations containing multiple Protobuf schema files across a directory tree, the repository provides a visual (GUI) VCL utility for Windows.

### How to Compile the GUI Tool
From the repository root, compile the visual utility using Delphi's `dcc32`:

```bash
dcc32.exe -Utools/compiler tools/gui/HorsePbCompilerGui.dpr
```
This generates the `HorsePbCompilerGui.exe` executable inside the `tools/gui/` folder.

### How to Use the GUI
1. Launch `HorsePbCompilerGui.exe`.
2. In the **Diretório dos arquivos .proto** field, select the source folder containing the schemas.
3. In the **Diretório de destino (.pas)** field, select the target folder where the generated Pascal units will be saved.
4. Check **Incluir subpastas de forma recursiva** if you wish to process subdirectories, preserving the original folder structure in the target location.
5. Click **Compilar em Lote**. The log memo will show the real-time conversion status for each file.

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

---

## ⚡ Server Coexistence (Parallel HTTP and gRPC)

One of the greatest benefits of Horse's socket isolation architecture is the ability to run multiple transport providers with different protocols (such as an HTTP/1.1 REST gateway and an internal gRPC microservice) concurrently within the same process.

To achieve this, simply run the traditional HTTP server in a background thread and the gRPC server in the main thread (or vice-versa):

```delphi
uses
  System.Classes,
  System.SysUtils,
  Horse,
  Horse.Provider.Grpc,
  users;

begin
  // 1. Configure traditional REST routes (HTTP/1.1 via Indy)
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  // Initialize the HTTP REST server on port 8080 in background
  TThread.CreateAnonymousThread(
    procedure
    begin
      THorse.Listen(8080);
    end).Start;

  // 2. Register and start the gRPC service (HTTP/2 h2c)
  THorseGrpcProvider.RegisterService(IUserService, TUserServiceImpl.Create);

  WriteLn('HTTP REST Server running on port 8080...');
  WriteLn('gRPC Server running on port 9090...');
  THorseGrpcProvider.Listen(9090);
end.
```

```
