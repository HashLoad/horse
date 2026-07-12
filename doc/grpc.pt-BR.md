# Provedor gRPC e HTTP/2 (Nativo)

O Horse possui suporte nativo a gRPC e transporte HTTP/2 sem TLS (h2c) através do `THorseGrpcProvider`. Ele permite criar APIs gRPC Code-First, gerando o arquivo `.proto` de forma automática a partir dos seus contratos de serviço Delphi, ou implementar rotas HTTP/2 de altíssima performance.

---

## 🚀 Como Funciona

O provedor realiza toda a decodificação de frames HTTP/2 (HEADERS e DATA), processa a serialização de mensagens baseadas no protocolo Protobuf por meio de RTTI híbrida rápida e atende a requisições gRPC de forma concorrente em threads físicas de socket TCP de forma 100% nativa (sem dependência de DLLs externas).

---

## 📝 Definição do Serviço (Code-First)

Para declarar o seu serviço, crie uma unit com o esquema de mensagens e interfaces anotadas com os atributos do Horse gRPC. 

> [!IMPORTANT]
> A unit de mensagens **deve ter a diretiva `{$M+}` habilitada** e todas as propriedades que representam campos das mensagens devem ser declaradas na seção `published` para garantir a geração precisa de offsets RTTI clássicos e evitar Access Violations.

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

## 🛠️ Implementação da Classe de Serviço

Ao implementar sua interface de serviço gRPC, desative a contagem de referências do Delphi (ARC) na classe implementadora. Isso é necessário para que a RTTI interna não libere acidentalmente o objeto de serviço no meio de chamadas de reflexão dinâmica.

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
  Result.name := 'João Silva';
  Result.email := 'joao.silva@example.com';
end;
```

---

## 🌐 Inicialização do Servidor e Exportação de Schema

O gRPC Provider permite exportar o arquivo `.proto` correspondente ao seu código Delphi de forma automática durante a inicialização, garantindo a interoperabilidade com qualquer linguagem (Node.js, Go, Python, C#, etc.).

```delphi
uses
  Horse,
  Horse.Provider.Grpc,
  users;

var
  ServiceImpl: TUserServiceImpl;
begin
  // Registrar as instâncias de serviços
  ServiceImpl := TUserServiceImpl.Create;
  THorseGrpcProvider.RegisterService(IUserService, ServiceImpl);

  // Opcional: Exportar o arquivo proto para consumo do cliente gRPC externo
  THorseGrpcProvider.ExportProto('users.proto');

  WriteLn('Iniciando servidor gRPC na porta 9090...');
  THorseGrpcProvider.Listen(9090);
end.
```
