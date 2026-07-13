---
name: horse-grpc
description: Guidelines and workflows for developing and maintaining gRPC services, HTTP/2 h2c transport, and Protobuf serialization within the Horse framework.
---

# Desenvolvimento gRPC no Horse

Este guia estabelece os padrões e instruções recomendadas para projetar, depurar e estender serviços gRPC usando o provedor nativo de transporte HTTP/2 do Horse.

## 🟢 Definição de Mensagens e Serviços

Ao gerar ou criar classes de mensagens a partir de arquivos `.proto` (usando o `horse-pb-compiler`), garanta que:

1. **Habilitação de RTTI (`{$M+}`)**: O arquivo contendo as classes geradas deve ter a diretiva `{$M+}` habilitada para o Delphi e Lazarus.
2. **Propriedades Publicadas**: Todas as propriedades anotadas com `[ProtoMember(Tag)]` devem residir na seção `published` para a geração correta de metadados de offsets.

### Exemplo de Mensagem:
```delphi
unit users;

{$M+}

interface

uses
  System.SysUtils,
  Horse.Grpc.Attributes;

type
  [GrpcMessage]
  TUserRequest = class
  private
    Fid: Integer;
  published
    [ProtoMember(1)]
    property id: Integer read Fid write Fid;
  end;
```

---

## 🟢 Implementação de Serviços gRPC

Ao implementar uma interface de serviço gRPC (ex: `IUserService`), atente-se às seguintes regras de conformidade arquitetural:

1. **Desativação de ARC (Contagem de Referências)**: Sobrescreva os métodos `_AddRef` e `_Release` para retornarem `-1`. Isso previne que a RTTI interna do compilador destrua acidentalmente a instância do objeto durante uma chamada dinâmica ao método `Invoke`.
2. **Retorno Síncrono de Instâncias**: Métodos de serviço devem instanciar e retornar os objetos de Response. O provedor gRPC é responsável por liberá-los após a serialização do frame DATA.

### Exemplo de Serviço:
```delphi
type
  TUserServiceImpl = class(TInterfacedObject, IUserService)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function GetUser(const ARequest: TUserRequest): TUserResponse;
  end;

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
  Result.name := 'Nome do Usuário';
end;
```

---

## 🟢 Boas Práticas e Regras de Segurança

* **RTTI Context Lifetime**: Nunca instancie `TRttiContext.Create` como variável local dentro de rotinas de serialização. Use sempre a instância global `THorseProtobufRtti.FContext` para evitar vazamentos e dangling pointers de propriedades na stack.
* **Passagem de TValue**: Ao chamar `TRttiProperty.SetValue`, ancore temporários de `TValue` em variáveis locais explícitas de pilha (`LVal: TValue`) em vez de passá-los inline para evitar que o compilador otimize e destrua o valor antes do tempo.
* **Resolução Multi-Instance**: Lembre-se de registrar os serviços no gRPC Provider correspondente da instância lógica (seja `THorse` facade global ou instâncias locais `THorseInstance`).
