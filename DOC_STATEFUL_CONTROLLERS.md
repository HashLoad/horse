# Statefull Controllers no Horse: Arquitetura e Implementação

O Horse agora suporta o padrão **Statefull Controller**, permitindo que as rotas sejam gerenciadas por classes. Esta abordagem facilita a organização do código, a injeção de dependências e o isolamento de estado por requisição.

A implementação foi desenhada para ser fluida em ambas as IDEs, respeitando as particularidades de cada compilador.

---

## 1. O Conceito Base

Diferente do modelo funcional, o modelo de classes instancia um objeto único para cada requisição HTTP. A infraestrutura do Horse garante o ciclo de vida completo: **Instanciar -> Mapear Rota -> Executar Método -> Destruir**.

### Benefícios
- **Isolamento de Estado:** Cada requisição possui sua própria instância da classe (Thread-Safe).
- **Gerenciamento de Memória Determinístico:** Utiliza o padrão `try..finally` internamente na base.
- **Roteamento Dinâmico:** Uma única classe gerencia múltiplos endpoints relacionados.

---

## 2. Como Utilizar: Exemplo Embarcadero Delphi

No Delphi, utilizamos as units padrão do framework e a RTTI nativa.

### Controller (UController.pas)
```pascal
unit UController;

interface

uses Horse, Horse.Controller, System.SysUtils, System.JSON;

type
  {$M+} // Ativa a RTTI para os métodos published
  TUserController = class(THorseController)
  published
    procedure ListUsers;
    procedure GetUserById;
  end;
  {$M-}

implementation

procedure TUserController.ListUsers;
var
  LJSON: TJSONArray;
begin
  LJSON := TJSONArray.Create;
  LJSON.Add('Lucas');
  LJSON.Add('Horse');
  Response.Send(LJSON).Status(200);
end;

procedure TUserController.GetUserById;
begin
  Response.Send('ID: ' + Request.Params['id']).Status(200);
end;

end.
```

### Projeto (DelphiProject.dpr)
```pascal
program DelphiProject;

{$APPTYPE CONSOLE}

uses Horse, Horse.Controller, Horse.Commons, UController;

begin
  // Mapeamento e Registro Automático
  THorseController<TUserController>.Map('/users', mtGet, 'ListUsers');
  THorseController<TUserController>.Map('/users/:id', mtGet, 'GetUserById');

  THorse.Listen(9000);
end.
```

---

## 3. Como Utilizar: Exemplo Lazarus (FreePascal)

No Lazarus, é fundamental o uso da diretiva `{$MODE DELPHI}` e o tratamento correto das units de JSON do FPC.

### Controller (ucontroller.pas)
```pascal
unit ucontroller;

{$mode delphi}{$H+}

interface

uses Horse, Horse.Controller, SysUtils, fpjson;

type
  {$M+} // Ativa a RTTI para os métodos published no FPC
  TUserController = class(THorseController)
  published
    procedure ListUsers;
    procedure GetUserById;
  end;
  {$M-}

implementation

procedure TUserController.ListUsers;
var
  LJSON: TJSONArray;
begin
  LJSON := TJSONArray.Create;
  LJSON.Add('Lazarus User');
  LJSON.Add('FPC Power');
  Response.Send(LJSON.AsJSON).Status(200);
end;

procedure TUserController.GetUserById;
begin
  Response.Send('FPC ID: ' + Request.Params['id']).Status(200);
end;

end.
```

### Projeto (LazarusProject.lpr)

**Nota de Configuração:** Para que o compilador encontre as units do Horse, você deve adicionar o caminho para a pasta `src` do Horse nas configurações do seu projeto. Vá em **Project > Project Options > Compiler Options > Paths > Other unit files (-Fu)** e adicione o caminho.

```pascal
program LazarusProject;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // Essencial para aplicações console multithread no Linux/macOS
  {$ENDIF}
  uses Horse, Horse.Controller, Horse.Commons, ucontroller;

begin
  // Mapeamento e Registro Automático (Sintaxe idêntica ao Delphi)
  THorseController<TUserController>.Map('/users', mtGet, 'ListUsers');
  THorseController<TUserController>.Map('/users/:id', mtGet, 'GetUserById');

  THorse.Listen(9000);
end.
```

---

## 4. Principais Diferenças de Implementação

| Recurso | Delphi | Lazarus (FPC) |
| :--- | :--- | :--- |
| **Diretiva de Linguagem** | Padrão da IDE. | Requer `{$MODE DELPHI}` no topo das units. |
| **JSON** | `System.JSON` (TJSONObject, TJSONArray). | `fpjson` (TJSONObject, TJSONArray). |
| **RTTI (`$M+`)** | Nativa e habilitada por padrão em muitas classes. | Obrigatória para expor métodos ao `MethodAddress`. |
| **Gerenciamento de Strings** | Unicode por padrão. | Requer `{$H+}` para AnsiString/Unicode moderno. |

---

## 5. Melhorias na Base do Horse

1.  **Unit `Horse.Controller.pas`**: Centraliza a lógica de ciclo de vida.
2.  **Dispatcher por `MethodAddress`**: Utiliza a localização física do método na memória para execução, garantindo alta performance sem depender de frameworks de RTTI pesados.
3.  **Auto-Registro**: O método `Map` injeta a rota diretamente no `THorse`, validando o verbo HTTP.
