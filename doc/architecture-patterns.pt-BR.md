# Padrões de Arquitetura & Organização de Projetos

Este guia orienta sobre como estruturar e arquitetar aplicações e APIs Delphi/Lazarus de grande escala utilizando o framework **Horse**, aplicando conceitos de Clean Code, SOLID e MVC.

---

## 1. Estruturação Sugerida de Diretórios

Para projetos corporativos que crescem continuamente, manter todas as rotas e regras no arquivo principal do projeto `.dpr` torna o código ilegível e de difícil manutenção. Recomenda-se dividir a aplicação em camadas lógicas:

```
MeuProjetoHorse/
├── bin/                       # Executáveis compilados
├── src/                       # Código-fonte
│   ├── Controllers/           # Responsável por receber o HTTP Request e retornar o HTTP Response
│   ├── Services/              # Contém as regras de negócio e lógica do sistema
│   ├── Repositories/          # Camada de persistência de dados (queries SQL, FireDAC, etc.)
│   ├── Models/                # Entidades e mapeamento de dados (classes ou registros)
│   └── Providers/             # Configurações de infraestrutura (banco, loggers, etc.)
├── tests/                     # Testes unitários e de integração
└── MeuProjeto.dpr             # Bootstrap e inicialização do servidor
```

---

## 2. O Padrão Controller (Orientado a Objetos)

Evite o uso massivo de métodos anônimos (`procedure begin end`) nas declarações de rota. Prefira criar classes de Controllers dedicadas para cada domínio do seu sistema, expondo métodos normais e fazendo o registro das rotas de forma limpa.

### Exemplo de Controller: `src/Controllers/Controller.Usuarios.pas`

```pascal
unit Controller.Usuarios;

interface

uses
  Horse;

type
  TUsuarioController = class
  private
    class procedure GetUsuarios(Req: THorseRequest; Res: THorseResponse; Next: TProc);
    class procedure GetUsuarioPorId(Req: THorseRequest; Res: THorseResponse; Next: TProc);
  public
    class procedure Registry;
  end;

implementation

uses
  System.SysUtils, System.JSON;

{ TUsuarioController }

class procedure TUsuarioController.Registry;
begin
  // Registra as rotas associadas a este domínio
  THorse.Get('/usuarios', GetUsuarios);
  THorse.Get('/usuarios/:id', GetUsuarioPorId);
end;

class procedure TUsuarioController.GetUsuarios(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  // Lógica para obter múltiplos usuários (repassando para a camada de Service)
  Res.Send('{"usuarios": []}').ContentType('application/json');
end;

class procedure TUsuarioController.GetUsuarioPorId(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LId: string;
begin
  LId := Req.Params.Items['id'];
  Res.Send(Format('{"id": %s, "nome": "Usuario de Exemplo"}', [LId])).ContentType('application/json');
end;

end.
```

### Arquivo DPR Principal (`MeuProjeto.dpr`):
No arquivo principal, basta carregar as configurações globais e invocar o método de registro de cada Controller:

```pascal
program MeuProjeto;

{$APPTYPE CONSOLE}

uses
  Horse,
  Controller.Usuarios in 'src/Controllers/Controller.Usuarios.pas';

begin
  // 1. Middlewares Globais
  THorse.Use(CORS);

  // 2. Registro das Rotas/Controllers
  TUsuarioController.Registry;

  // 3. Inicialização
  THorse.Listen(9000);
end.
```

---

## 3. Injeção de Dependências com escopo de Requisição (`Req.Services`)

O Horse oferece suporte nativo a contêineres de Injeção de Dependência e IoC (Inversão de Controle) thread-safe e no escopo de cada requisição. Isso ajuda a desacoplar completamente o Controller da implementação física de Services ou Repositories.

### Passo A: Definir Contratos e Classes
```pascal
type
  IEmailService = interface
    ['{E8F3C538-4A00-47F1-BEA3-0FE345B50A0E}']
    procedure Enviar(const ADestinatario, AAssunto, ACorpo: string);
  end;

  TEmailService = class(TInterfacedObject, IEmailService)
  public
    procedure Enviar(const ADestinatario, AAssunto, ACorpo: string);
  end;
```

### Passo B: Registrar via Middleware
Registre a dependência no pipeline de execução do Horse de forma direta ou usando fábricas sob demanda (*Lazy Loading*):

```pascal
begin
  THorse.Use(
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      // Lazy Loading: A instância do serviço de e-mail só será criada no momento do Resolve
      Req.Services.AddFactory(IEmailService,
        function: TObject
        begin
          Result := TEmailService.Create;
        end
      );
      Next;
    end
  );
end;
```

### Passo C: Resolver no Controller
O Controller não precisa saber qual classe implementa a interface `IEmailService`, ele apenas a resolve a partir da requisição:

```pascal
class procedure TUsuarioController.GetUsuarios(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LEmailService: IEmailService;
begin
  // Resolve a interface dinamicamente
  LEmailService := Req.Services.Resolve(IEmailService) as IEmailService;
  
  LEmailService.Enviar('cliente@email.com', 'Sua Conta', 'Olá!');
  
  Res.Send('{"status": "E-mail enviado"}');
end;
```

> [!TIP]
> **Autogestão de Memória**: O contêiner do Horse assume a posse dos objetos registrados em `Req.Services.Add` ou criados via `AddFactory`. Eles são destruídos e liberados da memória automaticamente de forma segura assim que a resposta HTTP é enviada, evitando qualquer vazamento de memória.
