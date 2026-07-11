# Multi-Instance

*Read this in [English](./multi-instance.md) or [Português (BR)](./multi-instance.pt-BR.md).*

A arquitetura **Multi-Instance** no Horse permite aos desenvolvedores instanciar, configurar e executar múltiplos servidores HTTP independentes concorrentemente dentro do mesmo processo de aplicação.

Este recurso desacopla as tabelas de roteamento, ganchos do ciclo de vida e middlewares do estado global estático de classe (`class var` singletons) para objetos isolados da classe `THorseInstance`.

---

## 🗺️ Arquitetura Visual

Um único processo de aplicação pode expor múltiplas portas de rede. Cada instância lógica de servidor do Horse é vinculada a uma porta física e resolve de forma isolada e thread-safe as suas próprias rotas:

```mermaid
graph TD
    subgraph Arquitetura Multi-Instance (Ambientes Isolados)
        Port9001[Porta 9001 - API Pública] -->|Resolve para| Instance1[THorseInstance 1]
        Port9002[Porta 9002 - Admin & Métricas] -->|Resolve para| Instance2[THorseInstance 2]

        Instance1 -->|Utiliza| Routes1[Router Tree 1]
        Instance2 -->|Utiliza| Routes2[Router Tree 2]
    end

    subgraph Fachada Legada (Retrocompatibilidade)
        THorse[Fachada Estática THorse] -->|Delega para| DefaultInstance[Instância Padrão]
    end
```

---

## 🚀 Uso Básico

Para inicializar múltiplos servidores lógicos concorrentes sem criar threads manuais (TThread), basta desativar o bloqueio de console configurando `IsConsole := False` e gerenciando o encerramento do console com um `Readln`:

```pascal
program MultiInstanceConsole;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  Horse;

var
  FPublicApi: THorseInstance;
  FAdminPanel: THorseInstance;

begin
  // Desativa o bloqueio interno do console para que os metodos Listen() retornem imediatamente
  IsConsole := False;

  // 1. Configurando a primeira instância (API Pública)
  FPublicApi := THorseInstance.Create;
  FPublicApi.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('Pong da API Pública');
    end);

  // 2. Configurando a segunda instância (Painel Administrativo)
  FAdminPanel := THorseInstance.Create;
  FAdminPanel.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
    begin
      Res.Send('Pong do Painel Admin');
    end);

  // 3. Inicializando a escuta (ambos retornam o controle imediatamente)
  FPublicApi.Listen(9001);
  FAdminPanel.Listen(9002);

  Writeln('Servidores escutando nas portas 9001 e 9002.');
  Writeln('Pressione [ENTER] para sair...');
  Readln;

  // 4. Parando e liberando as instâncias
  FPublicApi.StopListen;
  FAdminPanel.StopListen;

  FPublicApi.Free;
  FAdminPanel.Free;
end.
```

---

## 🧵 Sockets, Bloqueio de Execução & Gerenciamento de Threads

Os provedores físicos de transporte do Horse (Indy, IOCP, HttpSys, Epoll) já são internamente multithreaded. Quando você chama o método `Listen()`, o servidor cria pools de threads em background no sistema operacional para aceitar conexões e tratar requisições de forma assíncrona.

A thread chamadora (ex: Main Thread) só bloqueia dentro do `Listen()` caso a flag global `IsConsole := True` esteja ativa. Este é um mecanismo didático de segurança projetado para evitar que o console do Delphi finalize sua execução de escopo de forma imediata.

Dependendo do seu tipo de projeto, você pode controlar isso de três formas:

### A. Aplicações Não-Console (VCL, LCL, Serviços Windows, Daemons Linux)
Em aplicações com janelas GUI ou rodando como serviços de sistema, `IsConsole` é naturalmente `False`. Assim, chamar `Listen()` nas instâncias **nunca bloqueia** a thread principal, permitindo ativá-las sequencialmente:
```pascal
FInstance1.Listen(9001); // Ativa e retorna imediatamente
FInstance2.Listen(9002); // Ativa e retorna imediatamente
```

### B. Aplicações Console (Desativando o Bloqueio)
Como demonstrado no exemplo de uso básico acima, configurar `IsConsole := False` no escopo inicial do console permite que os métodos `Listen` retornem a execução na mesma hora, deixando o bloqueio e controle de parada para um `Readln` comum na thread principal.

### C. Abordagem de Thread Híbrida (Mantendo o Bloqueio de Console)
Se você preferir manter `IsConsole := True` e usar o bloqueio de conexão nativo do socket para segurar o console aberto, você deve encapsular apenas as instâncias iniciais em threads de background, e rodar a última instância na thread principal (Main Thread):
```pascal
// Executa a primeira instancia em thread separada
TThread.CreateAnonymousThread(
  procedure
  begin
    FPublicApi.Listen(9001);
  end).Start;

// Executa a segunda instancia diretamente na Main Thread (retém o console aberto)
FAdminPanel.Listen(9002);
```

---

## ⚡ Como Funciona a Resolução de Instâncias

Sempre que uma nova requisição TCP/HTTP chega no provedor físico do servidor (ex: Indy, IOCP, Http.sys, Epoll), o framework extrai a porta de destino a partir do cabeçalho da requisição (`Request.ServerPort`).

O módulo de controle central `THorseWebModule` intercepta a requisição e executa as seguintes etapas:

1. Chama a função global thread-safe `GetHorseInstanceByPort(LPort)` para pesquisar se há uma instância ativa de `THorseInstance` registrada para escutar na porta requisitada.
2. Se uma instância correspondente for localizada, o Horse incrementa o contador de requisições ativas daquela instância e executa a árvore de rotas e middlewares registrados **especificamente** nela:
   ```pascal
   LCore := GetHorseInstanceByPort(LPort);
   if LCore <> nil then
     LCore.GetRoutes.Execute(LRequest, LResponse)
   ```
3. Caso contrário, o fluxo é desviado para a árvore de roteamento e middlewares legados e estáticos da fachada principal `THorseCore`.

### 🌐 Compatibilidade de Provedores (Sockets) e Roteadores

A arquitetura Multi-Instance é compatível com os roteadores e provedores físicos do ecossistema do Horse. Porém, dependendo da natureza do transporte de rede, o comportamento físico varia:

#### 1. Roteadores (Radix Router vs. Classic Router)
**Compatibilidade: 100% (Agnóstico)**
O roteamento lógico (tanto o roteador linear padrão quanto o roteador assíncrono baseado em Radix Tree — `HORSE_RADIX_ROUTER`) é totalmente desacoplado da camada física de transporte. Cada instância de `THorseInstance` possui sua própria árvore de rotas isolada em memória.

#### 2. Provedores Físicos (Sockets Standalone vs. Servidores Web Gerenciados)
A tabela a seguir apresenta o comportamento e suporte de escuta simultânea para cada Provedor de transporte:

| Categoria | Provedores | Escuta Portas Físicas Distintas no Mesmo Processo? | Comportamento Arquitetural |
| :--- | :--- | :---: | :--- |
| **Alta Performance / Async** | `CrossSocket`, `mORMot2`, `HttpSys` | ✔️ Sim | Cada `THorseInstance` gerencia seu próprio socket físico isolado no sistema operacional. |
| **Clássicos / Monolíticos** | `Indy` (Console/VCL/Daemon), `fphttpserver` (LCL/Daemon/HTTPApplication) | ✔️ Sim | O servidor físico global compartilha o listener, mas gerencia múltiplos bindings de rede de forma transparente para todas as instâncias lógicas registradas. |
| **Hospedados / Acoplados** | `IIS` (ISAPI), `Apache` (Module), `CGI` / `FastCGI` | Não se aplica | O servidor externo (IIS/Apache) é dono dos sockets de rede físicos e gerencia as portas de entrada, repassando o request de forma lógica ao `THorseWebModule` com o cabeçalho `Request.ServerPort` correto. O Horse faz o roteamento lógico isolado de instâncias perfeitamente. |

---

## ⚙️ Retrocompatibilidade e Estabilidade

Todo o ecossistema existente de middlewares de terceiros e bases de código legadas que utilizam a sintaxe de classe estática `THorse` clássica continuam compilando e funcionando normalmente. Sob o capô, qualquer chamada estática executada na classe `THorse` (como `THorse.Get`, `THorse.Use`, `THorse.Listen`) é redirecionada para a instância default singleton do framework.
