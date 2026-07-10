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

Para inicializar múltiplos servidores lógicos concorrentes, basta criar instâncias de `THorseInstance` e configurar suas respectivas rotas:

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

  // 3. Inicializando a escuta em threads separadas
  TThread.CreateAnonymousThread(
    procedure
    begin
      FPublicApi.Listen(9001);
    end).Start;

  TThread.CreateAnonymousThread(
    procedure
    begin
      FAdminPanel.Listen(9002);
    end).Start;

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

---

## ⚙️ Retrocompatibilidade e Estabilidade

Todo o ecossistema existente de middlewares de terceiros e bases de código legadas que utilizam a sintaxe de classe estática `THorse` clássica continuam compilando e funcionando normalmente. Sob o capô, qualquer chamada estática executada na classe `THorse` (como `THorse.Get`, `THorse.Use`, `THorse.Listen`) é redirecionada para a instância default singleton do framework.
