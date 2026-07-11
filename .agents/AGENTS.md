# Diretrizes de IA para o Projeto Horse

Este documento estabelece as regras de design e desenvolvimento do framework Horse específicas para agentes de IA que atuam neste repositório.

## 🟢 Ciclo de Vida da Requisição (Lifecycle Hooks)
* O Horse possui suporte nativo e thread-safe a ganchos de ciclo de vida (`onRequest`, `preParsing`, `preValidation`, `onSend` e `onResponse`) em cascata cooperativa (CPS).
* Ao criar novos middlewares ou funcionalidades de tratamento, utilize a infraestrutura de ganchos em vez de interceptores ad-hoc nas rotas para manter a conformidade arquitetural.

## 🟢 Ciclo de Vida do Servidor e Desligamento Suave (Server Lifecycle & Graceful Shutdown)
* O Horse gerencia o escoamento lógico de requisições ativas através das propriedades de telemetria `ActiveRequests` e `IsShuttingDown` no core (`THorseCore`) e nas instâncias locais (`THorseInstance`).
* Para encerramentos coordenados e seguros (probes de saúde no Kubernetes/Load Balancers), use sempre `THorse.StopListenGraceful(TimeoutMS)` ou `LInstance.StopListenGraceful(TimeoutMS)`.
* As propriedades `ActiveRequests` e `IsShuttingDown` estão expostas estaticamente no facade `THorse` e devem ser usadas em endpoints de `/health` ou monitoramento de observabilidade APM.
* **Providers Físicos de Rede**: Ao criar ou atualizar qualquer provedor físico de transporte no ecossistema do Horse:
  * Deve-se obrigatoriamente sobrescrever a função `GetActivePort` retornando a porta física ativa (`FPort` ou `0` para acoplados externos como CGI/Apache/ISAPI) para viabilizar a resolução de instâncias no Multi-Instance.
  * Deve-se acionar explicitamente `TriggerBeforeListen` no topo das rotinas de inicialização física (`InternalListen` / `Listen`).
  * Deve-se acionar explicitamente `TriggerBeforeStop` no topo das rotinas de encerramento físico (`InternalStopListen` / `StopListen`).
  * Deve-se sobrescrever e implementar `StopListenGraceful(const ATimeoutMS: Integer)` para suportar o escoamento de requisições ativas daquele provedor físico se o mesmo gerenciar o socket TCP.

## 🟢 Gerenciamento e Injeção de Dependências (Request Scope)
* O Horse expõe a propriedade `Services` na classe de requisição `THorseRequest`, provendo um container IoC local e thread-safe para o escopo do request.
* Para serviços que devem ser destruídos automaticamente ao final da requisição (evitando vazamento de memória), registre-os usando `Req.Services.Add(TClass, Instance)`.
* Para inicialização sob demanda (Lazy Loading), use `Req.Services.AddFactory(TClass, FactoryMethod)`. A instância só será criada no momento da chamada de `Resolve`.
* Para obter um serviço previamente injetado, chame `Req.Services.Resolve(TClass)` e faça a coerção de tipo necessária.
* Nunca instancie dicionários de escopo ou de serviços paralelos dentro das closures de rotas; utilize sempre a infraestrutura nativa do `Services` para garantir o ciclo de vida e thread-safety coordenados pelo framework.

## 🧪 Padrões de Testes e Concorrência
* Ao escrever testes de integração que envolvam o encerramento do servidor ou simulação de tráfego, utilize sempre a biblioteca HTTP nativa do Delphi (`System.Net.HttpClient` e `System.Net.URLClient`) para garantir compatibilidade multiplataforma nativa no FPC (Lazarus/Linux) sem depender de pacotes externos.
* Em testes de shutdown ou concorrência física, utilize o cabeçalho `Connection: close` na requisição do cliente HTTP para forçar a liberação imediata do socket no sistema operacional, evitando travamento de pools de conexão físicos.

## 🟢 Arquitetura Multi-Instance (`THorseInstance`)
* O Horse suporta a execução paralela de múltiplos servidores lógicos e isolados no mesmo processo de aplicação através da classe `THorseInstance`.
* Ao projetar ou atualizar middlewares do ecossistema, garanta que eles não dependam de dados em variáveis globais ou estáticas (`class var` singletons) do core, permitindo que cada instância de `THorseInstance` configure isoladamente suas dependências, rotas e manipuladores.
* Para preservar a compatibilidade de compilação cruzada multiplataforma FPC/Lazarus, evite o uso de closures ou procedimentos anônimos inline (`procedure begin end`) em manipuladores de ciclo de vida e rotas lógicas locais das instâncias do Horse, preferindo procedimentos regulares e delegados de objetos.

