# Diretrizes de IA para o Projeto Horse

Este documento estabelece as regras de design e desenvolvimento do framework Horse específicas para agentes de IA que atuam neste repositório.

## 🟢 Ciclo de Vida da Requisição (Lifecycle Hooks)
* O Horse possui suporte nativo e thread-safe a ganchos de ciclo de vida (`onRequest`, `preParsing`, `preValidation`, `onSend` e `onResponse`) em cascata cooperativa (CPS).
* Ao criar novos middlewares ou funcionalidades de tratamento, utilize a infraestrutura de ganchos em vez de interceptores ad-hoc nas rotas para manter a conformidade arquitetural.

## 🟢 Desligamento Suave (Graceful Shutdown)
* O Horse gerencia o escoamento lógico de requisições ativas através das propriedades de telemetria `ActiveRequests` e `IsShuttingDown` no core (`THorseCore`).
* Para encerramentos coordenados e seguros (probes de saúde no Kubernetes/Load Balancers), use sempre `THorse.StopListenGraceful(TimeoutMS)`.
* As propriedades `ActiveRequests` e `IsShuttingDown` estão expostas estaticamente no facade `THorse` e devem ser usadas em endpoints de `/health` ou monitoramento de observabilidade APM.

## 🧪 Padrões de Testes e Concorrência
* Ao escrever testes de integração que envolvam o encerramento do servidor ou simulação de tráfego, utilize sempre a biblioteca HTTP nativa do Delphi (`System.Net.HttpClient` e `System.Net.URLClient`) para garantir compatibilidade multiplataforma nativa no FPC (Lazarus/Linux) sem depender de pacotes externos.
* Em testes de shutdown ou concorrência física, utilize o cabeçalho `Connection: close` na requisição do cliente HTTP para forçar a liberação imediata do socket no sistema operacional, evitando travamento de pools de conexão físicos.
