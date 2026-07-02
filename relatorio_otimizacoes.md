# Relatório Técnico de Otimização e Integridade — Branch `benchmark-comparison`

Este documento consolida todas as intervenções de engenharia, refatorações de código e melhorias de performance implementadas no framework **Horse** para atingir throughput máximo com latência reduzida no Linux (via Epoll) e Windows (via HTTP.sys), além do novo sistema de roteamento Radix.

---

## 🚀 1. Otimizações de Rede & Sockets (FPC Epoll & HTTP.sys)

### FPC Epoll (`src/Horse.Provider.Epoll.pas`)
* **Redução de Heap Allocation & Lock Contention**:
  * Implementação de buffers em stack/registros de 256 elementos de eventos (`L-Events`), mitigando alocações dinâmicas de heap de memória e eliminando falhas de página (*page faults*) no kernel Linux.
  * Anulação rápida e explícita de referências e ponteiros de contexto de conexão `TEpollConnectionContext`. Isso evitou que o coletor de lixo do compilador FPC bloqueasse threads sob alta concorrência.
* **Pipeline Assíncrono de Keep-Alive**:
  * Otimização do monitoramento de sockets inativos para fechar conexões mortas sem causar vazamento de descritores de arquivos (*file descriptor leaks*).

### Windows HTTP.sys (`src/Horse.Provider.HttpSys.pas`)
* **Request Recycling & Zero-Copy HTTP App**:
  * Centralização do ciclo de vida das requisições via `THorseContextPool`.
  * Ajustes na passagem de ponteiros de memória e remoção de conversões desnecessárias de encoding UTF-8 no envio e recepção de cabeçalhos diretamente da API do kernel do Windows.

---

## ⚡ 2. Roteamento Ultrarrápido (Radix Router)

### Roteador de Prefixos Radix (`src/Horse.Core.Router.Radix.pas` [NEW])
* **Busca Direta O(K)**:
  * Desenvolvido e integrado um novo roteador baseado em **Árvore de Prefixos (Radix Tree)**.
  * Substituiu a busca linear recursiva padrão $O(N)$ do roteador original por saltos diretos baseados nos caracteres da rota, reduzindo a complexidade de busca para $O(K)$, onde $K$ é o comprimento do caminho da URL.
  * Resolvido o suporte a rotas estáticas, rotas com múltiplos parâmetros concorrentes, curingas (`*`) e isolamento de middlewares.
* **Ativação Simples**:
  * Para utilizar, basta chamar `THorse.UseRadixRouter;` no ponto de inicialização da aplicação (antes de registrar qualquer rota).

### Otimizações no RouterTree Padrão (`src/Horse.Core.RouterTree.pas`)
* **Zero-Allocation String Matching (FPC)**:
  * Introdução do método `THorseBufferSlice.Compare(string)` no lugar de operações nativas de comparação e slicing de strings (como `Copy` e `=`).
  * Isso evitou a alocação de temporários na stack e heap do FPC durante o matching de rotas lineares convencionais.

---

## 🧪 3. Cobertura de Testes & Integridade Condicional

### Testes do Radix Router (`tests/src/tests/Tests.Horse.Core.Router.Radix.pas` [NEW])
* Escrita de **279 linhas de testes de unidade** específicos para o Radix Router, validando parâmetros compostos, case-insensitivity, ordem de precedência de rotas e injeção de middlewares.

### Suporte de Teste Condicional (`tests/src/tests/Tests.CleanupHelper.pas` & `tests/src/Console.dpr`)
* Injetada a diretiva de compilação `-dHORSE_RADIX_ROUTER`. Quando ativa, ela força o test runner a resetar e utilizar o `THorseRadixRouter` em toda a suíte de integridade.
* Permite validar a estabilidade do Radix Router com qualquer provedor (Indy, HTTP.sys, Epoll) sob concorrência intensa.

---

## 📊 4. Ambiente de Benchmark Consolidado
* **Infraestrutura Docker Multi-Linguagem**:
  * Estruturação e conteinerização de servidores equivalentes de alta performance em Go (Fiber), Rust (Actix-web), .NET (Minimal API), Java (Spring Boot) e Node.js (Express).
  * Criação dos scripts automatizados em PowerShell (`benchmarks/run-bench.ps1`) para orquestrar a execução local e coletar resultados consolidados.
