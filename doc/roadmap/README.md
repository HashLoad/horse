# Roadmap & Backlog de Evolução Técnica: Horse Framework

Este documento detalha o planejamento de melhorias arquiteturais de longo prazo no framework Horse.

> [!TIP]
> A matriz detalhada de priorização com cálculo de R.O.I. (Impacto vs. Dificuldade) para todos os itens pendentes está disponível em: [prioritization_matrix.md](prioritization_matrix.md).

## 🗺️ Roadmap de Evolução Arquitetural (Pendente)

### 2. Centralização de Pool de Buffers de Memória (`IMemoryBufferPool`)
* **Descrição:** Unificar a alocação e reciclagem de streams e buffers de leitura/escrita no Core do framework.
* **Ganhos:**
  * Redução geral de alocação de memória no heap (*zero-allocation response mapping*).
  * Todos os providers do ecossistema (incluindo Indy legado) passariam a se beneficiar de envio zero-copy automaticamente, sem precisar reimplementar essa lógica individualmente.


### 6. DTO Auto-Binding e Validação Declarativa
* **Descrição:** Desserialização e validação automáticas de dados de requisição (Body/Query) para objetos Delphi de transferência (DTOs) com uso de Atributos customizados.
* **Ganhos:**
  * Grande ganho em DX (*Developer Experience*), reduzindo boilerplate nos controllers.

### 7. Ganchos de Telemetria Padronizados (Observabilidade / OpenTelemetry)
* **Descrição:** Disponibilizar ganchos internos no Core para extração de latência, volumetria de requests e status HTTP sem perdas de performance.
* **Ganhos:**
  * Integração nativa facilitada com coletores de métricas do ecossistema APM (como Prometheus e Jaeger).

### 8. Roteamento Avançado (Regex e Parâmetros Opcionais)
* **Descrição:** Permitir parâmetros opcionais (`/users/:id?`) e restrições de rotas baseadas em Expressões Regulares (`/users/:id(\d+)`) na árvore do Radix Router.


## ✅ Evolução Arquitetural Entregue (Concluído)

### 1. Cadeias de Middlewares por Rota (*Route-level Middleware Chains*)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Permitida a declaração de múltiplos middlewares locais de rotas via Open Arrays (`array of THorseCallback`) em formato estático e fluente. Compatibilidade total de retrocompatibilidade e compilação multiplataforma.

### 2. Pipeline Global de Tratamento de Erros (*Error Handler Pipeline*)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Disponibilizado o método global `THorse.OnError(...)` que permite interceptar todas as exceções não tratadas ocorridas no ciclo de vida das requisições (middlewares globais, grupos ou handlers de rota). Totalmente integrado de forma segura (fail-safe) e com suporte a compiladores XE7+ e Lazarus/FPC.

### 3. Middleware de Limitação de Requisições (*Rate Limiting*)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Disponibilizada a biblioteca externa oficial e integrada de limitação e controle de tráfego baseada em chaves (como IP de origem ou Tokens) em memória ou integrada ([horse-rate-limit](https://github.com/regyssilveira/horse-rate-limit)), fornecendo proteção contra força bruta e sobrecargas no servidor de forma compatível com Delphi e Lazarus.

### 4. Middleware de Compressão de Resposta (Gzip/Deflate/Brotli)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Desenvolvido o middleware de compressão de alta performance e *zero-allocation*, com suporte a Gzip, Deflate e Brotli, disponível no repositório de comunidade [horse-compression-v2](https://github.com/regyssilveira/horse-compression-v2).

### 5. Middleware de Servidor de Arquivos Estáticos (Static File Streaming)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Desenvolvido o middleware de alta performance e thread-safe para servir arquivos estáticos locais de forma totalmente provider-agnostic, com suporte a HTTP 206 (Range/Partial Content) e controle de cache por ETag fraca e Last-Modified ([horse-static](https://github.com/regyssilveira/horse-static)).

### 6. Ganchos de Ciclo de Vida da Requisição (Lifecycle Hooks)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Adicionado suporte nativo e thread-safe a ganchos de ciclo de vida (`onRequest`, `preParsing`, `preValidation`, `onSend` e `onResponse`) em cascata cooperativa (CPS) no Core e integrado a ambos os roteadores (`THorseRouterTree` e `THorseRadixRouter`), com testes de integração e exemplos documentados.

### 7. Desligamento Suave (Graceful Shutdown)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Desenvolvido o mecanismo de encerramento coordenado no Core e Provedor de Console (Indy), permitindo interromper novas escutas físicas do socket enquanto as requisições ativas (`ActiveRequests`) são concluídas de forma suave sob um timeout de segurança, expondo as propriedades de telemetria `ActiveRequests` e `IsShuttingDown` (sinalização para Kubernetes/Load Balancers).

### 8. Injeção de Dependência Contextual (Request Scope)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Desenvolvida a propriedade de ciclo de vida `Services` na classe `THorseRequest`, provendo um container de inversão de controle (IoC) thread-safe que permite injeção direta de instâncias e carregamento preguiçoso (lazy loading) via fábricas com descarte físico e destruição automáticos e determinísticos ao término do pipeline HTTP da requisição ativa.

### 9. Refatoração Multi-Instance (`THorseInstance`)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Desacoplado o estado estático global de roteamento, ganchos de ciclo de vida e middlewares para objetos de instância independentes de `THorseInstance`. Adicionado suporte à escuta simultânea de múltiplos servidores HTTP concorrentes em portas diferentes de forma thread-safe e com 100% de retrocompatibilidade mantendo a fachada `THorse` clássica.

### 10. Ganchos de Ciclo de Vida do Servidor (*Server Lifecycle Hooks*)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Implementado suporte nativo a ganchos de ciclo de vida do servidor físico (`BeforeListen`, `AfterListen`, `BeforeStop`, `AfterStop`) de forma thread-safe tanto para o modo Multi-Instance (`THorseInstance`) quanto para o facade clássico (`THorse`). Garantido alinhamento polimórfico de portas de escuta e prevenção de travamentos não interativos de console, com testes de integração e concorrência 100% livres de memory leak e Access Violations.

---

## ✅ Entregas Recentes de Testes & CI/CD (Concluído)

### 1. Testes de Conexões Persistentes (HTTP Keep-Alive)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Desenvolvida a unit `Tests.Integration.KeepAlive.pas` validando a persistência e a conformidade dos cabeçalhos do protocolo HTTP/1.1 sob requisições sucessivas sobre o mesmo socket de conexão física.

### 2. Testes de Payload Volumoso (Stress de Heap)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Desenvolvida a unit `Tests.Integration.LargePayload.pas` que realiza uploads de volumes pesados de dados (10MB) monitorando a variável de heap `AllocMemSize` para atestar a liberação e limpeza total de memória após o ciclo de vida.

### 3. Automação de CI/CD Multiplataforma Real (Linux / FPC / GitHub Actions)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Refatorado o arquivo `.github/workflows/tests.yml` do GitHub Actions para compilar de forma dinâmica a suíte de testes de console a partir das fontes usando o compilador FPC (Free Pascal Compiler) e executar em ambiente Linux (Ubuntu) real a cada push e pull request.

### 4. Correção e Unificação dos Provedores Assíncronos (epoll, IOCP e HttpSys)
* **Status:** 🟢 **Concluído e Liberado**
* **Implementação:** Padronizada a extração de cabeçalhos (`PopulateHeaders`) e o mapeamento de streams/textos de resposta no core e nos adaptadores assíncronos (`TInterfacedWebRequest` / `TInterfacedWebResponse`). Garantida compatibilidade completa entre Delphi (10 Seattle até 13 Florence) e Lazarus/FPC com testes automatizados rodando sob o loop `epoll` no Linux e todas as variações no Windows.
