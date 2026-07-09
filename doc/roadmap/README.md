# Roadmap & Backlog de Evolução Técnica: Horse Framework

Este documento detalha o planejamento de melhorias arquiteturais de longo prazo no framework Horse.

> [!TIP]
> A matriz detalhada de priorização com cálculo de R.O.I. (Impacto vs. Dificuldade) para todos os itens pendentes está disponível em: [prioritization_matrix.md](prioritization_matrix.md).

---

## 🗺️ Roadmap de Evolução Arquitetural (Pendente)

### 1. Refatoração Multi-Instance (`THorseInstance`)
* **Descrição:** Desacoplar o estado estático global de classe (`FRoutes`, `FCallbacks`, `FPort`, `FHost`) movendo-o para uma classe de instância chamada `THorseInstance`.
* **Ganhos:**
  * **Múltiplos Servidores no Mesmo Processo:** Possibilidade de rodar servidores de forma paralela (ex: um na porta `80` para a API de produção e outro na `8080` para métricas, telemetria e admin).
  * **Isolamento de Middlewares:** Aplicação de middlewares pesados (como CORS ou JWT) apenas nas instâncias que de fato necessitam, mantendo outras instâncias ultraleves.
  * **Testes Concorrentes Limpos:** Execução paralela da suíte de testes sem conflitos de estado no singleton global.
* **Compatibilidade:** O `THorse` tradicional baseado em métodos estáticos de classe deve continuar existindo como um wrapper (*Facade*) apontando para uma instância default, garantindo **100% de retrocompatibilidade** com projetos e middlewares existentes.

### 2. Centralização de Pool de Buffers de Memória (`IMemoryBufferPool`)
* **Descrição:** Unificar a alocação e reciclagem de streams e buffers de leitura/escrita no Core do framework.
* **Ganhos:**
  * Redução geral de alocação de memória no heap (*zero-allocation response mapping*).
  * Todos os providers do ecossistema (incluindo Indy legado) passariam a se beneficiar de envio zero-copy automaticamente, sem precisar reimplementar essa lógica individualmente.

### 3. Cadeias de Middlewares por Rota (*Route-level Middleware Chains*)
* **Descrição:** Permitir declarar arrays de middlewares específicos diretamente na definição de um endpoint ou grupo de rotas.
* **Ganhos:**
  * Elimina a necessidade de registrar middlewares globais ou criar múltiplos controllers.
  * Possibilita isolar autenticações (JWT/BasicAuth) a nível de rota com Fluent API: `THorse.Get('/secure', [Auth, Logger], Handler)`.

### 4. Desligamento Suave (*Graceful Shutdown*)
* **Descrição:** Implementar encerramento coordenado de conexões. O servidor encerra a escuta de novos sockets mas conclui requisições ativas dentro de um tempo limite de segurança.
* **Ganhos:**
  * Resiliência a nível enterprise para orquestradores de contêiner (como Docker/Kubernetes).
  * Evita a corrupção de dados ou interrupção de transações em andamento ao atualizar serviços.

### 5. Pipeline Global de Tratamento de Erros (*Error Handler Pipeline*)
* **Descrição:** Oferecer um manipulador centralizado de exceções não tratadas disparadas durante o ciclo de vida das requisições.
* **Ganhos:**
  * Unificação de logs e formatação de respostas JSON padronizadas de erro (ex: `THorse.OnError(ErrorHandler)`).

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

### 9. Ganchos de Ciclo de Vida da Requisição (*Lifecycle Hooks*)
* **Descrição:** Oferecer eventos padronizados ao longo do pipeline (como `onRequest`, `preParsing`, `preValidation`, `onSend`, `onResponse`) inspirados no Fastify.
* **Ganhos:**
  * Permite que middlewares implementem caching inteligente, auditoria detalhada de payloads ou criptografia em tempo de tráfego sem acoplamento.

### 10. Injeção de Dependência Contextual (*Request Scope / Context*)
* **Descrição:** Prover um mecanismo estruturado para gerência de dependências cujo ciclo de vida está acoplado ao ciclo da requisição (ex: uma transação de banco de dados ou conexão FireDAC ativa).
* **Ganhos:**
  * Facilidade na gerência de concorrência com encerramento e liberação automática de recursos após o fim da requisição.

### 11. Middleware Oficial de Compressão de Resposta (Gzip/Deflate/Brotli)
* **Descrição:** Middleware oficial de alta performance integrado ao core para compressão dinâmica dos corpos de resposta HTTP.
* **Ganhos:**
  * Otimização de consumo de banda e redução no tempo de transferência de dados sem depender de proxies externos.

### 12. Middleware de Limitação de Requisições (*Rate Limiting*)
* **Descrição:** Solução nativa ou integrada para limitação e controle de tráfego baseado em chaves (como IP de origem ou Tokens) em memória ou integrado com Redis.
* **Ganhos:**
  * Proteção do servidor contra ataques de força bruta, scraping ou sobrecarga.

### 13. Servidor de Arquivos Estáticos Otimizado com Suporte a Range (*Static File Streaming*)
* **Descrição:** Middleware robusto e otimizado para entrega de arquivos físicos locais, incluindo cabeçalhos de controle de cache (`Cache-Control`, `ETags`) e suporte a requisições parciais (HTTP 206 para streaming de mídia).

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
