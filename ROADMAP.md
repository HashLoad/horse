# Roadmap & Backlog de Evolução Técnica: Horse Framework

Este documento detalha o planejamento de melhorias arquiteturais de longo prazo no framework Horse.

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
