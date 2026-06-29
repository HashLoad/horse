# Roadmap & Backlog de Evolução Técnica: Horse Framework

Este documento detalha o planejamento de melhorias arquiteturais de longo prazo e evolução da suíte de testes do framework Horse.

---

## 🗺️ Roadmap de Evolução Arquitetural

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

## 🧪 Backlog de Expansão de Testes

### 1. Testes de Conexões Persistentes (HTTP Keep-Alive Avançado)
* **Descrição:** Criar testes de integração que simulam conexões consecutivas usando a mesma conexão TCP física ativa.
* **Objetivo:** Garantir a estabilidade e conformidade do protocolo HTTP/1.1 em todos os providers sob Keep-Alive de longa duração.

### 2. Testes de Payload Volumoso (Stress de Heap)
* **Descrição:** Criar testes enviando payloads gigantes (20MB+) e monitorando a variável de heap `AllocMemSize`.
* **Objetivo:** Certificar empiricamente que os providers e o Core limpam e desalocam toda a memória consumida sem causar vazamentos residuais.

### 3. Automação de CI/CD Multiplataforma (Linux / FPC)
* **Descrição:** Criar um pipeline de CI/CD (GitHub Actions / GitLab) integrado com contêineres Docker (rodando Linux e FPC).
* **Objetivo:** Executar e compilar a suíte completa de testes no Linux automaticamente a cada commit, validando a estabilidade da unit de alta performance `Horse.Provider.Epoll.pas`.
