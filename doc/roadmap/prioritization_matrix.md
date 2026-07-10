# Tabela de Priorização de Backlog: Horse Framework

Esta tabela classifica as 13 melhorias pendentes do roadmap técnico do Horse com base na relação **Retorno sobre Investimento (R.O.I.)**, calculada dividindo o **Impacto/Ganho (1 a 5)** pelo **Nível de Dificuldade/Esforço (1 a 5)**.

* **Impacto**: 1 (Baixo) a 5 (Altíssimo)
* **Dificuldade/Esforço**: 1 (Fácil/Rápido) a 5 (Complexo/Demorado)
* **Prioridade de Execução (ROI)**: $\text{Impacto} \div \text{Dificuldade}$. Índices mais altos representam *Quick Wins* (ganhos rápidos com menor esforço) e devem ser priorizados primeiro.

---

## 📊 Matriz de Priorização (Classificada por ROI)

| # | Funcionalidade | Categoria | Impacto (1-5) | Dificuldade (1-5) | ROI (Imp/Dif) | Impacto Usuário Final | Classificação / Ação |
|---|---|---|:---:|:---:|:---:|---|---|
| 3 | **Cadeia de Middlewares por Rota** | DX / Legibilidade | 5 | 2 | **2.50** | ➕ **Novo Recurso** (Opcional) | 🟢 **Concluído** (Implementado e Liberado) |
| 5 | **Pipeline Global de Erros (OnError)** | DX / Robustez | 4 | 2 | **2.00** | ➕ **Novo Recurso** (Opcional) | 🟢 **Concluído** (Implementado e Liberado) |
| 12 | **Middleware de Rate Limiting** | Segurança | 4 | 2 | **2.00** | ➕ **Novo Middleware** (Opcional) | 🟢 **Concluído** (Implementado e Liberado) |
| 11 | **Middleware de Compressão (Gzip/Deflate/Brotli)** | Otimização | 4 | 3 | **1.33** | ➕ **Novo Middleware** (Opcional) | 🟢 **Concluído** (Implementado e Liberado) |
| 13 | **Static File Server com Range/Cache** | DX / Recursos | 4 | 3 | **1.33** | ➕ **Novo Middleware** (Opcional) | 🟢 **Concluído** (Implementado e Liberado) |
| 9 | **Ganchos de Ciclo de Vida (Hooks)** | Ecossistema | 4 | 3 | **1.33** | ➕ **Novo Recurso** (Opcional) | 🟢 **Concluído** (Implementado e Liberado) |
| 4 | **Desligamento Suave (Graceful)** | Resiliência | 4 | 3 | **1.33** | ➕ **Novo Recurso** (Opcional) | 🟡 **Em andamento** (Vital para Docker/K8s) |
| 10 | **Injeção de Dependência Contextual** | Arquitetura | 4 | 3 | **1.33** | ➕ **Novo Recurso** (Opcional) | 📅 **Planejar execução** (Gerência de banco) |
| 1 | **Refatoração Multi-Instance** | Arquitetura | 5 | 4 | **1.25** | ⚙️ **Transparente** (Mantém retrocompatibilidade) | 🎯 **Projeto Estratégico** (Exige refatoração global) |
| 6 | **DTO Auto-Binding & Validação** | DX / Produtividade | 5 | 4 | **1.25** | ➕ **Novo Recurso** (Opcional) | 🎯 **Projeto Estratégico** (Uso avançado de RTTI/Atributos) |
| 2 | **Pool de Buffers (MemoryBufferPool)** | Otimização | 4 | 4 | **1.00** | ⚙️ **Transparente** (Performance por baixo dos panos) | 🎯 **Projeto Estratégico** (Exige refatoração nos sockets) |
| 7 | **Ganchos OpenTelemetry/APM** | Observabilidade | 3 | 3 | **1.00** | ⚙️ **Transparente / Opcional** | ⏳ **Segunda prioridade** |
| 8 | **Roteamento Regex e Opcionais** | Recursos | 4 | 5 | **0.80** | ➕ **Novo Recurso** (Opcional) | 🔬 **Alta Complexidade** (Exige reescrever árvore Radix) |

---

## 🎯 Definição dos Tipos de Impacto para o Desenvolvedor

1. ⚙️ **Transparente (Sem Alteração de Código)**:
   * Melhorias feitas debaixo do capô. O desenvolvedor não precisa reescrever nenhuma linha do seu software atual para se beneficiar da melhoria (ex: Pool de Buffers para ganho de RAM, ou a refatoração Multi-Instance que manterá o `THorse` tradicional como Facade).
2. ➕ **Novo Recurso / Middleware (Opcional)**:
   * O desenvolvedor adquire uma nova sintaxe ou biblioteca oficial de prateleira para usar se quiser, sem quebrar o comportamento de rotas e rotinas tradicionais já implantadas.
3. ⚠️ **Quebra de Compatibilidade (Breaking Change)**:
   * *Nenhum* dos itens do roadmap atual causará quebra de compatibilidade. O design está planejado com total foco na manutenção do legado (*backwards compatibility*).
