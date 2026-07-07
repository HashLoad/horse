# Horse Framework — Agent Skills (Habilidades de IA)

> [!NOTE]
> **Propósito**: Este diretório contém pacotes de instruções otimizados para agentes de IA (como Antigravity, Claude e GitHub Copilot). Enquanto a documentação padrão é voltada para desenvolvedores humanos, estas habilidades ajudam as IAs a entenderem rapidamente e aplicarem padrões corretos, idiomáticos e seguros (evitando vazamentos de memória) ao trabalhar com o framework Horse.

---

## Habilidades Disponíveis

| Nome da Habilidade | Caminho | Quando Carregar |
| :--- | :--- | :--- |
| **horse-app-structure** | [`horse-app-structure/SKILL.md`](./horse-app-structure/SKILL.md) | Configuração de um novo projeto, inicialização do `.dpr` bootstrap, middleware inicial ou escuta de porta. |
| **horse-routing** | [`horse-routing/SKILL.md`](./horse-routing/SKILL.md) | Configuração de métodos HTTP, parâmetros de rota, curingas e grupos de rotas em Controllers. |
| **horse-middlewares** | [`horse-middlewares/SKILL.md`](./horse-middlewares/SKILL.md) | Configuração de middlewares oficiais (Johnson, CORS, basic-auth, compression, logger) e ordem de execução. |
| **horse-request-response** | [`horse-request-response/SKILL.md`](./horse-request-response/SKILL.md) | Leitura do corpo (body), query strings, parâmetros de rota, headers ou envio de respostas e códigos de status. |
| **horse-files-streams** | [`horse-files-streams/SKILL.md`](./horse-files-streams/SKILL.md) | Tratamento de upload de arquivos (multipart formData), downloads e ciclo de vida de streams. |
| **horse-providers** | [`horse-providers/SKILL.md`](./horse-providers/SKILL.md) | Escolha e configuração de adaptadores de servidor (Indy, CGI, ISAPI, Apache, Daemon, HTTP.sys). |
| **horse-writing-middleware** | [`horse-writing-middleware/SKILL.md`](./horse-writing-middleware/SKILL.md) | Implementação de middlewares customizados seguindo a assinatura correta e fluxo com o procedimento Next. |
| **horse-database-pooling** | [`horse-database-pooling/SKILL.md`](./horse-database-pooling/SKILL.md) | Garantia de thread-safety com pool de conexões de banco de dados (FireDAC / UniDAC) e ciclo de vida local de conexões. |
| **horse-security-auth** | [`horse-security-auth/SKILL.md`](./horse-security-auth/SKILL.md) | Proteção de endpoints utilizando JWT ou Basic-Auth e separação de rotas protegidas vs. públicas. |
| **horse-ssl-tls** | [`horse-ssl-tls/SKILL.md`](./horse-ssl-tls/SKILL.md) | Configuração de SSL/TLS (HTTPS) e gerenciamento de certificados nos diferentes providers (HTTP.sys, ICS, CrossSocket). |
| **horse-integration-tests** | [`horse-integration-tests/SKILL.md`](./horse-integration-tests/SKILL.md) | Escrita de testes de integração HTTP automatizados usando DUnit/DUnitX e portas de teste dinâmicas. |
| **horse-lazarus-compatibility** | [`horse-lazarus-compatibility/SKILL.md`](./horse-lazarus-compatibility/SKILL.md) | Compatibilidade cruzada com Lazarus/FPC, diretivas `{$MODE DELPHI}` e substituição de métodos anônimos do Delphi. |
| **horse-performance-tuning** | [`horse-performance-tuning/SKILL.md`](./horse-performance-tuning/SKILL.md) | Otimização de handlers, redução de alocação no heap (TStringBuilder, fast streaming) e escolha de providers eficientes. |
| **horse-zero-allocation** | [`horse-zero-allocation/SKILL.md`](./horse-zero-allocation/SKILL.md) | Técnicas avançadas de programação sem alocação (Zero-Allocation), fatiamento de string (slices), stack buffers e pools. |
| **horse-mvc-architecture** | [`horse-mvc-architecture/SKILL.md`](./horse-mvc-architecture/SKILL.md) | Organização de APIs de grande porte no padrão MVC limpo, separando a camada HTTP de regras de negócio e persistência. |
| **horse-minimal-api** | [`horse-minimal-api/SKILL.md`](./horse-minimal-api/SKILL.md) | Desenvolvimento rápido de microsserviços focados, mocks e APIs de arquivo único estruturadas com baixo boilerplate. |

---

## Regras Críticas para IAs (Aplicam-se a todas as Skills)

1. **Ordem dos Middlewares**: A ordem de registro é **crítica**. Middlewares globais como `CORS` e `Jhonson` devem ser registrados **antes** de definir qualquer rota.
2. **Gerenciamento de Memória (Johnson)**: O middleware Johnson assume a propriedade de qualquer `TJSONObject` ou `TJSONArray` enviado via `Res.Send<T>`. **NUNCA** chame `.Free` ou `FreeAndNil` em um objeto JSON após enviá-lo pelo response caso o Johnson esteja ativo.
3. **Parâmetros de Rota**: Os parâmetros são definidos usando a sintaxe de dois pontos (ex: `/products/:id`), e não chaves (ex: `/products/{id}`).
4. **Thread-Safety**: O Horse é multithreaded por natureza. **NUNCA** compartilhe conexões de banco de dados globais (como `TFDConnection`) entre requisições concorrentes. Cada handler de rota deve obter/criar sua própria conexão (preferencialmente de um Connection Pool).
5. **Console Output**: Chame `SetConsoleCharSet` em programas modo console para prevenir erros de codificação de caracteres se necessário.
6. **Gerenciamento de Streams**: O objeto `THorseResponse` assume a propriedade de qualquer stream passado para `SendFile`, `Download` ou `Render`. **NUNCA** chame `.Free` ou `FreeAndNil` em um stream após enviá-lo por estes métodos.
