# Contribuindo com o Horse

*Leia em [English](./CONTRIBUTING.md) ou [Português (BR)](./CONTRIBUTING.pt-BR.md).*

Obrigado pelo interesse em melhorar o Horse. Este documento explica como reportar bugs, sugerir features, contribuir com código e — importante — manter nossa **documentação bilíngue** (Inglês / Português-BR) sincronizada.

Se você só quer corrigir um typo ou abrir um PR pequeno, pule para [Processo de Pull Request](#processo-de-pull-request).

---

## Reportando bugs

Abra uma issue no GitHub em [`HashLoad/horse/issues`](https://github.com/HashLoad/horse/issues) com:

1. **O que você esperava** que acontecesse.
2. **O que realmente aconteceu** (inclua stack traces, bodies de resposta, status HTTP).
3. **Reprodução mínima** — um `.dpr` de 30 linhas ou menos é o ideal. Tire tudo que não é necessário para disparar o bug.
4. **Ambiente**:
   - Versão do Delphi ou FPC (ex. `Delphi 12 Athens` / `FPC 3.2.2 + Lazarus 2.2`).
   - Plataforma-alvo (`Win64`, `Linux64`, `macOS ARM64`, …).
   - Define(s) de provider ativos: `HORSE_CROSSSOCKET`, `HORSE_VCL`, `HORSE_DAEMON`, etc. (nenhum = Console + Indy padrão).
   - Versão do Horse (ou SHA do commit).

Os samples já prontos em `samples/delphi/` e `samples/lazarus/` são boas bases de comparação. Se o bug reproduz num sample não modificado, mencione qual.

## Sugerindo features

Abra uma issue descrevendo **o problema que você quer resolver**, não a implementação que tem em mente. Podemos discutir abordagens antes do código ser escrito. Novos providers de transporte, novos tipos do core e novas superfícies de API pública são melhor discutidos antes de um PR — refactors pequenos e correções de bugs podem ir direto pro PR.

## Contribuindo com código

### Branching e commits

- Branch a partir de `master` para fixes e features pequenas; a partir de `dev` para trabalho maior em andamento (quando existir).
- Use prefixos de [Conventional Commits](https://www.conventionalcommits.org/): `feat:`, `fix:`, `chore:`, `docs:`, `refactor:`, `test:`, `perf:`.
- Uma mudança lógica por commit. Esmague commits de WIP antes do push.

### Estilo de código

O estilo Pascal segue o código existente ao redor. O projeto não força um formatador, mas em resumo:
- Indent de 2 espaços (sem tabs).
- `PascalCase` para tipos e rotinas; `camelCase` para variáveis locais.
- Prefixo `T` para classes, `I` para interfaces, `F` para campos, `E` para exceptions, `H` para helpers quando necessário.
- `try / finally` para todo recurso; libere o que você cria.
- Sempre teste mentalmente contra **ambos** `dcc32` (Delphi) e `fpc` (FPC) para mudanças em units compartilhadas. Os deslizes mais comuns são `var` inline (apenas Delphi 10.3+) e tipos RTL só-Delphi como `TWebRequest`. Veja [Suporte de Compilador](./doc/compiler-support.pt-BR.md) para os padrões de guard a usar.

### Requisitos entre compiladores

Toda mudança em `src/Horse.*.pas` precisa compilar em ambos:

- Delphi 10.4 Sydney (ou mais recente)
- FPC 3.2.0 (ou mais recente) com Lazarus

Use `{$IF DEFINED(FPC)} ... {$ELSE} ... {$ENDIF}` para ramificar onde a RTL difere (mais frequente `Web.HTTPApp` vs `fpHTTP` / `HTTPDefs`).

### Testes

Adicione ou estenda testes em `tests/` quando mudar comportamento. Rode localmente antes do push:

```sh
# Na raiz do projeto, após boss install
boss test
```

O CI roda em Delphi 11 / 12 contra a mesma suíte. CI verde é pré-requisito para merge.

## Contribuindo com documentação

A documentação fica em duas camadas:

- **`README.md`** + **`README.pt-BR.md`** — páginas iniciais com o quickstart, a visão geral de providers e ponteiros para `doc/`.
- **`doc/`** — o wiki: oito páginas de tópicos, cada uma em dois idiomas (`.md` para inglês, `.pt-BR.md` para português).

### A regra bilíngue

**Toda página de doc precisa existir nos dois idiomas.** Quando você edita uma, edita a outra no mesmo PR. PRs que mexem em apenas um idioma serão solicitados a adicionar a tradução correspondente antes do merge.

| Você edita | Também precisa editar |
|---|---|
| `README.md` | `README.pt-BR.md` |
| `doc/<tópico>.md` | `doc/<tópico>.pt-BR.md` |
| Adicionar `doc/new-topic.md` | Adicionar `doc/new-topic.pt-BR.md` e atualizar `doc/index.md` **e** `doc/index.pt-BR.md` |

A regra bilíngue evita que as duas versões de idioma divirjam. Já vimos projetos onde o idioma secundário foi adicionado uma vez e nunca atualizado — a documentação se torna enganosa para metade dos usuários. Este projeto evita isso tratando os dois idiomas como primários.

### Se você só fala um dos dois idiomas

Tudo bem — abra o PR com suas edições no idioma em que você se sente confortável e escreva o arquivo correspondente com um placeholder claro:

```markdown
# <Título — Traduzido>

*Leia em [English](./<arquivo>.md) ou [Português (BR)](./<arquivo>.pt-BR.md).*

> **Tradução pendente.** Esta página espelha [`<arquivo>.md`](./<arquivo>.md) mas ainda não foi traduzida.
> Revisores fluentes neste idioma são bem-vindos a atualizá-la.
```

Um revisor ou PR de follow-up preencherá a tradução. A estrutura (headings, code blocks, alvos de links) deve casar com a página do idioma de origem para que um futuro tradutor só precise traduzir o texto corrido.

### Banner de troca de idioma

Toda página de doc (inclusive o README) começa com este banner logo após o título:

```markdown
*Leia em [English](./<arquivo>.md) ou [Português (BR)](./<arquivo>.pt-BR.md).*
```

Para o README, que usa `<p align="center">` em HTML na seção do topo, o bloco equivalente é:

```html
<p align="center">
  <i>Leia em <a href="./README.md">English</a> ou <a href="./README.pt-BR.md">Português (BR)</a>.</i>
</p>
```

A versão em inglês usa `Read this in [English](...) or [Português (BR)](...)`.

### Notas de estilo

- Mantenha parágrafos curtos — a maioria dos leitores escaneia em vez de ler.
- Code blocks devem ser **executáveis** — cole em um projeto novo e compila.
- Para termos técnicos amplamente usados em inglês na comunidade brasileira Delphi/FPC (`provider`, `middleware`, `thread`, `pool`, `framework`, `pull request`), mantenha a forma em inglês no texto português — lê mais natural que uma tradução forçada.
- Tabelas e headings traduzem por completo; preserve a ordem original das colunas.
- Quando referenciar outro doc, linke para a versão do mesmo idioma: em `routing.pt-BR.md`, linke para `request-response.pt-BR.md`, não `request-response.md`. O language switcher no topo de cada página cuida da troca entre idiomas.

### Verificando links internos

Antes de abrir o PR, rode isto na raiz do repo para pegar referências cruzadas quebradas:

```bash
for f in README.md README.pt-BR.md doc/*.md; do
  grep -oE '\]\(\.\/[^)]+\.md[^)]*\)' "$f" \
    | sed -E 's|\]\(\./||; s|\)$||; s|#.*||' \
    | while read target; do
        if [ -n "$target" ]; then
          dir=$(dirname "$f")
          [ -f "$dir/$target" ] || echo "QUEBRADO em $f: $target"
        fi
      done
done
```

Saída silenciosa = todo link interno resolve.

## Processo de Pull Request

1. **Fork** `HashLoad/horse` e crie uma branch tópica no seu fork.
2. **Faça commits focados** — uma mudança lógica por commit, mensagem em conventional-commit.
3. **Teste local** — mudanças de código precisam passar `boss test`; mudanças de doc precisam passar o link-checker acima.
4. **Atualize os dois idiomas** para qualquer mudança de doc. Veja [a regra bilíngue](#a-regra-bilíngue).
5. **Abra o PR** contra `HashLoad/horse:master` (ou `:dev` se o repo estiver usando uma branch de desenvolvimento no momento).
6. **Descreva a mudança** no body do PR: o que muda, por quê e qual impacto visível ao usuário.
7. **Responda à review** no mesmo PR — por favor não abra um novo PR para feedback de review; envie commits de fixup para a branch existente.

Os mantenedores tentam triagem de todo PR em uma semana. Se não receber resposta em duas semanas, um ping educado no PR é bem-vindo.

## Reportando problemas de segurança

Para relatos sensíveis de segurança — ex. crash remoto, smuggling de header, bypass de validação de certificado — por favor **não** abra uma issue pública. Use o canal Telegram ([@hashload](https://t.me/hashload)) ou e-mail um dos mantenedores listados no `git log` diretamente. Vamos coordenar a correção e a divulgação coordenada.

## Comunidade

- Telegram: [@hashload](https://t.me/hashload) — o local mais ativo para dúvidas e anúncios.
- Issues: [`HashLoad/horse/issues`](https://github.com/HashLoad/horse/issues) — para bugs e discussão de features.
- PRs: [`HashLoad/horse/pulls`](https://github.com/HashLoad/horse/pulls) — para código.

Obrigado por contribuir.
