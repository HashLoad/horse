# Checklist de release

*Leia em [English](./release-checklist.md) ou [Português (BR)](./release-checklist.pt-BR.md).*

As releases do Horse usam uma tag pontuada sem prefixo `v` (por exemplo,
`3.3.6`). Prepare a versão em uma PR, integre-a ao `master` e crie a GitHub
Release a partir do commit de merge. Não crie a tag em uma branch não integrada.

## Mapa de versões

| Item | Quando atualizar | Finalidade |
|---|---|---|
| `version` em [`boss.json`](../boss.json) | Na PR da release | Versão distribuída aos usuários do Boss. |
| `HORSE_VERSION` em [`src/Horse.Constants.pas`](../src/Horse.Constants.pas) | Na mesma PR | Valor retornado por `THorseCore.Version`. Deve ser igual a `boss.json.version`. |
| Tag e GitHub Release | Após integrar a PR | Ambas usam exatamente a mesma versão pontuada e apontam para o commit integrado ao `master`. |
| Horse transitivo resolvido em [`tests/src/boss-lock.json`](../tests/src/boss-lock.json) | **Depois** de publicar a release | Registro de dependências dos testes. Regenere com o Boss em outra PR; não edite versão ou hashes manualmente. |

O `version` em `tests/src/boss.json` pertence ao projeto de testes; **não** é a
versão da release do Horse. Números de compiladores como FPC 3.3.1, versões de
pacotes de exemplo e portas de servidor também não são versões do Horse.

## Antes de publicar

1. Revise as mudanças desde a última tag, escolha a nova versão e atualize os
   dois valores da release acima na mesma PR. Procure referências à versão
   anterior do Horse no repositório; atualize a documentação de usuários se o
   comportamento da API ou as instruções de instalação mudaram. Mantenha as
   páginas em inglês e português sincronizadas.
2. Compile e execute os testes relevantes para os providers alterados e para
   o provider padrão. Verifique FPC/Linux quando aplicável. Confirme se o
   workflow de testes do GitHub está habilitado; se estiver desativado,
   registre a validação local na PR e nas notas da release.
3. Integre a PR e confirme que `master`, `boss.json.version` e `HORSE_VERSION`
   coincidem e que a nova tag ainda não existe.
4. Crie a GitHub Release com a mesma versão, apontando para o commit de merge.
   Inclua mudanças, validação e link de comparação com a tag anterior. Confirme
   que a release não é rascunho/pré-release e que a tag aponta para o commit
   esperado. Sincronize a cópia local e o fork do mantenedor com o upstream.

## Publicação de textos multilinha com GitHub CLI

Sempre escreva descrições multilinha de PRs, comentários de issues e notas de
release em um arquivo Markdown UTF-8 e forneça esse arquivo ao GitHub CLI. Não
envie textos como `"Primeira linha\n\nSegunda linha"` por `--body` ou `--notes`:
o PowerShell e o `gh` podem enviar literalmente a barra e a letra `n`, fazendo o
GitHub exibir `\n` no lugar da quebra de linha.

```powershell
gh pr create --body-file pr-body.md
gh pr edit <número> --body-file pr-body.md
gh issue comment <número> --body-file issue-comment.md
gh release create <versão> --notes-file release-notes.md
gh release edit <versão> --notes-file release-notes.md
```

Antes de apagar o arquivo Markdown temporário, leia novamente o conteúdo salvo
e confirme que títulos, listas, blocos de código, caracteres acentuados e
quebras de linha foram preservados:

```powershell
gh pr view <número> --json body --jq .body
gh release view <versão> --json body --jq .body
```

Para comentários de issues, abra também a URL retornada ou consulte o
comentário pela API do GitHub. Essa verificação faz parte da publicação: um
código de saída bem-sucedido do CLI confirma apenas que o GitHub aceitou o
texto, não que ele foi formatado como esperado.

## Depois de publicar: atualizar o lockfile dos testes

1. Em outra branch, execute `boss update` em `tests/src` com uma versão
   conhecida do Boss CLI. A dependência Jhonson traz Horse transitivamente;
   uma release recém-criada não pode ser resolvida antes de sua tag existir.
2. Revise o `boss-lock.json` gerado: a versão do Horse deve coincidir com a
   release. O Boss pode também atualizar Jhonson/RESTRequest4Delphi e renomear
   pastas em `modules/`. Revise `tests/src/boss.json`, os caminhos de busca dos
   projetos Delphi, o workflow FPC e a matriz de compilação quanto a esses
   efeitos. Não inclua mudanças geradas nos projetos que não forem necessárias.
3. Recompile e execute as suítes afetadas com as novas dependências. Envie o
   lockfile e os ajustes de caminhos necessários em uma PR posterior. Os testes
   compilam Horse a partir de `../../src`; o lockfile é um registro de
   dependências, não a versão publicada do framework.

Checagens finais úteis: `git status --short`, `git ls-remote --tags upstream <versão>`,
`gh release view <versão> --repo HashLoad/horse` e uma busca pela
versão anterior do Horse nos metadados de release. Nunca altere uma tag já
publicada para esconder um esquecimento; corrija em uma nova PR e release,
quando necessário.
