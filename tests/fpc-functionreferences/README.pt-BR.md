# Regressões de compatibilidade de callbacks FPC

Leia em [English](./README.md) ou [Português (BR)](./README.pt-BR.md).

Execute na raiz do Horse com Python 3, FPC e as units RTL/FCL correspondentes:

```text
python3 tests/fpc-functionreferences/run.py --mode all
```

`all` compila as suítes procedurais e depois as suítes de closures e exceções
com `HORSE_FPC_FUNCTIONREFERENCES`. Use `--mode legacy` no FPC 3.2.2 ou
`--mode funcref` para o ramo opt-in no FPC 3.3.1. O runner recompila todas as
units (`-B`) em diretório temporário separado por executável; não mistura
arquivos `.ppu` entre modos. Erros de compilação, processo, timeout e blocos
não liberados reportados pelo heaptrc reprovam a execução.

A suíte de closures verifica Tree e Radix: string/interface capturadas após
retorno de uma fábrica, middleware antes e depois de `Next`, ordem dos hooks,
callbacks abreviados do core e instâncias separadas com hooks capturados e
callbacks abreviados. Também verifica retomada de `Next` após despacho aninhado,
finalizadores 404/405 e mais de 64 callbacks abreviados.
Chamadas são síncronas; os testes não prometem que `Next`
possa sobreviver à requisição. Não abrem sockets nem validam tráfego concorrente.

A suíte de exceções mantém suas 34 expectativas nos dois modos. A rodada
legada também executa hash de cabeçalhos, decodificação de URL e lifetime dos
roteadores; essas suítes existentes constroem callbacks procedurais. A rodada
legada verde não comprova suporte a closures naquele compilador.

Para compilador/RTL isolados, use `--compiler PATH` e repita
`--compiler-arg=OPÇÃO` para sua configuração, por exemplo `--compiler-arg=-n`
e o correspondente `--compiler-arg=-FuPATH_DAS_UNITS/*`. Não misture units
de versões de compilador. `FPC` também pode selecionar o executável.

Para contraprova, mantenha estes testes e indique os fontes sem o patch:

```text
python3 tests/fpc-functionreferences/run.py --mode funcref --source-root PATH_DA_BASE/src
```

Na base `a0ac019`, os callbacks capturados não compilam. Esse é o resultado
negativo esperado, não um teste aprovado.

A validação local cobre somente Linux x86_64. Delphi, FPC 3.2.0, outros alvos
e integração HTTP exigem validação separada; essas regressões portáteis não
substituem a matriz completa de testes upstream.
