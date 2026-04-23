# Guia de Execução de Testes: Pacote Horse

Os testes garantem que a implementação de Statefull Controllers funcione perfeitamente em múltiplos compiladores. O Horse utiliza o framework **DUnitX**.

## 1. Execução no Embarcadero Delphi

No Delphi, os testes são configurados como uma aplicação de console de 32 ou 64 bits.

### Via IDE
1. Abra o arquivo de grupo de projetos: `tests/src/DUnitX.groupproj`.
2. Ative o projeto `Console.dproj`.
3. Pressione `F9` para compilar e rodar.
4. **Verificação:** Procure no log de saída por `Tests.Horse.Controller`. Todos os testes devem retornar `Passed`.

### Via Linha de Comando (CI/CD)
```bash
msbuild tests/src/Console.dproj /t:Build /p:Config=Debug
.\tests\src\Win32\Debug\Console.exe
```

---

## 2. Execução no Lazarus (FreePascal)

No Lazarus, os testes seguem a estrutura do FPCUnit/DUnitX adaptada para o compilador.

### Via Interface Gráfica (IDE)
1. Abra o arquivo `tests/lazarus/Console.lpi`.
2. Pressione `F9` ou clique em `Executar`.
3. O Lazarus exibirá o progresso da execução. Caso ocorram erros, a IDE marcará a linha exata da falha no código-fonte.

### Via Linha de Comando (lazbuild)
Ideal para automação em servidores Linux:
```bash
# Compilação
lazbuild tests/lazarus/Console.lpi

# Execução
./tests/lazarus/Console --all --format:console
```

---

## 3. Detalhes Técnicos dos Testes de Controller

O arquivo `tests/src/tests/Tests.Horse.Controller.pas` valida os seguintes pontos cruciais para a estabilidade cross-IDE:

1.  **Instanciação e Destruição:** Confirma que o `try..finally` interno do Horse está liberando a memória corretamente (evitando Memory Leaks).
2.  **Mapeamento de Rotas Dinâmicas:** Valida se rotas como `/users/:id` estão sendo corretamente resolvidas para o método `GetUserById` via `MethodAddress`.
3.  **Fallback de Verbo:** Garante que se uma rota for registrada mas não houver um método publicado mapeado, o Horse ainda tente executar os métodos virtuais padrão (`Get`, `Post`, etc.).

**Nota:** Devido às diferenças de RTTI entre Delphi e FPC, este teste é o mais sensível do pacote e deve sempre passar em ambos antes de qualquer commit na branch principal.
