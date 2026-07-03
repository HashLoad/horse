# Testes de Integridade e Resiliência do Horse

O Horse possui um conjunto de testes de integridade automatizados projetados para validar o funcionamento do framework em cenários reais de rede, bem como analisar a sua resiliência perante falhas graves de sistema e estouros críticos.

Esses testes residem na pasta: `samples/delphi/console_complete/`

---

## Por que os Testes de Integridade são Úteis?

Diferente de testes unitários tradicionais (que testam métodos e classes isolados), os testes de integridade simulam o comportamento de um cliente real consumindo uma API Horse ativa na rede. 

Eles são extremamente úteis para:
- **Evitar Regressões**: Garantir que alterações no parser de HTTP, roteamento, middlewares ou buffers não quebrem o comportamento esperado das chamadas reais de rede.
- **Validar Verbos e Cabeçalhos**: Testar o parse correto de query strings, parâmetros de rota, cabeçalhos (*headers*) case-insensitive e o verbo customizado `QUERY`.
- **Testar uploads reais**: Validar transmissões em formato `multipart/form-data`.
- **Análise de Resiliência (Robustez)**: Avaliar como o servidor Horse lida com falhas fatais no código da aplicação (como referências nulas) sem comprometer o serviço.

---

## O que é Testado?

O script de teste de integridade ([test_integrity.ps1](file:///d:/Delphi/horse/samples/delphi/console_complete/test_integrity.ps1)) realiza duas rodadas consecutivas de testes: uma compilando o servidor exemplo ([ConsoleComplete.dpr](file:///d:/Delphi/horse/samples/delphi/console_complete/ConsoleComplete.dpr)) com o **Roteador Padrão** (`RouterTree`), e outra com o **Radix Router** (`RadixRouter`). Cada rodada executa as seguintes chamadas HTTP reais via `Invoke-RestMethod` e `curl`:

1. **GET /ping**: Testa o fluxo básico e a resposta simples.
2. **GET /resource/:id**: Valida o parse de parâmetros na URL, query strings e cabeçalhos de autorização case-insensitive.
3. **POST /resource**: Envio de dados no corpo (payload) do request.
4. **PUT, PATCH, DELETE /resource/:id**: Testes de verbos RESTful de atualização e remoção.
5. **QUERY /search**: Envio de payloads de pesquisa complexos usando o verbo customizado `QUERY`.
6. **POST /upload**: Upload real de arquivos simulando `multipart/form-data`.
7. **GET /error-trigger**: Fluxo de exceção tratada limpa através da classe `EHorseException`.
8. **GET /clientes, GET /pessoas, GET /qualquercoisa**: Prioridade de correspondência de rotas exatas em relação a rotas coringa (`*` ou wildcard).

---

## Resiliência e Limitações do Horse (Erros Graves)

Os testes de integridade incluem endpoints específicos para forçar falhas catastróficas e analisar a integridade do processo do servidor:

### 1. Interceptação de Access Violation (AV)
Ao acessar o endpoint `/av-trigger`, um erro de escrita em ponteiro nulo (de propósito) é disparado:
```delphi
var
  LPointer: PInteger;
begin
  LPointer := nil;
  LPointer^ := 42; // Access Violation!
end;
```
* **Comportamento do Horse**: O Horse intercepta a violação de acesso em nível de thread/requisição, impede a queda do executável principal, responde ao cliente com um erro HTTP `500 Internal Server Error` e mantém o servidor ativo para continuar atendendo a outras conexões normalmente.

### 2. Estouro de Pilha (Stack Overflow)
Ao acessar o endpoint `/stack-trigger`, uma recursão infinita estoura o limite de pilha de memória:
```delphi
var
  LRecurse: TProc;
begin
  LRecurse := procedure
    begin
      LRecurse();
    end;
  LRecurse(); // Stack Overflow!
end;
```
* **Comportamento e Limites**: O Stack Overflow representa um limite de resiliência a nível de hardware/SO. Quando a thread estoura a quantidade reservada de memória física para stack (pilha), o sistema operacional intervém e encerra o processo do servidor imediatamente para autoproteção. O script de integridade captura essa queda de forma segura.

---

## Como Executar Localmente

Para rodar os testes de integridade em ambiente local Windows com o PowerShell, basta navegar até a pasta e iniciar o script:

```powershell
cd samples/delphi/console_complete
.\test_integrity.ps1
```

O script cuidará de todo o processo de compilação dos dois roteadores com o compilador Delphi (`dcc32`), inicialização em background para cada um, chamadas HTTP de testes, limpeza e relatório final de falhas de ambos.
