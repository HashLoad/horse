# Regressão de TCP keep-alive dos providers Indy (#623)

O Indy antigo envia os argumentos de `SetKeepAliveValues` diretamente às opções
Linux, em segundos. O Indy mais novo divide esses argumentos por 1.000.
`60000/1000` falha com erro 22 no antigo; trocar indiscriminadamente para `60/1`
falha no moderno. Os testes verificam os valores efetivos, não apenas a ausência
de exceções.

## Windows

```powershell
./tests/indy-keepalive/run-keepalive.ps1 -Version 17.0 -Platform Windows -Provider Console
./tests/indy-keepalive/run-keepalive.ps1 -Version 37.0 -Platform Windows -Provider VCL
```

O teste de socket usa IPv4 e IPv6, confirma `SO_KEEPALIVE` e intercepta a chamada
virtual do stack para verificar que os argumentos continuam sendo exatamente
`True, 60000, 1000`. O teste HTTP faz três conexões independentes ao provider real
e verifica status 200 e corpo. O detector de vazamentos Delphi fica ativado.

## Linux: Indy moderno e providers reais

É necessário Delphi com compilador e bibliotecas Linux64 instalados, Docker Linux
e uma imagem com GNU binutils e bibliotecas de desenvolvimento C:

```powershell
docker build -t horse-indy-tests:latest tests/indy-keepalive
./tests/indy-keepalive/run-keepalive.ps1 -Version 23.0 -Platform Linux -Provider Console -DockerImage horse-indy-tests:latest
./tests/indy-keepalive/run-keepalive.ps1 -Version 37.0 -Platform Linux -Provider Daemon -Radix -DockerImage horse-indy-tests:latest
```

O runner recompila (`-B`) os fontes, liga os objetos Linux no container e aplica
limite de 40 segundos a cada execução. Os relatórios ficam em
`benchmarks/results/indy-keepalive-*`. `-Radix` seleciona o roteador alternativo.

`IndyKeepAliveCheck` usa sockets TCP IPv4/IPv6. Compara o controle da chamada
antiga com a configuração corrigida e lê `SO_KEEPALIVE`, `TCP_KEEPIDLE=60` e
`TCP_KEEPINTVL=1` por `getsockopt`, incluindo configuração repetida.

`ProviderKeepAliveCheck` executa o provider Console ou Daemon real. Faz três
requisições HTTP IPv4 com o stack padrão e repete o teste com um stack que modela
o contrato antigo em segundos. Esse segundo stack sobrescreve somente o método
virtual `SetKeepAliveValues`; as operações nativas do socket continuam reais.
O Daemon roda como filho direto do PID 1 no container, sem o double fork; isso
valida seu listener, não a daemonização do processo. A VCL é testada no Windows.

## Linux: fontes reais do Indy antigo

O teste de socket também aceita a árvore de fontes Indy de uma instalação antiga:

```powershell
./tests/indy-keepalive/run-keepalive.ps1 -Version 23.0 -Platform Linux -SocketOnly `
  -LegacyIndySource 'C:/Program Files (x86)/Embarcadero/Studio/22.0/source/Indy10' `
  -LegacyCompilerDefine VER350 -DockerImage horse-indy-tests:latest
```

O teste usa os fontes originais do Indy do Delphi 11, recompilados com Delphi 12
para Linux. `VER350` permite que os defines desse Indy reconheçam o compilador;
não altera os fontes nem a instalação do Delphi. Não é uma execução produzida
pelo compilador Delphi 11/Linux. Esse cenário verifica o erro 22 na chamada
antiga e os valores corrigidos com a implementação legada real. É separado do
modelo de contrato antigo usado no teste HTTP completo.

## Controle negativo

`-HorseSource` pode apontar para um diretório contendo os providers anteriores à
correção. Os demais fontes são encontrados em `src`. O teste HTTP com
`-legacy-contract` deve falhar por conexão encerrada/resetada nesse controle;
com a correção, ambas as execuções HTTP devem passar.

Estes testes dependem de Delphi licenciado e, por isso, não são executados pelo
job público de FPC do GitHub Actions. Não há dependência Indy nova para os
providers FPC. Não é reivindicada cobertura TLS nem HTTP IPv6.
