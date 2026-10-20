# Regressão do hash de headers no FPC

Na raiz do repositório, em Linux com FPC/FCL:

```sh
sh tests/fpc-header-hash/run-header-hash-test.sh
sh tests/fpc-header-hash/run-header-hash-test.sh -dHORSE_PROVIDER_EPOLL
```

O script recompila o teste **e as units do Horse** (`-B`) com range checking
(`-Cr`) e overflow checking (`-Co`) ativados. O programa recusa compilação sem
essas verificações. `heaptrc` (`-gh`) detecta blocos não liberados; falhas de
compilação, verificações ou memória retornam código diferente de zero.

Os testes verificam hashes DJB2 conhecidos módulo 2³², equivalência entre
maiúsculas/minúsculas ASCII, chave vazia e chave longa de 4096 caracteres,
operações do dicionário de headers e extração de headers de uma requisição FPC.
Eles exercitam as classes diretamente, sem abrir um servidor HTTP.

`HORSE_TEST_SOURCE` pode apontar para um diretório com uma versão anterior da
unit, permitindo reproduzir a falha sem modificar os fontes do checkout.
