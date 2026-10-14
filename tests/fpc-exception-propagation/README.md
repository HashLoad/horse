# Regressão de propagação de exceções no FPC

Execute na raiz do repositório, em Linux com FPC e as units FCL instaladas:

```sh
sh tests/fpc-exception-propagation/run-exception-propagation-test.sh
sh tests/fpc-exception-propagation/run-exception-propagation-test.sh -dHORSE_PROVIDER_EPOLL
```

O teste compila todos os fontes com `-B`, usa um diretório temporário para os
binários e habilita `heaptrc` (`-gh`). Falhas nas verificações ou blocos de memória
não liberados produzem código de saída diferente de zero.

São 34 casos, 17 para cada roteador (Tree e Radix), usando callbacks procedurais
compatíveis com FPC. Eles cobrem `Exception` e `EHorseException` capturadas pelo
middleware, callbacks posteriores, middleware interno, prioridade sobre `OnError`,
fallback sem middleware e após relançamento, status de erro existente e interrupção
da cadeia com e sem `OnError`. A resposta JSON é verificada por parsing, incluindo
mensagem, status e `Content-Type`.

Na saída, `kind=0` indica `Exception`, e `kind=1`, `EHorseException`.
`mode` identifica o cenário: 0 = fallback, 1 = middleware trata, 2 = `OnError`,
3 = middleware relança, 4 = middleware trata com `OnError` registrado,
5 = resposta de erro existente, 6 = interrupção e 7 = interrupção com `OnError`.
`source=0` indica a rota, 1 = callback posterior e 2 = middleware interno.

O parâmetro opcional `HORSE_TEST_SOURCE` permite selecionar outro diretório de
fontes para reproduzir a falha anterior à correção. Esses testes exercitam os
roteadores diretamente; não abrem um servidor HTTP.
