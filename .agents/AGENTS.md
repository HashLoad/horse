# Diretrizes de IA para o Projeto Horse

Este documento estabelece as regras de design e desenvolvimento do framework Horse específicas para agentes de IA que atuam neste repositório.

## 🟢 Ciclo de Vida da Requisição (Lifecycle Hooks)
* O Horse possui suporte nativo e thread-safe a ganchos de ciclo de vida (`onRequest`, `preParsing`, `preValidation`, `onSend` e `onResponse`) em cascata cooperativa (CPS).
* Ao criar novos middlewares ou funcionalidades de tratamento, utilize a infraestrutura de ganchos em vez de interceptores ad-hoc nas rotas para manter a conformidade arquitetural.

## 🟢 Ciclo de Vida do Servidor e Desligamento Suave (Server Lifecycle & Graceful Shutdown)
* O Horse gerencia o escoamento lógico de requisições ativas através das propriedades de telemetria `ActiveRequests` e `IsShuttingDown` no core (`THorseCore`) e nas instâncias locais (`THorseInstance`).
* Para encerramentos coordenados e seguros (probes de saúde no Kubernetes/Load Balancers), use sempre `THorse.StopListenGraceful(TimeoutMS)` ou `LInstance.StopListenGraceful(TimeoutMS)`.
* As propriedades `ActiveRequests` e `IsShuttingDown` estão expostas estaticamente no facade `THorse` e devem ser usadas em endpoints de `/health` ou monitoramento de observabilidade APM.
* **Providers Físicos de Rede**: Ao criar ou atualizar qualquer provedor físico de transporte no ecossistema do Horse:
  * Deve-se obrigatoriamente sobrescrever a função `GetActivePort` retornando a porta física ativa (`FPort` ou `0` para acoplados externos como CGI/Apache/ISAPI) para viabilizar a resolução de instâncias no Multi-Instance.
  * Deve-se acionar explicitamente `TriggerBeforeListen` no topo das rotinas de inicialização física (`InternalListen` / `Listen`).
  * Deve-se acionar explicitamente `TriggerBeforeStop` no topo das rotinas de encerramento físico (`InternalStopListen` / `StopListen`).
  * Deve-se sobrescrever e implementar `StopListenGraceful(const ATimeoutMS: Integer)` para suportar o escoamento de requisições ativas daquele provedor físico se o mesmo gerenciar o socket TCP.

## 🟢 Gerenciamento e Injeção de Dependências (Request Scope)
* O Horse expõe a propriedade `Services` na classe de requisição `THorseRequest`, provendo um container IoC local e thread-safe para o escopo do request.
* Para serviços que devem ser destruídos automaticamente ao final da requisição (evitando vazamento de memória), registre-os usando `Req.Services.Add(TClass, Instance)`.
* Para inicialização sob demanda (Lazy Loading), use `Req.Services.AddFactory(TClass, FactoryMethod)`. A instância só será criada no momento da chamada de `Resolve`.
* Para obter um serviço previamente injetado, chame `Req.Services.Resolve(TClass)` e faça a coerção de tipo necessária.
* Nunca instancie dicionários de escopo ou de serviços paralelos dentro das closures de rotas; utilize sempre a infraestrutura nativa do `Services` para garantir o ciclo de vida e thread-safety coordenados pelo framework.

## 🧪 Padrões de Testes e Concorrência
* Ao escrever testes de integração que envolvam o encerramento do servidor ou simulação de tráfego, utilize sempre a biblioteca HTTP nativa do Delphi (`System.Net.HttpClient` e `System.Net.URLClient`) para garantir compatibilidade multiplataforma nativa no FPC (Lazarus/Linux) sem depender de pacotes externos.
* Em testes de shutdown ou concorrência física, utilize o cabeçalho `Connection: close` na requisição do cliente HTTP para forçar a liberação imediata do socket no sistema operacional, evitando travamento de pools de conexão físicos.

## 🟢 Arquitetura Multi-Instance (`THorseInstance`)
* O Horse suporta a execução paralela de múltiplos servidores lógicos e isolados no mesmo processo de aplicação através da classe `THorseInstance`.
* Ao projetar ou atualizar middlewares do ecossistema, garanta que eles não dependam de dados em variáveis globais ou estáticas (`class var` singletons) do core, permitindo que cada instância de `THorseInstance` configure isoladamente suas dependências, rotas e manipuladores.
* Para preservar a compatibilidade de compilação cruzada multiplataforma FPC/Lazarus, evite o uso de closures ou procedimentos anônimos inline (`procedure begin end`) em manipuladores de ciclo de vida e rotas lógicas locais das instâncias do Horse, preferindo procedimentos regulares e delegados de objetos.

## 🟢 Observabilidade e Ganchos de Telemetria (Telemetry Hooks)
* O Horse possui infraestrutura nativa e de baixíssimo overhead baseada em `TStopwatch` para rastreamento de latência em requisições.
* Ao estender o ecossistema ou criar novos middlewares de APM/observabilidade (como Prometheus, OpenTelemetry, logging), use sempre o gancho nativo `AddOnTelemetry` (`THorse.AddOnTelemetry` ou `LInstance.AddOnTelemetry`) em vez de introduzir wrappers customizados nos blocos de execução de rotas ou temporizadores ad-hoc que geram overhead de heap.
* Garanta que os callbacks registrados em `AddOnTelemetry` sejam protegidos internamente com blocos `try-except` individuais (silenciando exceções) para assegurar que falhas na coleta de telemetria nunca causem interrupções no fluxo principal de retorno HTTP do cliente ou derrubem a thread de execução do socket.

## 🟢 Roteamento Avançado (Regex e Parâmetros Opcionais)
* Ao criar rotas com restrições Regex, utilize a sintaxe parametrizada `:paramName(regexPattern)` (ex: `:id(\d+)`). Evite Regex isolado de segmento inteiro do tipo `(regexPattern)` para novas implementações, preferindo o padrão parametrizado compatível com FPC e Delphi.
* Ao manipular parâmetros opcionais de rota `:paramName?` nos roteadores ou middlewares, garanta que o dicionário de parâmetros `Params` da requisição sempre receba a chave correspondente mesmo quando o parâmetro for omitido (nesse caso com valor vazio `''`), para assegurar determinismo no acesso das chaves em runtime.
* Para qualquer novo utilitário de Regex ou validação de rotas, utilize e estenda a unit unificada `Horse.Core.Regex.pas` (que abstrai `System.RegularExpressions` e `RegExpr` de forma compatível e multiplataforma), nunca importando bibliotecas brutas de Regex diretamente nos roteadores para preservar a compilação no Lazarus/FPC.

## 🟢 Desenvolvimento com gRPC e RTTI
* **Diretiva de Compilação RTTI (`{$M+}`)**: Ative sempre a diretiva `{$M+}` na unidade de mensagens gRPC (como `users.pas`) para garantir que os metadados RTTI sejam gerados de forma correta e completa em ambos os compiladores (Delphi e FPC/Lazarus).
* **Visibilidade de Propriedades**: Declare todas as propriedades de classes de mensagens gRPC (Request e Response) na seção `published` em vez de `public` ou `private`. Isso garante a geração completa do metadado de escrita e leitura RTTI clássico e evita Access Violations no `TRttiProperty.SetValue` decorrentes de offsets de campos nulos.
* **Ciclo de Vida do `TRttiContext`**: O tempo de vida dos objetos RTTI (como `TRttiProperty` ou `TRttiType`) está vinculado à instância de `TRttiContext` que os retornou. Para evitar referências pendentes (*dangling pointers*) e Access Violations na heap durante a serialização/deserialização concorrente, utilize uma instância de `TRttiContext` estática a nível de classe e global (ex: `THorseProtobufRtti.FContext`) em vez de variáveis locais temporárias.
* **Gerenciamento de Instâncias de Serviço**: Ao implementar serviços baseados em interfaces (`IInvokable`), desative a contagem de referências implícita (ARC) no Delphi sobrescrevendo `_AddRef` e `_Release` para retornar `-1` na classe de serviço implementadora (como `TUserServiceImpl`). Isso evita a auto-destruição prematura do objeto de serviço quando ele for invocado via RTTI ou repassado na infraestrutura do provedor.

## 🟢 Desenvolvimento com WebSockets
* **Upgrade de Conexão**: O upgrade do protocolo deve ser realizado chamando `Res.UpgradeToWebSocket(AOnConnectCallback)`.
* **Ciclo de Vida do Contexto**: Não armazene nem faça referência a instâncias de `THorseRequest` ou `THorseResponse` dentro dos callbacks ou closures de eventos do WebSocket (como `OnMessage`, `OnDisconnect`, etc.). O ciclo de vida da requisição HTTP encerra-se com o upgrade, portanto use apenas a interface `IHorseWebSocketConnection`.
* **Escrita Multithread**: Os métodos `SendText` e `SendBinary` da conexão são thread-safe por projeto (utilizando `TCriticalSection` interno). Não há necessidade de adicionar travas adicionais ao realizar envios concorrentes.
* **Comportamento Fail-Fast**: Provedores incompatíveis (como `HttpSys` e servidores gerenciados/hospedados como `Apache`, `ISAPI`, `CGI`) devem retornar HTTP `501 Not Implemented` ou `400 Bad Request` e lançar `EHorseCallbackInterrupted` (da unit `Horse.Exception.Interrupted`) para abortar o processamento do pipeline do Horse de forma segura.
* **Compatibilidade Lazarus/FPC**: Para manter a compatibilidade cruzada do código entre Delphi e FPC, evite o uso de closures/procedimentos anônimos inline nos callbacks de conexão do WebSocket do Lazarus, preferindo procedimentos regulares ou delegações de classe.

## 🟢 Desenvolvimento com Web Streams (Chunked HTTP) e Server-Sent Events (SSE)
* **Assinatura e Escrita no Stream**: Para disparar transmissões de dados continuadas, utilize sempre `Res.SendStream(Callback)`. A escrita física deve ser efetuada chamando `AWriter.Write(Data)` após a verificação de conectividade `AWriter.IsConnected()`.
* **Compatibilidade Lazarus/FPC**: Evite closures ou procedimentos anônimos inline (`procedure begin end`) para os callbacks de rotas e streams no Lazarus/FPC, visando compilação multiplataforma estável. Use procedimentos ou métodos normais.
* **Ciclo de Vida do Indy (`TIdHTTPAppResponse`)**:
  * Ao operar sob Indy, a thread de conexão física é obtida acessando o campo protegido `FThread` do `TIdHTTPAppResponse`.
  * Sempre acione `MoveCookiesAndCustomHeaders` e marque a flag `FSent := True` do Indy antes de disparar o `WriteHeader` para evitar que cabeçalhos duplicados ou a resposta HTML padrão de 200 OK do Indy sejam enviados de forma redundante.
  * Para evitar que o Indy redefina o `ContentType` do stream ou injete seu HTML de 39 bytes padrão no encerramento da rota, vincule um `TMemoryStream` vazio e `ContentLength := 0` no `FResponseInfo.ContentStream` com `FreeContentStream := True` no momento de iniciar o streaming.
* **Segurança e Middlewares**: Garanta que middlewares que alteram a resposta HTTP de forma assíncrona ou global (como compressão de dados gzip/deflate ou interceptores globais) ignorem a requisição quando `Res.IsStreaming` for `True` para prevenir corrupção de buffer e travamentos na rede.

