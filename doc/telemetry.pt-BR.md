# Observabilidade e Telemetria

*Leia em [English](./telemetry.md) ou [Português (BR)](./telemetry.pt-BR.md).*

A observabilidade é um aspecto crítico para executar APIs modernas em produção. O Horse suporta o monitoramento de aplicações, rastreamento (tracing) e coleta de métricas através de seu pipeline de middlewares.

Ao usar middlewares, você pode coletar a duração das requisições, trace IDs, códigos de resposta e métricas do sistema sem misturar essa lógica com a sua regra de negócio.

---

## Pilares da Observabilidade no Horse

Por meio de integrações de terceiros, você pode configurar uma observabilidade completa para suas aplicações Delphi e Lazarus:

### 1. Rastreamento Distribuído (OpenTelemetry)

O rastreamento distribuído permite acompanhar as requisições à medida que elas fluem pela sua arquitetura. Usando o padrão OpenTelemetry, você pode visualizar gráficos de chamadas, medir tempos de consulta ao banco de dados e rastrear chamadas de microsserviços.

* **Middleware:** [horse-opentelemetry](https://github.com/regyssilveira/horse-opentelemetry)
* **O que faz:** Inicia e propaga spans automaticamente, registra requisições, injeta o contexto de trace W3C e exporta dados de rastreamento para coletores OpenTelemetry (Jaeger, Zipkin, Dynatrace, Datadog, etc.).

### 2. Coleta de Métricas (Prometheus)

As métricas informam o *quanto* e o *quão rápido* sua aplicação está rodando. Você pode monitorar taxas de requisição (Request Rates), taxas de erro (Error Rates) e percentis de duração (método RED).

* **Middleware:** [horse-prometheus](https://github.com/regyssilveira/horse-prometheus)
* **O que faz:** Monitora métricas internamente (contadores, histogramas, gauges) e expõe um endpoint (geralmente `/metrics`) no formato esperado pelo coletor do Prometheus.

---

## Início Rápido: Configurando Telemetria

Para adicionar métricas e rastreamento à sua aplicação Horse, instale os pacotes via Boss:

```sh
boss install regyssilveira/horse-opentelemetry
boss install regyssilveira/horse-prometheus
```

Em seguida, registre-os em seu arquivo principal. Recomenda-se registrar os middlewares de observabilidade logo no início do pipeline de middlewares para capturar a latência total da resposta e registrar quaisquer exceções:

```delphi
uses
  Horse,
  Horse.OpenTelemetry,
  Horse.Prometheus;

begin
  // Registre o OpenTelemetry primeiro para rastrear todo o ciclo de vida da requisição
  THorse.Use(THorseOpenTelemetry.New('my-horse-api'));

  // Registre o Prometheus para coletar métricas das requisições
  THorse.Use(THorsePrometheus.New);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

## Executando Métricas em uma Porta Separada

Em produção, geralmente é uma prática recomendada de segurança expor endpoints de métricas (como `/metrics`) apenas internamente. Você pode rodar um servidor Horse separado em uma porta diferente exclusivamente para fins de administração e métricas:

```delphi
uses
  Horse,
  Horse.Prometheus;

begin
  // Servidor da API Principal
  THorse.Use(THorsePrometheus.New);
  
  THorse.Get('/api/v1/users', ...);
  // Escuta na porta pública
  THorse.Listen(9000);

  // Endpoint de métricas em uma porta privada
  // Nota: A execução de múltiplas instâncias do Horse é suportada.
end.
```

---

## Ganchos de Telemetria Nativos (Native Telemetry Hooks)

O Horse introduz um gancho de telemetria nativo de altíssima precisão e sem alocação de memória (*Zero-Allocation*), baseado em `TStopwatch` (stack-allocated).

Este recurso permite monitorar e medir com precisão milimétrica a latência de todas as requisições HTTP processadas, permitindo a fácil integração de logs, ferramentas APM (Application Performance Monitoring) e coletores customizados de observabilidade.

### Assinatura do Callback

O tipo do callback de telemetria é definido da seguinte forma:

```delphi
THorseOnTelemetry = {$IF DEFINED(FPC)}procedure{$ELSE}reference to procedure{$ENDIF}(const ARequest: THorseRequest; const AResponse: THorseResponse; const AExecutionTimeMS: Double);
```

### Configurando o Gancho Globalmente

Você pode registrar um callback global que será disparado ao término de todas as requisições HTTP da aplicação:

```delphi
uses
  Horse, System.SysUtils;

begin
  THorse.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      Writeln(Format('[Telemetry] %s %s - Status: %d - Latency: %.2f ms', 
        [Req.Method, Req.PathInfo, Res.Status, ExecutionTimeMS]));
    end);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

### Isolamento por Instância (Multi-Instance)

Se sua aplicação utiliza a arquitetura Multi-Instance (`THorseInstance`), você pode registrar ganchos de telemetria isolados por porta/instância. O Horse resolve polimorficamente a instância correta associada à requisição ativa:

```delphi
uses
  Horse, System.SysUtils;

var
  LInstance1, LInstance2: THorseInstance;
begin
  LInstance1 := THorseInstance.Create;
  LInstance1.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      Writeln(Format('[Instance 1 - Port %d] Latency: %.2f ms', [Req.RawWebRequest.ServerPort, ExecutionTimeMS]));
    end);
  LInstance1.Get('/service1', ...);

  LInstance2 := THorseInstance.Create;
  LInstance2.AddOnTelemetry(
    procedure(const Req: THorseRequest; const Res: THorseResponse; const ExecutionTimeMS: Double)
    begin
      Writeln(Format('[Instance 2 - Port %d] Latency: %.2f ms', [Req.RawWebRequest.ServerPort, ExecutionTimeMS]));
    end);
  LInstance2.Get('/service2', ...);
end.
```

### Garantia de Performance

* **Zero-Allocation:** O controle de tempo utiliza `TStopwatch` alocado diretamente na stack da thread, não gerando pressão no Garbage Collector (FPC/Lazarus) ou estresse de heap/alocação de memória no Delphi.
* **Segurança e Isolamento:** O gancho de telemetria é acionado de forma síncrona dentro da seção `finally` do roteamento físico, garantindo que o tempo total capture middlewares, processamento da rota e qualquer erro gerado no pipeline.

---

## Veja Também
- [Ecossistema de Middlewares](./middleware-ecosystem.pt-BR.md)
- [Criando um Middleware](./writing-middleware.pt-BR.md)
