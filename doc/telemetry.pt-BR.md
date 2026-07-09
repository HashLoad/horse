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

## Veja Também
- [Ecossistema de Middlewares](./middleware-ecosystem.pt-BR.md)
- [Criando um Middleware](./writing-middleware.pt-BR.md)
