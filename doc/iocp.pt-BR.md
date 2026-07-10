# Provider de Transporte IOCP

*Leia em [English](./iocp.md) ou [Português (BR)](./iocp.pt-BR.md).*

O Provider **IOCP** é um driver de transporte HTTP assíncrono nativo para Windows embutido diretamente no core do Horse. Ele utiliza a tecnologia de **Portas de Conclusão de E/S (Input/Output Completion Ports - IOCP)** do Windows e a API de alto desempenho **Winsock2** (`AcceptEx` / `GetAcceptExSockaddrs`) para fornecer extrema escalabilidade e vazão (throughput) para tipos de aplicações self-hosted executadas no Windows (Console, VCL ou Serviços Windows/Daemons).

---

## ⚡️ Início Rápido

Para ativar o provider de transporte IOCP em sua aplicação:

1. Adicione a diretiva condicional de compilação `HORSE_PROVIDER_IOCP` nas configurações de projeto do seu executável.
2. Compile o projeto no Windows (utilizando Delphi ou Lazarus/FPC).

```delphi
program MeuServidor;

uses
  Horse;

begin
  // (Opcional) Habilita o Roteador Radix de alta performance
  THorse.UseRadixRouter;

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

---

## ⚙️ Arquitetura e Modelo de Concorrência

O provider IOCP foi projetado para obter o máximo desempenho de rede com baixíssimo uso de recursos e threads do sistema operacional:

*   **Aceites Assíncronos (`AcceptEx`)**: Ao contrário do provider Indy clássico, que bloqueia uma thread aguardando conexões entrantes, o provider IOCP agenda múltiplos pedidos de aceite assíncronos em lote diretamente no kernel do Windows via `AcceptEx`. Isso permite ao servidor absorver picos repentinos de novas conexões instantaneamente.
*   **Laço de Eventos Assíncronos**: Um pequeno grupo fixo de threads de trabalho do IOCP (`TIocpWorkerThread` — dimensionado tipicamente para `CPUCount * 2`) monitora a porta de conclusão do Windows através da API `GetQueuedCompletionStatus`. Estas threads permanecem em estado de suspensão (sem gastar CPU) quando não há tráfego de rede e acordam imediatamente quando pacotes de dados chegam.
*   **Despacho via Thread Pool do Windows**: Assim que o cabeçalho completo de uma requisição HTTP é recebido e parseado pelo scanner zero-allocation do Horse, a execução da cadeia de middlewares e o handler de rota correspondente são despachados para a Thread Pool do Windows (`TThread.Queue`). Isso garante que operações bloqueantes executadas por você no código do handler (como consultas ao banco de dados ou processamentos longos) não travem o laço de rede principal do IOCP, mantendo a responsividade do servidor sob estresse.
*   **Zero Dependências Externas**: O provedor se vincula diretamente às APIs de kernel do Windows e bibliotecas Winsock nativas (`ws2_32.dll`), sem exigir qualquer distribuição de DLLs adicionais ou dependência de pacotes de terceiros.

---

## 📊 Recursos e Compatibilidade

| Recurso | Suporte | Observação |
|---|:---:|---|
| **Sistema Operacional** | Windows | Requer Windows Vista / Server 2008 ou posterior. |
| **Compiladores** | Delphi & Lazarus | Funciona de forma transparente em ambos os compiladores Pascal. |
| **Tipos de Aplicação** | Console, VCL, Daemon | Oferece suporte completo aos formatos self-hosted de binário. |
| **Keep-Alive** | ✔ | Reutilização de conexões TCP é gerenciada nativamente. |
| **Radix Router** | ✔ | Pode ser combinado com `HORSE_RADIX_ROUTER` para máxima performance de rotas. |
| **SSL/TLS** | ✘ | Para criptografia TLS no Windows com o provider IOCP, recomendamos posicionar um proxy reverso (como IIS, Nginx ou Caddy) à frente do executável do Horse, ou usar os providers CrossSocket/ICS. |
