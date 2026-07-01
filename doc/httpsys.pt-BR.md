# Provider HTTP.sys para o Horse

O provedor HTTP.sys utiliza a pilha de protocolos HTTP em modo kernel do Windows (`http.sys`). Ele é embutido diretamente no core do Horse — nenhum pacote externo é necessário. Como o processamento de sockets, o parseamento de pacotes HTTP e o gerenciamento de fila de requisições acontecem diretamente no Ring 0 (espaço de kernel), o overhead de troca de contexto (context-switch) de CPU é drasticamente reduzido, permitindo altíssima vazão e estabilidade extrema no Windows.

Nos Defines de Compilação do seu projeto: `HORSE_PROVIDER_HTTPSYS`.

---

## ⚙️ Propriedades e Arquitetura

- **Exclusivo para Windows:** Nativo no Windows 7/Server 2008 e versões superiores.
- **Parseamento em Modo Kernel:** O processamento de rede e cabeçalhos HTTP é feito pelo próprio kernel do sistema operacional, poupando CPU no espaço de usuário (User-Space).
- **Compartilhamento de Porta (Port Sharing):** Permite que múltiplos processos/aplicações compartilhem a mesma porta física (ex: porta 80 ou 443) registrando diferentes prefixos de URL (ex: `http://+:80/app1/` e `http://+:80/app2/`).
- **Pool de Threads Estático:** Cria um pool pré-alocado de threads trabalhadoras persistentes para processar as requisições de forma thread-safe, eliminando overhead de criação e destruição de threads em hot paths.
- **Eventos de Reset Automático:** Utiliza sinalização thread-safe otimizada para evitar que as threads do pool entrem em busy-waiting ou consumam CPU de forma desnecessária.
- **Desempenho:** Atinge facilmente ~12.000+ req/s com latências extremamente baixas.

---

## 🔒 Privilégios Administrativos

Como o HTTP.sys registra prefixos de URL diretamente no kernel do Windows, iniciar seu aplicativo em portas comuns (como 80, 443 ou qualquer outra porta personalizada) exige:
1. **Executar a aplicação como Administrador** (prompt de comando elevado).
2. **Reservar o namespace de URL** para usuários comuns.

Para registrar o namespace de URL uma única vez (ex: porta 9095) para todos os usuários do Windows, execute em um prompt do PowerShell elevado:

```powershell
netsh http add urlacl url=http://+:9095/ user=Todos
# Em sistemas Windows em inglês, substitua "Todos" por "Everyone"
netsh http add urlacl url=http://+:9095/ user=Everyone
```

Para remover a reserva posteriormente:

```powershell
netsh http delete urlacl url=http://+:9095/
```

---

## ⚡ Início Rápido (Quick Start)

1. Adicione `HORSE_PROVIDER_HTTPSYS` aos Defines Condicionais do seu projeto (**Project Options → Delphi Compiler → Conditional defines**).
2. Escreva o código do seu servidor normalmente:

```delphi
program MyServer;

{$APPTYPE CONSOLE}

uses
  Horse;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  // Inicia o listener do HTTP.sys na porta 9095
  THorse.Listen(9095);
end.
```

Nenhuma outra alteração no código do aplicativo é necessária. O framework redireciona automaticamente as chamadas de `THorse.Listen` para o provedor HTTP.sys.
