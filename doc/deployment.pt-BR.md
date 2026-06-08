# Cheatsheet de Deploy

*Leia em [English](./deployment.md) ou [Português (BR)](./deployment.pt-BR.md).*

Mesmo código Horse, sete formatos de deploy, dois transportes assíncronos intercambiáveis (CrossSocket e mORMot2). Esta página é a referência rápida; para o racional e exemplos de código mais longos, veja [Providers e Tipos de aplicação §8 (CrossSocket)](./providers.pt-BR.md#8-rodando-o-crosssocket-em-cada-tipo-de-aplicação) ou [§9 (mORMot2)](./providers.pt-BR.md#9-rodando-o-mormot2-em-cada-tipo-de-aplicação).

---

## O padrão de quatro passos

Todo formato nesta página segue os mesmos quatro passos:

1. **Defina exatamente um transporte** em Project Options → Conditional Defines:
   - `HORSE_PROVIDER_CROSSSOCKET` para o Provider CrossSocket, **ou**
   - `HORSE_PROVIDER_MORMOT` para o Provider mORMot2.

   Adicione o `HORSE_APPTYPE_*` correspondente se quiser a unit de conveniência produto cruzado (desde o PATCH-HORSE-2). O alias legado `HORSE_CROSSSOCKET` continua funcionando pra compatibilidade; não existe alias legado para mORMot. Os dois defines de Provider são mutuamente exclusivos — o `Horse.pas` rejeita a combinação em tempo de compilação.
2. Escolha o **tipo de projeto** pro formato desejado.
3. Chame `THorse.Listen(port)` do **hook de ciclo de vida** certo daquele formato.
4. Chame `THorse.StopListen` do **hook de shutdown** para que o Provider esgote as requisições ativas.

A verificação em runtime que dirige o comportamento do formato é o `IsConsole`:

- **`IsConsole = True`** (binário console, `{$APPTYPE CONSOLE}`) → `Listen` bloqueia a thread chamadora; o hook de shutdown desbloqueia via `StopListen`.
- **`IsConsole = False`** (VCL / LCL / TService) → `Listen` inicia as threads de IO e retorna imediatamente; a thread chamadora fica livre pro message loop da GUI ou pro loop de controle do serviço.

---

## Visão geral

| Formato | `{$APPTYPE CONSOLE}` | Tipo de projeto | `Listen` de | `StopListen` de |
|---|:---:|---|---|---|
| Console (Delphi) | ✔ | Console Application | `begin … end.` | `SetConsoleCtrlHandler` |
| VCL (Delphi) | ❌ | VCL Forms Application | `FormCreate` | `FormClose` |
| Daemon Linux (Delphi) | ✔ | Console (target Linux64) | `begin … end.` | POSIX `signal(SIGTERM, …)` |
| Serviço Windows (Delphi) | ❌ | Service Application | `ServiceStart` (worker thread) | `ServiceStop` |
| Daemon Linux (FPC) | ✔ | Console (FPC) | `begin … end.` | `fpSignal(SIGTERM, …)` |
| LCL desktop (FPC) | ❌ | Lazarus Application | `FormCreate` | `FormClose` |
| HTTPApplication FPC | ✔ | Console (FPC) | `begin … end.` | `fpSignal(SIGTERM, …)` |

---

## Código mínimo por formato

### Console (Delphi) — `SetConsoleCtrlHandler` para interrupção

```pascal
{$APPTYPE CONSOLE}
function CtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  if dwCtrlType in [CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT, CTRL_SHUTDOWN_EVENT] then
  begin 
    THorse.StopListen; 
    Result := True; 
  end
  else 
    Result := False;
end;
begin
  SetConsoleCtrlHandler(@CtrlHandler, True);
  THorse.Listen(9000);
end.
```

### VCL (Delphi) — `FormCreate` / `FormClose`

```pascal
procedure TfrmMain.FormCreate(Sender: TObject);  
begin 
  THorse.Listen(9000); 
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin 
  THorse.StopListen;   
end;
```

### Daemon Linux (Delphi) — signal POSIX + systemd

```pascal
{$APPTYPE CONSOLE}
{$IFDEF LINUX}
procedure HandleSignal(ASignal: Integer); cdecl;
begin 
  THorse.StopListen; 
end;
{$ENDIF}
begin
  {$IFDEF LINUX} 
  signal(SIGTERM, @HandleSignal); 
  signal(SIGINT, @HandleSignal); 
  {$ENDIF}
  THorse.Listen(9000);
end.
```

Unit do systemd (`/etc/systemd/system/myhorse.service`):

```ini
[Unit]
After=network.target
[Service]
Type=simple
ExecStart=/opt/myhorse/MyDaemon
Restart=on-failure
[Install]
WantedBy=multi-user.target
```

### Serviço Windows (Delphi) — TService

```pascal
procedure TMyHorseService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  FListenerThread := TThread.CreateAnonymousThread(
    procedure 
    begin 
      THorse.Listen(9000); 
    end);
  FListenerThread.FreeOnTerminate := False;
  FListenerThread.Start;
  Started := True;
end;

procedure TMyHorseService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  THorse.StopListen;
  FListenerThread.WaitFor;
  FreeAndNil(FListenerThread);
  Stopped := True;
end;
```

Instale / desinstale pelos verbos padrão do SCM:

```bat
MyHorseServer.exe /install
sc start MyHorseService
sc stop  MyHorseService
MyHorseServer.exe /uninstall
```

Alternativa mais simples sem escrever um TService: construa o binário Console acima e embrulhe com [NSSM](https://nssm.cc/) — `nssm install MyHorseService C:\path\to\Console.exe`. O NSSM envia `Ctrl+Break` no stop, que o `CtrlHandler` do formato Console pega.

### Daemon Linux (FPC) — `fpSignal` + systemd

```pascal
{$MODE DELPHI}{$H+}
{$APPTYPE CONSOLE}
{$IFDEF UNIX}
procedure HandleSignal(ASignal: cint); cdecl;
begin THorse.StopListen; end;
{$ENDIF}
begin
  {$IFDEF UNIX} 
  fpSignal(SIGTERM, @HandleSignal); 
  fpSignal(SIGINT, @HandleSignal); 
  {$ENDIF}
  THorse.Listen(9000);
end.
```

Unit do systemd idêntica ao daemon Linux Delphi acima.

### LCL (FPC / Lazarus) — `FormCreate` / `FormClose`

```pascal
procedure TfrmMain.FormCreate(Sender: TObject);
begin 
  THorse.Listen(9000); 
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin 
  THorse.StopListen; 
end;
```

### HTTPApplication FPC — binário FPC formato console

Mesmo código do daemon Linux FPC — o `THorse.Listen` é dono do loop; não chame `fphttpapp.Application.Run`. Se uma biblioteca espera que `TFPHTTPApplication` exista, instancie ele mas deixe o `Run` quieto.

---

## Mapeamento de sinais de stop

O sinal de shutdown varia por OS e supervisor. Todos os casos acabam no mesmo lugar: `THorse.StopListen` → drenagem do contador de requisições ativas SEC-30 do Provider ativo (implementada de forma idêntica em `horse-provider-crosssocket` e `horse-provider-mormot`) → `Listen` retorna → processo sai limpo.

| Supervisor | Sinal enviado | Como seu código pega |
|---|---|---|
| Terminal (Ctrl-C) | `SIGINT` / `CTRL_C_EVENT` | `SetConsoleCtrlHandler` (Windows) / `signal` / `fpSignal` (POSIX) |
| systemd | `SIGTERM` | Handler POSIX `signal` / `fpSignal` |
| Windows SCM | `SERVICE_CONTROL_STOP` | Evento `TService.OnStop` |
| NSSM | `Ctrl+Break` (via console) | `SetConsoleCtrlHandler` pegando `CTRL_BREAK_EVENT` |
| Fechar janela VCL / LCL | `WM_CLOSE` | Evento `TForm.OnClose` |
| Docker `docker stop` | `SIGTERM` (depois `SIGKILL` após período de graça) | Handler POSIX `signal` / `fpSignal` — dê < 10 s pro seu handler retornar |

---

## Armadilhas comuns

| Sintoma | Causa | Correção |
|---|---|---|
| Processo sai imediatamente após o start | Binário console sem handler de sinal chega ao `end.` depois que `Listen` retorna | Adicione o setup de `CtrlHandler` / `signal` *antes* do `Listen`. |
| Form VCL trava no startup | `{$APPTYPE CONSOLE}` deixado por engano no .dpr | Remova essa diretiva; apps VCL/LCL precisam de `IsConsole = False`. |
| Serviço Windows trava em "Starting" | `ServiceStart` bloqueia porque `Listen` foi chamado direto na thread do SCM | Encapsule o `Listen` num `TThread.CreateAnonymousThread` (veja o snippet TService). |
| systemd reporta "main process exited, code=killed, status=15/TERM" | Processo não pegou `SIGTERM` — systemd teve que escalar | Instale o handler de sinal POSIX pro binário sair limpo com `0`. |
| `Address already in use` após restart | Processo anterior segurou o socket e foi force-killed (sem drenagem limpa) | Sempre chame `StopListen`; pra Docker, configure `--stop-grace-period=30s`. |
| Requisições em andamento perdidas no shutdown | `Listen` retornou imediatamente após `StopListen` sem aguardar o contador de requisições ativas | O SEC-30 já cuida disso — garanta que está no `horse-provider-crosssocket >= 1.0.4` contra um `winddriver/Delphi-Cross-Socket` recente (ou o fork [`freitasjca/Delphi-Cross-Socket v1.0.3`](https://github.com/freitasjca/Delphi-Cross-Socket/releases/tag/v1.0.3)), ou qualquer release do `horse-provider-mormot`, onde o SEC-30 está integrado desde o primeiro dia. |
| **~60 % de HTTP 500 sob carga no Indy** (`EWebBrokerException: "Maximum number of concurrent connections exceeded"`), só com middleware de cabeçalhos de resposta + keep-alive + concorrência ≥ ~40 | O `MaxConnections` do pool de módulos do WebBroker tinha default **32** | **Corrigido por padrão** — os providers Indy agora elevam o teto para `DEFAULT_MAX_CONNECTIONS` (1024) quando `THorse.MaxConnections` não é definido. Defina `THorse.MaxConnections := N` para ir além. (Somente Indy; ver [Providers §10.2](./providers.pt-BR.md#102-defaults-de-conexão-do-provider-indy--maxconnections-e-listenqueue).) |
| Conexões recusadas/descartadas em rajadas no Indy | O `ListenQueue` do Indy tinha default **15** | **Corrigido por padrão** — os providers Indy agora usam `DEFAULT_LISTEN_QUEUE` (511) quando `THorse.ListenQueue` não é definido; aumente o `somaxconn` do SO para acompanhar em concorrência muito alta. |

---

## Deploy multi-OS

A maioria das equipes entrega o mesmo código Horse como **daemon Linux** em produção e um **Serviço Windows** ou **Console** binário pra dev. Os Conditional Defines ficam iguais — só muda o target do projeto (Win64 / Linux64). Build duas vezes, uma por OS.

Config compartilhada de CI:

```yaml
jobs:
  build-linux:
    runs-on: ubuntu-latest
    steps: [...]
    env:
      CONFIGURATION: Release
      PLATFORM: Linux64
      DEFINES: HORSE_PROVIDER_CROSSSOCKET

  build-windows:
    runs-on: windows-latest
    steps: [...]
    env:
      CONFIGURATION: Release
      PLATFORM: Win64
      DEFINES: HORSE_PROVIDER_CROSSSOCKET
```

O mesmo `.dpr` compila nos dois — só a casca de deploy (unit systemd vs. registro de serviço SCM) muda por OS.

---

## HTTPS / TLS em runtime — o que entregar por OS

Tanto `HORSE_PROVIDER_CROSSSOCKET` quanto `HORSE_PROVIDER_MORMOT` usam **OpenSSL** para HTTPS — eles fazem `dlopen` / `LoadLibrary` da biblioteca compartilhada do sistema no startup. A pilha de transporte fica no seu binário; o OpenSSL **não** é linkado estaticamente por padrão. Planeje o deploy considerando isso.

### Linux

Instale o OpenSSL pelo gerenciador de pacotes da distro pra que `libssl.so` e `libcrypto.so` fiquem no caminho do loader:

```bash
# Debian / Ubuntu (22.04+, OpenSSL 3.x)
apt install libssl3 libcrypto3

# Debian / Ubuntu (20.04, OpenSSL 1.1.x)
apt install libssl1.1

# RHEL / Rocky / Alma 9.x  (OpenSSL 3.x)
dnf install openssl-libs

# Alpine
apk add openssl libcrypto3 libssl3
```

Os dois providers aceitam tanto 1.1.x quanto 3.x — eles testam no startup. Se o loader não encontrar nenhum dos dois, o binário ainda roda, mas `SSLEnabled := True` falha no `Listen` com um erro claro de "no SSL backend available".

Para deploys em containers mínimos ou ambientes air-gapped onde não dá pra contar com os pacotes da distro:
- **CrossSocket:** entregue o `libssl.so` + `libcrypto.so` compatíveis junto do binário e declare na seção `[Service]` da unit systemd: `Environment="LD_LIBRARY_PATH=/opt/seuapp"`.
- **mORMot2:** o pacote `mormot2static` inclui uma variante com link estático pra algumas plataformas (`mormot2static/static/x86_64-linux` no FPC) — veja o [samples/tests/README do horse-provider-mormot](https://github.com/freitasjca/horse-provider-mormot/blob/master/samples/tests/README.md) para o setup completo de Search-path.

### Windows

Entregue as DLLs do OpenSSL **junto do `.exe`** (não jogue em `C:\Windows\System32` e não use uma pasta global no PATH — co-localizar com o binário evita que outros apps com OpenSSL bundled sequestrem o load):

| Versão OpenSSL | Nomes das DLLs (por arquitetura) |
|---|---|
| 1.1.x | `libssl-1_1-x64.dll`, `libcrypto-1_1-x64.dll` (Win64); tire o `-x64` pra Win32 |
| 3.x | `libssl-3-x64.dll`, `libcrypto-3-x64.dll` (Win64); tire o `-x64` pra Win32 |

A fonte padrão são os [builds oficiais OpenSSL para Windows](https://wiki.openssl.org/index.php/Binaries) ou os [instaladores da SLProWeb](https://slproweb.com/products/Win32OpenSSL.html). Escolha **uma** versão e use ela em todos os ambientes — código que faz dynamic-load de `libssl-1_1.dll` não roda num host que só tem `libcrypto-3.dll`, e as duas não convivem no mesmo processo.

Em deploy como Serviço Windows, as DLLs precisam estar na mesma pasta do `.exe` do serviço — o SCM **não** herda o `PATH` do usuário.

### Armadilha comum — crash por descasamento de versão

| Sintoma | Causa | Correção |
|---|---|---|
| `EOSError: failed to load libssl` no Linux | Nenhum pacote OpenSSL instalado, ou container minimal só com `libc` | Instale `libssl3` / `libssl1.1` (Linux) ou copie as DLLs junto do binário (Windows). |
| HTTPS funciona em dev, quebra em prod com "wrong version number" | Box de dev tem OpenSSL 3.x; box de prod tem 1.1.x (ou vice-versa) — features de negociação TLS diferem | Padronize numa família de versão em todos os ambientes. Se tiver que suportar as duas, entregue as DLLs (Windows) ou use a variante estática do `mormot2static` (mORMot2, Linux). |
| SIGSEGV aleatório no handshake TLS no Linux | Duas cópias do `libcrypto` carregadas ao mesmo tempo (sistema 3.x + uma 1.1.x bundled diferente em `LD_LIBRARY_PATH`) | Garanta que só uma ABI do OpenSSL fica alcançável. |

---

## Veja também

- [Providers e Tipos de aplicação](./providers.pt-BR.md) — o modelo arquitetural completo, inclusive §8 (CrossSocket) e §9 (mORMot2) com código anotado pra cada formato.
- [Primeiros passos](./getting-started.pt-BR.md) — seu primeiro servidor Horse antes de decidir o formato de deploy.
- [Suporte de Compilador](./compiler-support.pt-BR.md) — exigências de versão Delphi / FPC e a matriz de plataformas.
- [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket) — config (TLS, limites de body, número de threads IO) do Provider CrossSocket.
- [`horse-provider-mormot`](https://github.com/freitasjca/horse-provider-mormot) — config (thread pool, máximo de bytes do body, timeout de drenagem, banner do servidor) do Provider mORMot2.
