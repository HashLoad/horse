# Cheatsheet de Deploy

*Leia em [English](./deployment.md) ou [Português (BR)](./deployment.pt-BR.md).*

Mesmo código Horse, sete formatos de deploy. Esta página é a referência rápida; para o racional e exemplos de código mais longos, veja [Providers e Tipos de aplicação §8](./providers.pt-BR.md#8-rodando-o-crosssocket-em-cada-tipo-de-aplicação).

---

## O padrão de quatro passos

Todo formato nesta página segue os mesmos quatro passos:

1. **Defina `HORSE_PROVIDER_CROSSSOCKET`** em Project Options → Conditional Defines, mais o `HORSE_APPTYPE_*` correspondente se quiser a unit de conveniência produto cruzado (desde o PATCH-HORSE-2). O alias legado `HORSE_CROSSSOCKET` continua funcionando pra compatibilidade.
2. Escolha o **tipo de projeto** pro formato desejado.
3. Chame `THorse.Listen(port)` do **hook de ciclo de vida** certo daquele formato.
4. Chame `THorse.StopListen` do **hook de shutdown** pro CrossSocket drenar requisições em voo.

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

### Console (Delphi) — `SetConsoleCtrlHandler` pro stop

```pascal
{$APPTYPE CONSOLE}
function CtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  if dwCtrlType in [CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT, CTRL_SHUTDOWN_EVENT] then
  begin THorse.StopListen; Result := True; end
  else Result := False;
end;
begin
  SetConsoleCtrlHandler(@CtrlHandler, True);
  THorse.Listen(9000);
end.
```

### VCL (Delphi) — `FormCreate` / `FormClose`

```pascal
procedure TfrmMain.FormCreate(Sender: TObject);  begin THorse.Listen(9000); end;
procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
                                                begin THorse.StopListen;   end;
```

### Daemon Linux (Delphi) — signal POSIX + systemd

```pascal
{$APPTYPE CONSOLE}
{$IFDEF LINUX}
procedure HandleSignal(ASignal: Integer); cdecl;
begin THorse.StopListen; end;
{$ENDIF}
begin
  {$IFDEF LINUX} signal(SIGTERM, @HandleSignal); signal(SIGINT, @HandleSignal); {$ENDIF}
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
    procedure begin THorse.Listen(9000); end);
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
  {$IFDEF UNIX} fpSignal(SIGTERM, @HandleSignal); fpSignal(SIGINT, @HandleSignal); {$ENDIF}
  THorse.Listen(9000);
end.
```

Unit do systemd idêntica ao daemon Linux Delphi acima.

### LCL (FPC / Lazarus) — `FormCreate` / `FormClose`

```pascal
procedure TfrmMain.FormCreate(Sender: TObject);
begin THorse.Listen(9000); end;
procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin THorse.StopListen; end;
```

### HTTPApplication FPC — binário FPC formato console

Mesmo código do daemon Linux FPC — o `THorse.Listen` é dono do loop; não chame `fphttpapp.Application.Run`. Se uma biblioteca espera que `TFPHTTPApplication` exista, instancie ele mas deixe o `Run` quieto.

---

## Mapeamento de sinais de stop

O sinal de shutdown varia por OS e supervisor. Todos os casos acabam no mesmo lugar: `THorse.StopListen` → drenagem do contador de requisições ativas do CrossSocket SEC-30 → `Listen` retorna → processo sai limpo.

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
| Serviço Windows trava em "Starting" | `ServiceStart` bloqueia porque `Listen` foi chamado direto na thread do SCM | Envelope o `Listen` num `TThread.CreateAnonymousThread` (veja o snippet TService). |
| systemd reporta "main process exited, code=killed, status=15/TERM" | Processo não pegou `SIGTERM` — systemd teve que escalar | Instale o handler de sinal POSIX pro binário sair limpo com `0`. |
| `Address already in use` após restart | Processo anterior segurou o socket e foi force-killed (sem drenagem limpa) | Sempre chame `StopListen`; pra Docker, configure `--stop-grace-period=30s`. |
| Requisições em voo derrubadas no shutdown | `Listen` retornou imediatamente após `StopListen` sem aguardar o contador de requisições ativas | O SEC-30 do CrossSocket já cuida disso — garanta que está no `horse-provider-crosssocket >= 1.0.4`. |

---

## Deploy multi-OS

A maioria dos times entrega o mesmo código Horse como **daemon Linux** em produção e um **Serviço Windows** ou **Console** binário pra dev. Os Conditional Defines ficam iguais — só muda o target do projeto (Win64 / Linux64). Build duas vezes, uma por OS.

Config compartilhada de CI:

```yaml
jobs:
  build-linux:
    runs-on: ubuntu-latest
    steps: [...]
    env:
      CONFIGURATION: Release
      PLATFORM: Linux64
      DEFINES: HORSE_CROSSSOCKET

  build-windows:
    runs-on: windows-latest
    steps: [...]
    env:
      CONFIGURATION: Release
      PLATFORM: Win64
      DEFINES: HORSE_CROSSSOCKET
```

O mesmo `.dpr` compila nos dois — só a casca de deploy (unit systemd vs. registro de serviço SCM) muda por OS.

---

## Veja também

- [Providers e Tipos de aplicação](./providers.pt-BR.md) — o modelo arquitetural completo, inclusive §8 com código anotado pra cada formato.
- [Primeiros passos](./getting-started.pt-BR.md) — seu primeiro servidor Horse antes de decidir o formato de deploy.
- [Suporte de Compilador](./compiler-support.pt-BR.md) — exigências de versão Delphi / FPC e a matriz de plataformas.
- [`horse-provider-crosssocket`](https://github.com/freitasjca/horse-provider-crosssocket) — config (TLS, limites de body, número de threads IO) do Provider CrossSocket.
