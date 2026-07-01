# HTTP.sys Provider for Horse

The HTTP.sys provider utilizes the Windows kernel-mode HTTP protocol stack (`http.sys`). It is built directly into the Horse framework — no external packages are required. Since HTTP parsing, queue management, and request dispatching are handled directly in Ring 0 (kernel mode), context-switching overhead is drastically reduced, enabling exceptionally high throughput and stability under Windows.

In your project's Conditional Defines: `HORSE_PROVIDER_HTTPSYS`.

---

## ⚙️ Properties & Architecture

- **Windows-only:** Native to Windows 7/Server 2008 and above.
- **Kernel-Mode Parsing:** Sockets and HTTP protocol processing are managed directly by the kernel, freeing up User-Space CPU.
- **Port Sharing:** Allows multiple applications to share the same port (e.g., port 80/443) by registering distinct URL prefixes (e.g., `http://+:80/app1/` and `http://+:80/app2/`).
- **Static Thread Pool:** Spawns a pre-allocated pool of persistent worker threads to handle requests efficiently without thread-creation overhead on hot paths.
- **Auto-Reset Events:** Uses optimized synchronization mechanisms to eliminate CPU busy-waiting.
- **Performance:** Reaches ~12,000+ req/s with very low latencies.

---

## 🔒 Administrative Privileges

Because HTTP.sys registers URL prefixes directly with the Windows kernel, running your application on ports like 80, 443, or other custom ports requires either:
1. **Running the application as Administrator** (elevated command prompt).
2. **Reserving the URL namespace** for non-administrative users.

To register a namespace once (e.g., port 9095) for all users, execute in an elevated PowerShell command prompt:

```powershell
netsh http add urlacl url=http://+:9095/ user=Todos
# On English Windows systems, replace "Todos" with "Everyone"
netsh http add urlacl url=http://+:9095/ user=Everyone
```

To remove the reservation:

```powershell
netsh http delete urlacl url=http://+:9095/
```

---

## ⚡ Quick Start

1. Add `HORSE_PROVIDER_HTTPSYS` to your project's Conditional Defines (**Project Options → Delphi Compiler → Conditional defines**).
2. Write your server code normally:

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

  // Starts the HTTP.sys listener on port 9095
  THorse.Listen(9095);
end.
```

No other application code changes are required. The framework automatically redirects `THorse.Listen` calls to the HTTP.sys provider.
