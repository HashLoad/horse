---
name: horse-providers
description: Guide to selecting and configuring Horse server adapters (Indy console, CGI, ISAPI, Daemon, HTTP.sys).
---

# Horse Providers

## Available Provedores (Server Adapters)
Horse supports multiple execution providers. To change the provider, simply include the corresponding unit in your `.dpr` uses clause.

*   **`Horse.Provider.Console`** (Default - Indy-based): Launches a standalone console application listening on a TCP port. Best for local development and microservices.
*   **`Horse.Provider.Daemon`**: Integrates Horse with Windows Services or Linux Daemons.
*   **`Horse.Provider.HTTPsys`**: Integrates with the native Windows HTTP Server API (HTTP.sys). Provides high performance and native SSL/HTTPS support on Windows without needing Apache/Nginx.
*   **`Horse.Provider.CGI` / `Horse.Provider.ISAPI`**: For deployment inside IIS (Internet Information Services) or Apache on Windows.
*   **`Horse.Provider.Apache`**: Native Apache web server module.

---

## Configuration Example
```pascal
program MyAPI;

{$APPTYPE CONSOLE}

uses
  // Include the target provider unit. Only ONE provider should be active.
  Horse,
  Horse.Provider.HTTPsys; 

begin
  THorse.Get('/health',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('healthy');
    end);

  // Starts the HTTPsys server
  THorse.Listen(8080);
end.
```
