---
name: horse-ssl-tls
description: Guide to enabling SSL/TLS, configuring HTTPS, handling certificates, and securing transport layers.
---

# Horse SSL / TLS (HTTPS)

In production environments, always encrypt data in transit using SSL/TLS. Depending on the transport provider you select, the configuration for HTTPS differs.

---

## 1. Native HTTPS with HTTP.sys (Windows Kernel-Mode)
When using the native `Horse.Provider.HTTPsys` provider on Windows, the SSL handshake is handled entirely by the operating system kernel. 

### Step 1: Bind SSL Cert in Windows
You do **not** configure certificates inside your Delphi code. Instead, bind your SSL certificate (using its thumbprint) to the target port using the Windows command line tool `netsh` (requires Administrator privileges):

```cmd
netsh http add sslcert ipport=0.0.0.0:443 certhash=YOUR_CERT_THUMBPRINT appid={YOUR-APP-GUID}
```

### Step 2: Configure HTTP.sys in Delphi
Just start the Horse server normally. HTTP.sys will route HTTPS traffic on the bound port to your application automatically:

```pascal
program SecureAPI;

{$APPTYPE CONSOLE}

uses
  Horse,
  Horse.Provider.HTTPsys;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  // Starts the server. HTTP.sys routes both HTTP/HTTPS automatically depending on OS bindings.
  THorse.Listen(443);
end.
```

---

## 2. SSL/TLS with OverbyteICS Provider (ICS)
The [`horse-provider-ics`](https://github.com/freitasjca/horse-provider-ics) provider handles OpenSSL 1.1.1 / 3.x / 4.x directly within the application process.

### Configuration
Load your certificate `.pem` or `.crt` files and private key files during initialization:

```pascal
uses
  Horse,
  Horse.Provider.ICS;

begin
  // Set up SSL certificate paths and properties fluently
  THorse.Provider.ICS.SSLSecured := True;
  THorse.Provider.ICS.SSLCertFile := 'C:\certs\server.crt';
  THorse.Provider.ICS.SSLPrivateKeyFile := 'C:\certs\server.key';
  THorse.Provider.ICS.SSLPassword := 'my_private_key_password';
  
  // Optionally enable Server-side Mutual TLS (mTLS)
  THorse.Provider.ICS.SSLVerifyPeer := True;
  THorse.Provider.ICS.SSLCACertFile := 'C:\certs\ca.crt'; // CA certificate to verify clients

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(8443);
end.
```

---

## 3. SSL/TLS with CrossSocket Provider
For the async CrossSocket provider, configure the TLS context in your server instance:

```pascal
uses
  Horse,
  Horse.Provider.CrossSocket;

begin
  // Enable SSL/TLS and load certificates (requires OpenSSL library binaries on PATH)
  THorse.Provider.CrossSocket.SSLSecured := True;
  THorse.Provider.CrossSocket.SSLCertFile := 'C:\certs\server.crt';
  THorse.Provider.CrossSocket.SSLPrivateKeyFile := 'C:\certs\server.key';

  THorse.Listen(8443);
end.
```

---

## 4. Best Practices for HTTPS
1.  **Cipher Suites**: Enforce modern, secure cipher suites (TLS 1.2 and TLS 1.3 only). Disable legacy TLS 1.0/1.1 protocols.
2.  **HSTS (HTTP Strict Transport Security)**: Add the `Strict-Transport-Security` header to response headers to force browsers to connect only via HTTPS.
3.  **Port Standard**: Always host public production APIs on standard HTTPS port `443`.
