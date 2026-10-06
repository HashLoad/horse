unit Horse.Provider.Config;

// =============================================================================
//  Horse.Provider.Config  —  NEW FILE (Horse fork for CrossSocket provider)
// =============================================================================
//  Upstream: https://github.com/HashLoad/horse  (tag 3.1.9)
//  Fork:     https://github.com/your-org/horse
//
//  Purpose
//  -------
//  Holds THorseCrossSocketConfig so it can be used by BOTH:
//    • Horse.Provider.Abstract.pas  (declares ListenWithConfig parameter type)
//    • Horse.Provider.CrossSocket.Server.pas  (implements the config)
//
//  Without this unit, one of those two files would have to use the other,
//  creating a circular dependency the Delphi compiler cannot resolve.
//
//  This file has NO dependencies on either Horse.Provider.Abstract or
//  Horse.Provider.CrossSocket — it is a pure data unit.
//
//  The identical record is also declared in Horse.Provider.CrossSocket.Server
//  in the provider repository, which re-exports it for backward compatibility.
//  When both units are in the search path the compiler will find this canonical
//  version first (provider places its src/ after horse/src/).
// =============================================================================

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

const
  // [SEC-1] Safe defaults
  DEFAULT_MAX_HEADER_SIZE  = 8192;             // 8 KB — matches nginx default
  DEFAULT_MAX_BODY_SIZE    = 4 * 1024 * 1024;  // 4 MB
  DEFAULT_IO_THREADS       = 0;                // 0 = library picks (CPU count)
  // [SEC-6]
  DEFAULT_DRAIN_TIMEOUT_MS = 5000;             // ms
  // Compression
  DEFAULT_MIN_COMPRESS_SIZE = 512;             // bytes — matches CrossSocket MIN_COMPRESS_SIZE

  // Indy/WebBroker self-hosted provider defaults (Console / Daemon / VCL).
  // Applied only when THorse.MaxConnections / THorse.ListenQueue are left unset.
  // [FIX-MAXCONN] WebBroker caps concurrent web-module activations at 32 by default
  //   (Web.WebReq.pas) — under keep-alive + concurrency >= ~40 that returns ~60% HTTP 500.
  //   Raising the module-pool ceiling to a sane default makes the out-of-the-box build safe.
  DEFAULT_MAX_CONNECTIONS  = 1024;
  // [FIX-LISTENQUEUE] Indy's IdListenQueueDefault is 15 — too low for concurrent
  //   connection bursts (dropped/refused connections). 511 mirrors nginx's backlog
  //   (the OS clamps it to net.core.somaxconn / SOMAXCONN if lower).
  DEFAULT_LISTEN_QUEUE     = 511;


type
  // Minimum TLS protocol version a provider must accept (SSLMinVersion).
  // Ordinal 0 is "no override", so a zero-initialised record keeps the
  // provider's / TLS library's own floor.
  THorseTlsMinVersion = (
    htvDefault,   // provider / TLS library default - nothing is configured
    htvTLS12,     // TLS 1.2 or newer (TLS 1.3 still allowed)
    htvTLS13      // TLS 1.3 only
  );

  THorseCrossSocketConfig = record

    // IO model
    IoThreads:       Integer;  // [SEC-2] 0 = library default (recommended)

    // ── Timeouts ─────────────────────────────────────────────────────────
    KeepAliveTimeout: Integer;
    // seconds; 0 = disable keep-alive entirely.
    // Default: 30
    // NOTE: CrossSocket does not currently expose a KeepAlive timeout
    // property.  This field is reserved for future use.

    ReadTimeout: Integer;
    // seconds; enforced by CrossSocket at the socket layer.
    // Mitigates slow-HTTP (Slowloris) attacks.
    // Default: 20  — never leave at 0 (would be unlimited).
    // NOTE: CrossSocket does not currently expose a ReadTimeout property.
    // This field is reserved for future use.

    DrainTimeoutMs: Integer;
    // milliseconds to wait for in-flight requests to complete when
    // THorseCrossSocketServer.Stop is called.  After this timeout the
    // server proceeds with shutdown regardless.
    // Default: 5000

    // ── Request size limits ───────────────────────────────────────────────
    // Size limits [SEC-1]
    MaxHeaderSize: Integer;
    // Maximum size of all request headers combined, in bytes.
    // Matches the nginx default.
    // Default: 8192  (8 KB)

    // Size limits [SEC-1]
    MaxBodySize: Int64;
    // Maximum request body size in bytes.  CrossSocket rejects bodies
    // larger than this with 413 before the Horse pipeline is entered.
    // Default: 4194304  (4 MB)

    // ── Connection ceiling ────────────────────────────────────────────────
    MaxConnections: Integer;
    // Maximum number of simultaneous open connections.
    // Prevents file-descriptor exhaustion under a connection-flood DoS.
    // Default: 10000
    // NOTE: CrossSocket does not currently expose a MaxConnections property.
    // This field is reserved for future use.

    // ── Compression ───────────────────────────────────────────────────────
    Compressible: Boolean;
    // When True, CrossSocket will gzip-compress responses whose Content-Type
    // is compressible and whose body exceeds MinCompressSize bytes.
    // Mapped directly to TCrossHttpServer.Compressible.
    // Default: False

    MinCompressSize: Int64;
    // Minimum response body size (bytes) below which compression is skipped.
    // Mapped directly to TCrossHttpServer.MinCompressSize.
    // Default: 512  (matches CrossSocket's internal MIN_COMPRESS_SIZE)

    // ── TLS / SSL ─────────────────────────────────────────────────────────

    // SSL / TLS [SEC-3]
    // SSL is enabled by passing SSLEnabled=True at construction.
    // Certificates are loaded via SetCertificateFile / SetPrivateKeyFile
    // on the TCrossSslSocketBase API — confirmed in Net.CrossSslSocket.Base.
    SSLEnabled: Boolean;
    // Set True to listen on HTTPS.  Requires SSLCertFile and SSLKeyFile.
    // Default: False

    SSLCertFile: string;
    // Absolute or relative path to the PEM certificate file.

    SSLKeyFile: string;
    // Absolute or relative path to the PEM private key file.

    SSLKeyPassword: string;
    // Passphrase for an encrypted PEM private key.  Leave empty if unencrypted.
    // Applied via FServer.SetPrivateKeyPassword before the key is loaded
    // (TLSOPT-1 — requires the Net.CrossSslSocket.* patches / fork release;
    // OpenSSL parses the key with a PEM password callback).

    SSLCACertFile: string;
    // Path to the CA certificate used to verify client certificates.
    // Required only for mutual TLS (mTLS).  Leave empty for server-only TLS.

    SSLVerifyPeer: Boolean;
    // When True, CrossSocket requires the client to present a certificate
    // signed by the CA in SSLCACertFile.  Only meaningful when SSLEnabled
    // and SSLCACertFile are both set.
    // Default: False

    SSLCipherList: string;
    // TLS 1.2 AND BELOW ONLY. OpenSSL cipher-list rule string (aliases, '!'
    // exclusions, @SECLEVEL), applied with SSL_CTX_set_cipher_list. Empty =
    // the provider's / library's default list. This field does NOT affect
    // TLS 1.3 - OpenSSL configures TLS 1.3 suites through a separate call, see
    // SSLCipherSuitesTLS13. An @SECLEVEL=n here does set the context-wide
    // security level, which TLS 1.3 handshakes obey too.
    // Override only when you have a specific compliance requirement.

    SSLCipherSuitesTLS13: string;
    // TLS 1.3 cipher suites: exact names, colon-separated, in priority order,
    // e.g. 'TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256'. Applied with
    // SSL_CTX_set_ciphersuites (OpenSSL 1.1.1+). Empty = library default.
    // Names are case-sensitive, and OpenSSL silently DROPS an unknown name
    // that sits next to a valid one, so a provider should read the effective
    // list back and refuse to start on a mismatch. A provider whose TLS
    // library cannot configure TLS 1.3 suites must refuse a non-empty value at
    // Listen rather than ignore it.
    // Default: ''

    SSLMinVersion: THorseTlsMinVersion;
    // Minimum TLS protocol version the server accepts; see THorseTlsMinVersion.
    // htvDefault makes no call, leaving the provider's / library's floor.
    // A provider that cannot enforce the requested minimum must refuse at
    // Listen rather than serve with a weaker one.
    // Default: htvDefault
    //
    // The "must refuse" rules above are the CONTRACT for a provider that
    // implements SSLCipherSuitesTLS13 / SSLMinVersion, not a guarantee from
    // this record: Horse's built-in providers read only the port in
    // ListenWithConfig, and providers released before these fields existed
    // compile against them and ignore them. doc/providers.md lists which
    // provider versions apply or refuse each value.

    // ── Server identity ───────────────────────────────────────────────────
    ServerBanner: string;
    // Value to emit in the HTTP Server: response header.
    // Empty string emits 'unknown' to prevent library/version fingerprinting.
    // Default: ''  (results in 'unknown')

    // ── Factory ───────────────────────────────────────────────────────────
    class function Default: THorseCrossSocketConfig; static;
  end;

implementation

class function THorseCrossSocketConfig.Default: THorseCrossSocketConfig;
begin
  Result.IoThreads         := DEFAULT_IO_THREADS;

  Result.KeepAliveTimeout  := 30;
  Result.ReadTimeout       := 20;
  Result.DrainTimeoutMs    := DEFAULT_DRAIN_TIMEOUT_MS;   // [SEC-6]
  Result.MaxHeaderSize     := DEFAULT_MAX_HEADER_SIZE;    // [SEC-1]
  Result.MaxBodySize       := DEFAULT_MAX_BODY_SIZE;      // [SEC-1]
  Result.MaxConnections    := 10000;

  Result.Compressible      := False;
  Result.MinCompressSize   := DEFAULT_MIN_COMPRESS_SIZE;

  Result.SSLEnabled        := False;
  Result.SSLCertFile       := '';
  Result.SSLKeyFile        := '';
  Result.SSLKeyPassword    := '';
  Result.SSLCACertFile     := '';
  Result.SSLVerifyPeer     := False;
  Result.SSLCipherList     := '';    // empty = provider / library default list (TLS <= 1.2)
  Result.SSLCipherSuitesTLS13 := '';  // empty = library default TLS 1.3 suites
  Result.SSLMinVersion     := htvDefault;
  Result.ServerBanner      := '';    // empty = emit 'unknown'
end;

end.
