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


type
  THorseCrossSocketConfig = record

    // IO model
    IoThreads:       Integer;  // [SEC-2] 0 = library default (recommended)

    // ── Timeouts ─────────────────────────────────────────────────────────
    KeepAliveTimeout: Integer;
    // seconds; 0 = disable keep-alive entirely.
    // Default: 30

    ReadTimeout: Integer;
    // seconds; enforced by CrossSocket at the socket layer.
    // Mitigates slow-HTTP (Slowloris) attacks.
    // Default: 20  — never leave at 0 (would be unlimited).

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
    // Passphrase for an encrypted private key.  Leave empty if unencrypted.

    SSLCACertFile: string;
    // Path to the CA certificate used to verify client certificates.
    // Required only for mutual TLS (mTLS).  Leave empty for server-only TLS.

    SSLVerifyPeer: Boolean;
    // When True, CrossSocket requires the client to present a certificate
    // signed by the CA in SSLCACertFile.  Only meaningful when SSLEnabled
    // and SSLCACertFile are both set.
    // Default: False

    SSLCipherList: string;
    // OpenSSL cipher-list string.  Empty = use the built-in AEAD-only list
    // defined in Horse.Provider.CrossSocket.Server (TLS 1.2 + TLS 1.3,
    // forward secrecy, no RC4/3DES/export).
    // Override only when you have a specific compliance requirement.

    // ── Server identity ───────────────────────────────────────────────────
    ServerBanner: string;
    // Value to emit in the HTTP Server: response header.
    // Empty string suppresses the header (replaced with 'unknown') to
    // prevent library/version fingerprinting.
    // Default: ''  (suppressed)

    // ── Factory ───────────────────────────────────────────────────────────
    class function Default: THorseCrossSocketConfig; static;
  end;

implementation

class function THorseCrossSocketConfig.Default: THorseCrossSocketConfig;
begin
  Result.IoThreads      := DEFAULT_IO_THREADS;

  Result.KeepAliveTimeout := 30;
  Result.ReadTimeout      := 20;
  Result.DrainTimeoutMs := DEFAULT_DRAIN_TIMEOUT_MS;     // [SEC-6]
  Result.MaxHeaderSize    := DEFAULT_MAX_HEADER_SIZE;    // [SEC-1]
  Result.MaxBodySize      := DEFAULT_MAX_BODY_SIZE;      // [SEC-1]
  Result.MaxConnections   := 10000;
  Result.SSLEnabled       := False;
  Result.SSLCertFile      := '';
  Result.SSLKeyFile       := '';
  Result.SSLKeyPassword   := '';
  Result.SSLCACertFile    := '';
  Result.SSLVerifyPeer    := False;
  Result.SSLCipherList    := '';    // empty = use SECURE_CIPHER_LIST constant
  Result.ServerBanner     := '';    // empty = suppress Server: header
end;

end.
