# epoll Provider for Horse

The `epoll` provider is an asynchronous, non-blocking transport layer native to Linux, leveraging the kernel's `epoll` API (via `epoll_wait`). Like HTTP.sys, it is built directly into the Horse framework core — no external packages are required. After the Phase 2 optimizations, it features a highly scalable Thread-Local Buffer Pool, zero-allocation header parsing, vectorised system writes (`writev`), and zero-copy file streaming (`sendfile`).

In your project's Conditional Defines: `HORSE_PROVIDER_EPOLL`.

---

## ⚙️ Properties & Architecture

- **Linux-only:** Native event loop for Linux (perfect for Ubuntu, Debian, CentOS, Alpine, and Docker containers).
- **Asynchronous Event Loop:** Multiplexes thousands of active TCP sockets using a fixed IO/Worker thread pool, completely bypassing the thread-per-connection scaling wall.
- **Thread-Local Buffer Pool (`threadvar`):** Worker threads retrieve buffers from local pools, eliminating global lock contention on memory allocations.
- **Zero-Allocation Parser:** HTTP headers are parsed and mapped directly via byte-offsets on the socket buffer without allocating temporary strings on the heap.
- **Vetorised Writes (`writev`):** Groups header and payload buffers to reduce system call context switches.
- **Zero-Copy File Transfer (`sendfile`):** Transfers streams (`TFileStream`) directly from the OS page cache to the network socket, avoiding user-space copies.
- **Active Keep-Alive:** Manages inactive connections efficiently, releasing idle descriptors via a 5-second automatic timeout.
- **Slowloris Protection:** Automatically monitors and drops slow, incomplete request streams.
- **File Descriptor Auto-Elevation:** Automatically raises `RLIMIT_NOFILE` limits to 65535 upon startup to handle large volumes of simultaneous clients.
- **Performance:** Processed up to **29,209 req/s** with zero errors under a 1,000 concurrent Keep-Alive connection load in stress benchmarks.

---

## 🐧 Linux Kernel & System Limits

To support thousands of concurrent connections under Linux, make sure your operating system limits are configured correctly.

The provider automatically attempts to elevate user limits on startup. However, you can verify and set system descriptor limits manually if needed:

```bash
# Check current shell file limits
ulimit -n

# Temporarily set file descriptor limit to 65535
ulimit -n 65535
```

## 🚀 Production Tuning for Linux

For extreme loads and high-performance setups, tune the Linux kernel parameters in `/etc/sysctl.conf`:

```ini
# Increase max queued connections backlog
net.core.somaxconn = 65535
net.ipv4.tcp_max_syn_backlog = 65535

# Enable fast reuse of TIME_WAIT sockets
net.ipv4.tcp_tw_reuse = 1

# Increase ephemeral outbound port range
net.ipv4.ip_local_port_range = 1024 65535

# Increase max open file descriptors limit globally
fs.file-max = 2097152
```

Run `sysctl -p` to apply kernel configurations immediately.

---

## ⚡ Quick Start

1. Add `HORSE_PROVIDER_EPOLL` to your project's Conditional Defines (**Project Options → Delphi Compiler → Conditional defines**).
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

  // Starts the epoll reactor on port 9095
  THorse.Listen(9095);
end.
```

The framework will resolve the `THorse.Listen` call to the native epoll reactor when running under Linux.
