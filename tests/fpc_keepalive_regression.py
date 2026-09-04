#!/usr/bin/env python3
"""FPC 3.3.1+ keep-alive latency regression test for the default provider."""

import socket
import statistics
import time

HOST = "127.0.0.1"
PORT = 9901
REQUEST_COUNT = 30
MAX_MEDIAN_MS = 25.0  # Half of FPC's 50 ms keep-alive idle polling interval.
REQUEST = (
    b"GET /ping HTTP/1.1\r\n"
    b"Host: localhost\r\n"
    b"Connection: keep-alive\r\n\r\n"
)


def read_response(sock: socket.socket, buffered: bytes) -> tuple[bytes, bytes]:
    while b"\r\n\r\n" not in buffered:
        chunk = sock.recv(65536)
        if not chunk:
            raise RuntimeError("peer closed before the response headers")
        buffered += chunk

    raw_headers, buffered = buffered.split(b"\r\n\r\n", 1)
    headers = {}
    for line in raw_headers.split(b"\r\n")[1:]:
        key, value = line.split(b":", 1)
        headers[key.lower()] = value.strip()

    content_length = int(headers.get(b"content-length", b"0"))
    while len(buffered) < content_length:
        chunk = sock.recv(65536)
        if not chunk:
            raise RuntimeError("peer closed before the response body")
        buffered += chunk

    body = buffered[:content_length]
    return body, buffered[content_length:]


def main() -> None:
    samples = []
    buffered = b""
    with socket.create_connection((HOST, PORT), timeout=5) as sock:
        sock.settimeout(5)
        connection = sock.getsockname()

        for _ in range(REQUEST_COUNT):
            started = time.perf_counter_ns()
            sock.sendall(REQUEST)
            body, buffered = read_response(sock, buffered)
            samples.append((time.perf_counter_ns() - started) / 1_000_000)
            if body != b"pong":
                raise RuntimeError(f"unexpected response body: {body!r}")

        if connection != sock.getsockname():
            raise RuntimeError("the client socket changed during the test")

    # Ignore connection establishment/first-request cost and use the median so
    # isolated scheduler jitter cannot fail the regression test.
    steady_state = samples[1:]
    median_ms = statistics.median(steady_state)
    p95_ms = sorted(steady_state)[int(len(steady_state) * 0.95) - 1]
    print(f"same socket: yes; median={median_ms:.3f} ms; p95={p95_ms:.3f} ms")
    if median_ms >= MAX_MEDIAN_MS:
        raise RuntimeError(
            f"keep-alive median {median_ms:.3f} ms is still near the FPC 50 ms tick"
        )


if __name__ == "__main__":
    main()
