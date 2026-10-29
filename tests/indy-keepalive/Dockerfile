FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends \
    binutils libc6-dev libgcc-s1 zlib1g \
    && rm -rf /var/lib/apt/lists/*
