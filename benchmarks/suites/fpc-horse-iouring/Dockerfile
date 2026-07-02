# Estágio de compilação (build)
FROM debian:bookworm-slim AS builder

RUN apt-get update && apt-get install -y \
    fpc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copiar o framework Horse e o código do benchmark
COPY src/ ./src
COPY benchmarks/suites/fpc-horse-iouring/HorseBench.pas ./

# Compilar o servidor com compatibilidade Delphi (-Sd), símbolos de depuração (-gl),
# ativando o provider IOURING (-dHORSE_PROVIDER_IOURING)
RUN fpc -Sd -gl -dHORSE_PROVIDER_IOURING -Fu./src/ HorseBench.pas

# Estágio final de execução
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /build/HorseBench .

EXPOSE 9090

ENTRYPOINT ["./HorseBench"]
