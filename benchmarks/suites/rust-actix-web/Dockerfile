FROM rust:alpine AS builder
RUN apk add --no-cache musl-dev
WORKDIR /usr/src/app

COPY benchmarks/suites/rust-actix-web/Cargo.toml ./
COPY benchmarks/suites/rust-actix-web/src ./src

RUN cargo build --release

FROM alpine:latest
WORKDIR /usr/local/bin
COPY --from=builder /usr/src/app/target/release/rust-actix-web .
EXPOSE 9090
CMD ["./rust-actix-web"]
