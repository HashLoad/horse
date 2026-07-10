FROM golang:1.21-alpine AS build
WORKDIR /app

# Copiar os códigos fontes do módulo
COPY benchmarks/suites/go-fiber/go.mod ./
COPY benchmarks/suites/go-fiber/main.go ./

# Resolver dependências e compilar de forma estática
RUN go mod tidy
RUN CGO_ENABLED=0 GOOS=linux go build -ldflags="-s -w" -o server .

# Imagem final ultra enxuta
FROM alpine:latest
WORKDIR /app

COPY --from=build /app/server .

EXPOSE 9090

ENTRYPOINT ["./server"]
