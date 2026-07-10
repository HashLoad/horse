FROM ubuntu:22.04

# Definir o diretório de trabalho no container
WORKDIR /app

# Copiar o binário ELF compilado nativamente para Linux
COPY benchmarks/suites/delphi-horse-default/HorseBench /app/

# Expor a porta que o Horse escuta
EXPOSE 9090

# Ponto de entrada para o executável Linux nativo
ENTRYPOINT ["/app/HorseBench"]
