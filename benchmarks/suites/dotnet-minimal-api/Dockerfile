FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /src

# Copiar os fontes e restaurar dependências
COPY benchmarks/suites/dotnet-minimal-api/ ./
RUN dotnet restore

# Compilar e publicar a versão de produção
RUN dotnet publish -c Release -o /app/publish /p:UseAppHost=false

FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS runtime
WORKDIR /app
COPY --from=build /app/publish .

EXPOSE 9090

ENTRYPOINT ["dotnet", "dotnet-minimal-api.dll"]
