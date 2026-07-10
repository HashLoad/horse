using Microsoft.AspNetCore.Builder;
using Microsoft.Extensions.Logging;

var builder = WebApplication.CreateBuilder(args);

// Remove os logs padrão do console para que o I/O de console não distorça as métricas de performance do processador de rede
builder.Logging.ClearProviders();

var app = builder.Build();

app.MapGet("/ping", () => "pong");

app.Run("http://0.0.0.0:9090");
