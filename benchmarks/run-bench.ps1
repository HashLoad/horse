# Script de Automação de Benchmark - Horse vs Outras Tecnologias
# Executa os testes de carga, coleta CPU/RAM e gera os logs de latência/throughput.

$PSScriptRoot = Split-Path -Parent -Path $MyInvocation.MyCommand.Definition
Set-Location $PSScriptRoot

# 1. Configurar Diretórios e Ferramenta de Carga
$toolsDir = Join-Path $PSScriptRoot "tools"
$resultsDir = Join-Path $PSScriptRoot "results"
if (-not (Test-Path $toolsDir)) { New-Item -ItemType Directory -Path $toolsDir | Out-Null }
if (-not (Test-Path $resultsDir)) { New-Item -ItemType Directory -Path $resultsDir | Out-Null }

$bombardier = Join-Path $toolsDir "bombardier.exe"
if (-not (Test-Path $bombardier)) {
    Write-Host "==> Baixando o bombardier..." -ForegroundColor Yellow
    $url = "https://github.com/codesenberg/bombardier/releases/download/v1.2.6/bombardier-windows-amd64.exe"
    try {
        Invoke-WebRequest -Uri $url -OutFile $bombardier -UseBasicParsing
        Write-Host "✅ Bombardier baixado com sucesso!" -ForegroundColor Green
    } catch {
        Write-Host "⚠️ Falha ao baixar v1.2.6, tentando v1.2.5..." -ForegroundColor Yellow
        $urlFallback = "https://github.com/codesenberg/bombardier/releases/download/v1.2.5/bombardier-windows-amd64.exe"
        try {
            Invoke-WebRequest -Uri $urlFallback -OutFile $bombardier -UseBasicParsing
            Write-Host "✅ Bombardier (v1.2.5) baixado com sucesso!" -ForegroundColor Green
        } catch {
            Write-Error "❌ Falha crítica ao baixar o bombardier. Instale-o manualmente na pasta 'benchmarks/tools/' como 'bombardier.exe'"
            exit 1
        }
    }
}

# 2. Compilar Servidor Delphi (HTTP.sys Windows)
Write-Host "`n==> Compilando o servidor Delphi (HTTP.sys)..." -ForegroundColor Cyan
Push-Location suites\delphi-horse-httpsys
if (Test-Path "HorseBench.exe") { Remove-Item "HorseBench.exe" -Force }
# Compila utilizando dcc64 (PATH)
& dcc64 -Q -W -H -GD -$D- -I"..\..\..\src" -U"..\..\..\src" "-NSSystem;System.Win;Winapi;Web;System.Hash" HorseBench.dpr
if (-not (Test-Path "HorseBench.exe")) {
    Write-Error "❌ Falha ao compilar o HorseBench.exe (Delphi)"
    Pop-Location
    exit 1
}
Write-Host "✅ HorseBench.exe compilado com sucesso!" -ForegroundColor Green
Pop-Location

# 3. Compilar Imagens Docker (Linux)
Write-Host "`n==> Compilando imagens no Docker (FPC, .NET, Node, Java, Go)..." -ForegroundColor Cyan
& docker compose build
if ($LASTEXITCODE -ne 0) {
    Write-Error "❌ Falha ao construir os containers Docker!"
    exit 1
}
Write-Host "✅ Imagens Docker prontas!" -ForegroundColor Green

# 4. Configuração dos Candidatos de Benchmark
$candidatos = @(
    @{
        Nome = "Delphi (HTTP.sys) - Windows Host"
        Tipo = "host"
        Executavel = "suites\delphi-horse-httpsys\HorseBench.exe"
        Porta = 9090
    },
    @{
        Nome = "FPC/Lazarus (Default) - Linux Docker"
        Tipo = "docker"
        Service = "fpc-horse-default"
        Container = "fpc-horse-default"
        Porta = 9090
    },
    @{
        Nome = "FPC/Lazarus (epoll) - Linux Docker"
        Tipo = "docker"
        Service = "fpc-horse-epoll"
        Container = "fpc-horse-epoll"
        Porta = 9090
    },
    @{
        Nome = ".NET (Minimal API) - Linux Docker"
        Tipo = "docker"
        Service = "dotnet-minimal-api"
        Container = "dotnet-minimal-api"
        Porta = 9090
    },
    @{
        Nome = "Node.js (Express) - Linux Docker"
        Tipo = "docker"
        Service = "nodejs-express"
        Container = "nodejs-express"
        Porta = 9090
    },
    @{
        Nome = "Java (Spring Boot) - Linux Docker"
        Tipo = "docker"
        Service = "java-springboot"
        Container = "java-springboot"
        Porta = 9090
    },
    @{
        Nome = "Go (Fiber) - Linux Docker"
        Tipo = "docker"
        Service = "go-fiber"
        Container = "go-fiber"
        Porta = 9090
    }
)

$concurrencias = @(128, 512, 1024)
$testDuration = "30s"
$warmupDuration = "10s"

# 5. Execução do Loop de Testes
Write-Host "`n===============================================" -ForegroundColor Magenta
Write-Host "   INICIANDO TESTES OFICIAIS DE BENCHMARK" -ForegroundColor Magenta
Write-Host "===============================================" -ForegroundColor Magenta

$resultsSummary = @()

foreach ($cand in $candidatos) {
    Write-Host "`n>>> Testando: $($cand.Nome) <<<" -ForegroundColor Yellow

    # Inicializar Servidor
    $processJob = $null
    if ($cand.Tipo -eq "host") {
        Write-Host "Iniciando processo local: $($cand.Executavel)" -ForegroundColor DarkGray
        $processJob = Start-Process -FilePath $cand.Executavel -PassThru -NoNewWindow
        Start-Sleep -Seconds 3
    } else {
        Write-Host "Iniciando container Docker: $($cand.Service)" -ForegroundColor DarkGray
        & docker compose up -d $($cand.Service)
        Start-Sleep -Seconds 5 # Tempo maior de boot para Java/Spring
    }

    # Verificar se servidor está respondendo
    try {
        $testReq = Invoke-WebRequest -Uri "http://localhost:$($cand.Porta)/ping" -UseBasicParsing -TimeoutSec 5
        if ($testReq.Content -notlike "*pong*") {
            throw "Resposta inesperada: $($testReq.Content)"
        }
        Write-Host "✅ Servidor respondendo OK." -ForegroundColor Green
    } catch {
        Write-Error "❌ Servidor não está respondendo em http://localhost:$($cand.Porta)/ping: $_"
        if ($cand.Tipo -eq "host") {
            Stop-Process -Id $processJob.Id -Force -ErrorAction SilentlyContinue
        } else {
            & docker compose down
        }
        continue
    }

    # Fase de Aquecimento (Warm-up)
    Write-Host "Executando Aquecimento ($warmupDuration)..." -ForegroundColor DarkGray
    & $bombardier -c 128 -d $warmupDuration -l "http://localhost:$($cand.Porta)/ping" | Out-Null
    Start-Sleep -Seconds 2

    # Loop de Concorrência
    foreach ($connections in $concurrencias) {
        Write-Host "Executando Teste: Concorrência=$connections, Tempo=$testDuration" -ForegroundColor Cyan
        
        # Nome do arquivo de log
        $safeName = $cand.Nome -replace "[ /()\-.]", "_"
        $logFile = Join-Path $resultsDir "$($safeName)_c${connections}.log"

        # Coleta de recursos em paralelo
        $cpuSamples = @()
        $memSamples = @()
        $collecting = $true
        
        $statBlock = {
            param($candType, $containerName, $processId)
            if ($candType -eq "docker") {
                $stats = docker stats $containerName --no-stream --format "{{.CPUPerc}} | {{.MemUsage}}" 2>$null
                if ($stats) {
                    $parts = $stats -split " \| "
                    $cpu = $parts[0].Replace("%", "").Trim()
                    $mem = $parts[1] -split " / " | Select-Object -First 1
                    return @($cpu, $mem)
                }
            } else {
                # Windows Process
                $proc = Get-Process -Id $processId -ErrorAction SilentlyContinue
                if ($proc) {
                    # CPU no Windows via Process pode ser imprecisa em amostragem rápida, mas dá um norte
                    $cpu = (Get-Counter '\Process(HorseBench)\% Processor Time' -ErrorAction SilentlyContinue).CounterSamples[0].CookedValue
                    # Divide pelo número de núcleos de CPU lógicos para ter uma métrica comparável a docker stats (que escala até 200% se usar 2 cores)
                    $cores = (Get-CimInstance Win32_ComputerSystem).NumberOfLogicalProcessors
                    $cpuPerc = [Math]::Round(($cpu), 2)
                    $memMb = [Math]::Round(($proc.WorkingSet64 / 1MB), 2)
                    return @($cpuPerc, "$($memMb)MiB")
                }
            }
            return @(0, "0MiB")
        }

        # Disparar bombardier em background
        $bombardierJob = Start-Job -ScriptBlock {
            param($exe, $conn, $dur, $url)
            & $exe -c $conn -d $dur -l $url
        } -ArgumentList $bombardier, $connections, $testDuration, "http://localhost:$($cand.Porta)/ping"

        # Capturar métricas durante o teste (1 amostra a cada 2 segundos)
        $elapsed = 0
        while ($elapsed -lt 30) {
            Start-Sleep -Seconds 2
            $elapsed += 2
            $procId = if ($processJob) { $processJob.Id } else { $null }
            $stats = &$statBlock -candType $cand.Tipo -containerName $cand.Container -processId $procId
            if ($stats) {
                $cpuSamples += [double]$stats[0]
                $memSamples += $stats[1]
            }
        }

        # Aguardar fim do bombardier
        $bombardierOutput = Wait-Job $bombardierJob | Receive-Job
        Remove-Job $bombardierJob
        $bombardierOutput | Out-File $logFile

        # Calcular Médias das Métricas
        $avgCpu = 0
        if ($cpuSamples.Count -gt 0) {
            $sumCpu = 0
            foreach ($c in $cpuSamples) { $sumCpu += $c }
            $avgCpu = [Math]::Round(($sumCpu / $cpuSamples.Count), 2)
        }
        
        $lastMem = "N/A"
        if ($memSamples.Count -gt 0) {
            $lastMem = $memSamples[-1] # Pega o último consumo de memória sob carga estável
        }

        # Extrair RPS e Latência Média do Log do Bombardier
        $rps = 0
        $avgLatency = "0ms"
        $p99Latency = "0ms"
        
        $rpsLine = $bombardierOutput | Where-Object { $_ -like "*Reqs/sec*" }
        if ($rpsLine) {
            if ($rpsLine -match "(\d+(\.\d+)?)\s*Reqs/sec") {
                $rps = $Matches[1]
            }
        } else {
            # Tenta encontrar a linha "Requests      [      Total,    Rate ]"
            $rateLine = $bombardierOutput | Where-Object { $_ -like "*Rate*" -and $_ -like "*Total*" }
            # O bombardier as vezes printa as estatísticas consolidadas no final
            # Varremos todo o log buscando a taxa
            foreach ($line in $bombardierOutput) {
                if ($line -match "Rate:\s+(\d+(\.\d+)?)/s") {
                    $rps = $Matches[1]
                }
            }
        }

        # Buscar estatísticas de latência no log formatado do bombardier
        # Procura por "Latency" na tabela
        $latencySection = $false
        foreach ($line in $bombardierOutput) {
            if ($line -match "Latency\s+(\d+(\.\d+)?\w+)\s+(\d+(\.\d+)?\w+)\s+(\d+(\.\d+)?\w+)") {
                # Formato: Latency      1.20ms      5.40ms     12.00ms
                # Pegamos a média (primeira coluna depois da label)
                $avgLatency = $Matches[1]
            }
            # Percentis
            if ($line -match "99%\s+(\d+(\.\d+)?\w+)") {
                $p99Latency = $Matches[1]
            }
        }

        Write-Host "   -> RPS obtido: $rps" -ForegroundColor Green
        Write-Host "   -> Latência Média: $avgLatency | p99: $p99Latency" -ForegroundColor Green
        Write-Host "   -> CPU Média: $avgCpu% | Memória: $lastMem" -ForegroundColor Green

        # Salvar resultados consolidados
        $resultsSummary += [PSCustomObject]@{
            Tecnologia = $cand.Nome
            Concorrencia = $connections
            RPS = $rps
            LatenciaMedia = $avgLatency
            Latenciap99 = $p99Latency
            CPUMedia = "$avgCpu%"
            Memoria = $lastMem
        }
    }

    # Desligar Servidor
    if ($cand.Tipo -eq "host") {
        Write-Host "Parando processo local..." -ForegroundColor DarkGray
        Stop-Process -Id $processJob.Id -Force -ErrorAction SilentlyContinue
    } else {
        Write-Host "Parando container Docker..." -ForegroundColor DarkGray
        & docker compose down $($cand.Service)
    }
    Start-Sleep -Seconds 2
}

# 6. Gerar Tabela Resumo em Markdown
Write-Host "`n==> Gerando tabela consolidada de resultados..." -ForegroundColor Cyan

$mdReport = "# Resultados do Benchmark (RPS e Latência)`n`n"
$mdReport += "Teste executado em: $(Get-Date -Format 'dd/MM/yyyy HH:mm:ss')`n"
$mdReport += "Duração dos Testes: $testDuration por cenário`n"
$mdReport += "Limites Docker: 2 CPUs, 512MB RAM`n`n"

foreach ($conn in $concurrencias) {
    $mdReport += "## ⚡ Concorrência: $conn Conexões Simultâneas`n`n"
    $mdReport += "| Tecnologia | RPS (Throughput) | Latência Média | Latência p99 | CPU Média | Memória Final |`n"
    $mdReport += "| :--- | :--- | :--- | :--- | :--- | :--- |`n"
    
    $filtered = $resultsSummary | Where-Object { $_.Concorrencia -eq $conn } | Sort-Object -Property {[double]$_.RPS} -Descending
    foreach ($r in $filtered) {
        $mdReport += "| $($r.Tecnologia) | $($r.RPS) | $($r.LatenciaMedia) | $($r.Latenciap99) | $($r.CPUMedia) | $($r.Memoria) |`n"
    }
    $mdReport += "`n"
}

$reportPath = Join-Path $resultsDir "consolidado.md"
$mdReport | Out-File $reportPath
Write-Host "✅ Benchmark concluído! Relatório gerado em: $reportPath" -ForegroundColor Green
