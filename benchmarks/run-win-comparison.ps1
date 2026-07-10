# run-win-comparison.ps1
# Script de Automação de Benchmark 100% Nativo no Windows (sem Docker)
# Executa todos os servidores no Windows Host e dispara carga com Bombardier.

$PSScriptRoot = Split-Path -Parent -Path $MyInvocation.MyCommand.Definition
Set-Location $PSScriptRoot

$toolsDir = Join-Path $PSScriptRoot "tools"
$resultsDir = Join-Path $PSScriptRoot "results"
if (-not (Test-Path $toolsDir)) { New-Item -ItemType Directory -Path $toolsDir | Out-Null }
if (-not (Test-Path $resultsDir)) { New-Item -ItemType Directory -Path $resultsDir | Out-Null }

$dotnetExe = "C:\Program Files\dotnet\dotnet.exe"
$cargoExe = "C:\Users\regys\.cargo\bin\cargo.exe"

# Lista de candidatos que compilaram com sucesso e serão executados
$candidatosHabilitados = @(
    "Delphi 13 (HTTP.sys)",
    "Delphi 13 (HTTP.sys + Radix)",
    "Delphi 13 (IOCP)",
    "Delphi 13 (IOCP + Radix)",
    "Node.js (Express)"
)

# 1. Configurar Bombardier
$bombardier = Join-Path $toolsDir "bombardier.exe"
if (-not (Test-Path $bombardier)) {
    Write-Host "==> Baixando o bombardier..." -ForegroundColor Yellow
    $url = "https://github.com/codesenberg/bombardier/releases/download/v1.2.6/bombardier-windows-amd64.exe"
    Invoke-WebRequest -Uri $url -OutFile $bombardier -UseBasicParsing
    Write-Host "✅ Bombardier pronto." -ForegroundColor Green
}

# 2. Configurar Go Portátil
$goDir = Join-Path $toolsDir "go"
$goExe = Join-Path $goDir "bin\go.exe"
if (-not (Test-Path $goExe)) {
    Write-Host "==> Baixando Go portátil (este download pode levar alguns instantes)..." -ForegroundColor Yellow
    $goZip = Join-Path $toolsDir "go.zip"
    $url = "https://go.dev/dl/go1.22.4.windows-amd64.zip"
    Invoke-WebRequest -Uri $url -OutFile $goZip -UseBasicParsing
    Write-Host " -> Extraindo Go..." -ForegroundColor Gray
    Expand-Archive -Path $goZip -DestinationPath $toolsDir -Force
    Remove-Item $goZip -Force
    Write-Host "✅ Go portátil configurado em $goExe." -ForegroundColor Green
}

# 3. Configurar Maven Portátil
$mavenDir = Join-Path $toolsDir "maven"
$mvnCmd = Join-Path $mavenDir "apache-maven-3.9.8\bin\mvn.cmd"
if (-not (Test-Path $mvnCmd)) {
    Write-Host "==> Baixando Maven portátil..." -ForegroundColor Yellow
    $mvnZip = Join-Path $toolsDir "maven.zip"
    $url = "https://archive.apache.org/dist/maven/maven-3/3.9.8/binaries/apache-maven-3.9.8-bin.zip"
    Invoke-WebRequest -Uri $url -OutFile $mvnZip -UseBasicParsing
    Write-Host " -> Extraindo Maven..." -ForegroundColor Gray
    Expand-Archive -Path $mvnZip -DestinationPath $mavenDir -Force
    Remove-Item $mvnZip -Force
    Write-Host "✅ Maven portátil configurado em $mvnCmd." -ForegroundColor Green
}

# 4. Compilar Candidatos no Windows

# A. Delphi HTTP.sys
try {
    Write-Host "`n==> Compilando Horse HTTP.sys..." -ForegroundColor Cyan
    if (Test-Path "win_comparison\HorseBenchHTTPSys.exe") { Remove-Item "win_comparison\HorseBenchHTTPSys.exe" -Force }
    Remove-Item "..\src\*.dcu" -Force -ErrorAction SilentlyContinue
    Remove-Item "win_comparison\*.dcu" -Force -ErrorAction SilentlyContinue

    & "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\dcc64.exe" -Q -W -H -GD -I"..\src" -U"..\src" "-NSSystem;System.Win;Winapi;Web;System.Hash" -dHORSE_PROVIDER_HTTPSYS win_comparison\HorseBench.dpr
    if (-not (Test-Path "win_comparison\HorseBench.exe")) { throw "Erro de build" }
    Move-Item "win_comparison\HorseBench.exe" "win_comparison\HorseBenchHTTPSys.exe" -Force
    Write-Host "✅ Horse HTTP.sys pronto." -ForegroundColor Green
} catch {
    Write-Warning "⚠️ Falha crítica ao compilar Horse HTTP.sys: $_"
}

# B. Delphi IOCP
try {
    Write-Host "`n==> Compilando Horse IOCP..." -ForegroundColor Cyan
    if (Test-Path "win_comparison\HorseBenchIOCP.exe") { Remove-Item "win_comparison\HorseBenchIOCP.exe" -Force }
    Remove-Item "..\src\*.dcu" -Force -ErrorAction SilentlyContinue
    Remove-Item "win_comparison\*.dcu" -Force -ErrorAction SilentlyContinue

    & "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\dcc64.exe" -Q -W -H -GD -I"..\src" -U"..\src" "-NSSystem;System.Win;Winapi;Web;System.Hash" -dHORSE_PROVIDER_IOCP win_comparison\HorseBench.dpr
    if (-not (Test-Path "win_comparison\HorseBench.exe")) { throw "Erro de build" }
    Move-Item "win_comparison\HorseBench.exe" "win_comparison\HorseBenchIOCP.exe" -Force
    Write-Host "✅ Horse IOCP pronto." -ForegroundColor Green
} catch {
    Write-Warning "⚠️ Falha crítica ao compilar Horse IOCP: $_"
}

# C. Go Fiber
try {
    Write-Host "`n==> Compilando Go Fiber..." -ForegroundColor Cyan
    Push-Location suites\go-fiber
    if (Test-Path "go-server.exe") { Remove-Item "go-server.exe" -Force }
    $env:GOROOT = $goDir
    & $goExe mod tidy
    & $goExe build -ldflags "-s -w" -o go-server.exe main.go
    if (-not (Test-Path "go-server.exe")) { throw "Erro de build" }
    Write-Host "✅ Go Fiber pronto." -ForegroundColor Green
    $candidatosHabilitados += "Go 1.22 (Fiber)"
} catch {
    Write-Warning "⚠️ Ignorando Go Fiber: $_"
} finally {
    Pop-Location
}

# D. Rust Actix-web
try {
    Write-Host "`n==> Compilando Rust Actix-web..." -ForegroundColor Cyan
    Push-Location suites\rust-actix-web
    if (Test-Path "target\release\rust-actix-web.exe") { Remove-Item "target\release\rust-actix-web.exe" -Force -ErrorAction SilentlyContinue }
    & $cargoExe build --release
    if (-not (Test-Path "target\release\rust-actix-web.exe")) { throw "Erro de build" }
    Write-Host "✅ Rust Actix-web pronto." -ForegroundColor Green
    $candidatosHabilitados += "Rust (Actix-web)"
} catch {
    Write-Warning "⚠️ Ignorando Rust Actix-web: $_"
} finally {
    Pop-Location
}

# E. .NET Minimal API
try {
    Write-Host "`n==> Compilando .NET Minimal API..." -ForegroundColor Cyan
    Push-Location suites\dotnet-minimal-api
    if (Test-Path "publish\dotnet-minimal-api.exe") { Remove-Item "publish\dotnet-minimal-api.exe" -Force -ErrorAction SilentlyContinue }
    & $dotnetExe publish -c Release -r win-x64 --self-contained true -o publish
    if (-not (Test-Path "publish\dotnet-minimal-api.exe")) { throw "Erro de build" }
    Write-Host "✅ .NET Minimal API pronto." -ForegroundColor Green
    $candidatosHabilitados += "C# (.NET 8)"
} catch {
    Write-Warning "⚠️ Ignorando .NET Minimal API: $_"
} finally {
    Pop-Location
}

# F. Java Spring Boot
try {
    Write-Host "`n==> Compilando Java Spring Boot..." -ForegroundColor Cyan
    Push-Location suites\java-springboot
    if (Test-Path "target\java-springboot-1.0.0.jar") { Remove-Item "target\java-springboot-1.0.0.jar" -Force -ErrorAction SilentlyContinue }
    $env:JAVA_HOME = "C:\Program Files\Java\jdk-25.0.1"
    & $mvnCmd clean package -DskipTests
    if (-not (Test-Path "target\java-springboot-1.0.0.jar")) { throw "Erro de build" }
    Write-Host "✅ Java Spring Boot pronto." -ForegroundColor Green
    $candidatosHabilitados += "Java 25 (Spring Boot)"
} catch {
    Write-Warning "⚠️ Ignorando Java Spring Boot: $_"
} finally {
    Pop-Location
}

# G. Node.js Express
try {
    Write-Host "`n==> Instalando dependências do Node.js..." -ForegroundColor Cyan
    Push-Location suites\nodejs-express
    & npm install
    Write-Host "✅ Node.js pronto." -ForegroundColor Green
} catch {
    Write-Warning "⚠️ Falha nas dependências do Node.js: $_"
} finally {
    Pop-Location
}


# 5. Definição dos Candidatos para Execução com Caminhos Absolutos
$candidatos = @(
    @{
        Nome = "Delphi 13 (HTTP.sys)"
        Caminho = Join-Path $PSScriptRoot "win_comparison\HorseBenchHTTPSys.exe"
        WorkingDirectory = Join-Path $PSScriptRoot "win_comparison"
        Args = ""
        Porta = 9090
    },
    @{
        Nome = "Delphi 13 (HTTP.sys + Radix)"
        Caminho = Join-Path $PSScriptRoot "win_comparison\HorseBenchHTTPSys.exe"
        WorkingDirectory = Join-Path $PSScriptRoot "win_comparison"
        Args = "--radix"
        Porta = 9090
    },
    @{
        Nome = "Delphi 13 (IOCP)"
        Caminho = Join-Path $PSScriptRoot "win_comparison\HorseBenchIOCP.exe"
        WorkingDirectory = Join-Path $PSScriptRoot "win_comparison"
        Args = ""
        Porta = 9090
    },
    @{
        Nome = "Delphi 13 (IOCP + Radix)"
        Caminho = Join-Path $PSScriptRoot "win_comparison\HorseBenchIOCP.exe"
        WorkingDirectory = Join-Path $PSScriptRoot "win_comparison"
        Args = "--radix"
        Porta = 9090
    },
    @{
        Nome = "Go 1.22 (Fiber)"
        Caminho = Join-Path $PSScriptRoot "suites\go-fiber\go-server.exe"
        WorkingDirectory = Join-Path $PSScriptRoot "suites\go-fiber"
        Args = ""
        Porta = 9090
    },
    @{
        Nome = "Rust (Actix-web)"
        Caminho = Join-Path $PSScriptRoot "suites\rust-actix-web\target\release\rust-actix-web.exe"
        WorkingDirectory = Join-Path $PSScriptRoot "suites\rust-actix-web"
        Args = ""
        Porta = 9090
    },
    @{
        Nome = "C# (.NET 8)"
        Caminho = Join-Path $PSScriptRoot "suites\dotnet-minimal-api\publish\dotnet-minimal-api.exe"
        WorkingDirectory = Join-Path $PSScriptRoot "suites\dotnet-minimal-api"
        Args = ""
        Porta = 9090
    },
    @{
        Nome = "Java 25 (Spring Boot)"
        Caminho = "java.exe"
        WorkingDirectory = Join-Path $PSScriptRoot "suites\java-springboot"
        Args = "-jar target\java-springboot-1.0.0.jar"
        Porta = 9090
    },
    @{
        Nome = "Node.js (Express)"
        Caminho = "node.exe"
        WorkingDirectory = Join-Path $PSScriptRoot "suites\nodejs-express"
        Args = "server.js"
        Porta = 9090
    }
)

$resultsSummary = @()

# 6. Rodar os Benchmarks
Write-Host "`n==> Iniciando execução dos benchmarks..." -ForegroundColor Cyan
foreach ($cand in $candidatos) {
    if ($candidatosHabilitados -notcontains $cand.Nome) {
        Write-Host "`n -> Candidato $($cand.Nome) ignorado devido a falhas na compilação do host." -ForegroundColor Gray
        continue
    }

    Write-Host "`n----------------------------------------------------------" -ForegroundColor Yellow
    Write-Host " Candidato: $($cand.Nome)" -ForegroundColor Yellow
    Write-Host "----------------------------------------------------------" -ForegroundColor Yellow

    # Iniciar processo
    $startInfo = New-Object System.Diagnostics.ProcessStartInfo
    $startInfo.FileName = $cand.Caminho
    $startInfo.Arguments = $cand.Args
    $startInfo.WorkingDirectory = $cand.WorkingDirectory
    $startInfo.UseShellExecute = $false
    $startInfo.CreateNoWindow = $true
    
    $proc = $null
    try {
        $proc = [System.Diagnostics.Process]::Start($startInfo)
        Write-Host " -> Iniciado processo com ID: $($proc.Id)" -ForegroundColor DarkGray
    } catch {
        Write-Error "❌ Falha ao iniciar processo: $_"
        continue
    }
    
    Start-Sleep -Seconds 4

    # Verificar se está respondendo
    $success = $false
    try {
        $testReq = Invoke-WebRequest -Uri "http://127.0.0.1:$($cand.Porta)/ping" -UseBasicParsing -TimeoutSec 5
        if ($testReq.Content -match "pong") {
            $success = $true
        }
    } catch {}

    if (-not $success) {
        Write-Error "❌ Servidor não está respondendo em http://127.0.0.1:$($cand.Porta)/ping"
        if ($proc -and -not $proc.HasExited) {
            $proc.Kill()
        }
        Start-Sleep -Seconds 2
        continue
    }
    Write-Host "✅ Servidor respondendo OK." -ForegroundColor Green

    # Aquecimento (Warm-up)
    Write-Host " -> Executando Warm-up (5s)..." -ForegroundColor DarkGray
    & $bombardier -c 125 -d 5s -l "http://127.0.0.1:$($cand.Porta)/ping" | Out-Null
    Start-Sleep -Seconds 2

    # Medição real
    Write-Host " -> Executando teste real (125 conexões, 15s)..." -ForegroundColor Cyan
    $bombardierOutput = & $bombardier -c 125 -d 15s "http://127.0.0.1:$($cand.Porta)/ping"

    # Matar processo imediatamente
    Write-Host " -> Finalizando servidor..." -ForegroundColor DarkGray
    if ($proc -and -not $proc.HasExited) {
        try {
            $proc.Kill()
        } catch {}
    }
    Start-Sleep -Seconds 4

    # Parsing dos Resultados
    $rps = 0
    $avgLatency = "0ms"
    $p99Latency = "0ms"

    foreach ($line in $bombardierOutput) {
        # Extrai RPS
        if ($line -match "Reqs/sec\s+(\d+(\.\d+)?)") {
            $rps = $Matches[1]
        }
        # Extrai Latência Média
        if ($line -match "Latency\s+(\d+(\.\d+)?\w+)\s+(\d+(\.\d+)?\w+)\s+(\d+(\.\d+)?\w+)") {
            $avgLatency = $Matches[1]
        }
        # Extrai p99
        if ($line -match "99%\s+(\d+(\.\d+)?\w+)") {
            $p99Latency = $Matches[1]
        }
    }

    Write-Host "   -> RPS: $rps | Latência Média: $avgLatency | p99: $p99Latency" -ForegroundColor Green

    $resultsSummary += [PSCustomObject]@{
        Nome = $cand.Nome
        RPS = [double]$rps
        LatenciaMedia = $avgLatency
        Latenciap99 = $p99Latency
    }
}

# 7. Gerar Relatório Consolidado
Write-Host "`n==> Gerando relatório de benchmark..." -ForegroundColor Cyan

$mdReport = "# Resultados do Benchmark 100% Nativo no Windows`n`n"
$mdReport += "Teste executado em: $(Get-Date -Format 'dd/MM/yyyy HH:mm:ss')`n"
$mdReport += "Parâmetros: 125 Conexões Simultâneas, 15 segundos de carga`n"
$mdReport += "Ambiente: Windows Host Nativo (sem Docker/Containers)`n`n"
$mdReport += "| Candidato / Tecnologia | RPS (Throughput) | Latência Média | Latência p99 |`n"
$mdReport += "| :--- | :--- | :--- | :--- |`n"

$sortedResults = $resultsSummary | Sort-Object -Property RPS -Descending
foreach ($r in $sortedResults) {
    $mdReport += "| $($r.Nome) | $($r.RPS) | $($r.LatenciaMedia) | $($r.Latenciap99) |`n"
}

$reportPath = Join-Path $resultsDir "win_comparison_consolidado.md"
$mdReport | Out-File $reportPath -Encoding UTF8
Write-Host "✅ Relatório gerado em: $reportPath" -ForegroundColor Green
