# run_e2e_integration_tests.ps1
# Script para automação de testes de integração ponta a ponta (End-to-End) físicos no Delphi 13 Florence.

$ErrorActionPreference = "Stop"

$StudioPath = "C:\Program Files (x86)\Embarcadero\Studio\37.0"
$RsvarsPath = Join-Path $StudioPath "bin\rsvars.bat"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

# Função para parar processos em portas específicas
function Stop-ProcessesOnPorts {
    param([int[]]$Ports)
    foreach ($Port in $Ports) {
        $Connections = Get-NetTCPConnection -LocalPort $Port -ErrorAction SilentlyContinue
        if ($Connections) {
            Write-Host " -> Liberando porta $Port ocupada..." -ForegroundColor Yellow
            foreach ($Conn in $Connections) {
                if ($Conn.OwningProcess -gt 0) {
                    Stop-Process -Id $Conn.OwningProcess -Force -ErrorAction SilentlyContinue
                }
            }
            Sleep 1
        }
    }
}

Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host " FASE 1: TESTE DE INTEGRAÇÃO MULTI-INSTANCE (E2E)         " -ForegroundColor Cyan
Write-Host "==========================================================" -ForegroundColor Cyan

# Garante que as portas 9001 e 9002 estejam livres
Stop-ProcessesOnPorts -Ports @(9001, 9002)

$SampleSrcDir = Join-Path $ScriptDir "..\samples\delphi\console_multi_instance"
$SampleExe = Join-Path $SampleSrcDir "ConsoleMultiInstance.exe"

# 1. Limpeza
if (Test-Path $SampleExe) { Remove-Item -Path $SampleExe -Force }

# 2. Configurações de compilação do Sample
$SearchPath = '..\..\..\src'
$CfgPath = Join-Path $SampleSrcDir "ConsoleMultiInstance.cfg"
$CfgContent = @(
    "-B",
    "-Q",
    "-NS`"System;Xml;Data;Datasnap;Web;Soap;Winapi`"",
    "-I`"$SearchPath`"",
    "-U`"$SearchPath`""
)
Set-Content -Path $CfgPath -Value ($CfgContent -join "`r`n") -Force

# 3. Compilação do Sample Multi-Instance
Write-Host " -> Compilando Sample Multi-Instance..." -ForegroundColor Gray
$BuildCommand = "call `"{0}`" && cd /d `"{1}`" && dcc32.exe ConsoleMultiInstance.dpr" -f $RsvarsPath, $SampleSrcDir
$BuildOutput = cmd.exe /c $BuildCommand 2>&1
$BuildExitCode = $LASTEXITCODE

if (Test-Path $CfgPath) { Remove-Item -Path $CfgPath -Force }

if ($BuildExitCode -eq 0 -and (Test-Path $SampleExe)) {
    Write-Host " -> Compilação: SUCESSO" -ForegroundColor Green
} else {
    Write-Host " -> Compilação: ERRO" -ForegroundColor Red
    Write-Host $BuildOutput -ForegroundColor DarkRed
    exit 1
}

# 4. Executa o Sample em background
Write-Host " -> Executando servidor Multi-Instance em background..." -ForegroundColor Gray
$SampleProcess = Start-Process -FilePath $SampleExe -ArgumentList "--delay" -NoNewWindow -PassThru

# Aguarda inicialização física das portas
$TimeoutSeconds = 10
$Start = Get-Date
$Ready = $false
while (((Get-Date) - $Start).TotalSeconds -lt $TimeoutSeconds) {
    try {
        $Res1 = Invoke-RestMethod -Uri "http://127.0.0.1:9001/api/v1/ping" -Method Get
        $Res2 = Invoke-RestMethod -Uri "http://127.0.0.1:9002/admin/ping" -Method Get
        if (($Res1 -eq "Pong da API Publica (Instancia 1)") -and ($Res2 -eq "Pong da Area Admin (Instancia 2)")) {
            $Ready = $true
            Break
        }
    } catch {
        Sleep -Milliseconds 200
    }
}
if (-not $Ready) {
    throw "Servidores não inicializaram a tempo na porta 9001/9002"
}

try {
    # 5. Faz chamadas HTTP E2E
    Write-Host " -> Testando Requisições HTTP:" -ForegroundColor Gray

    # Teste 1.1: Porta 9001 (API Pública)
    $Res1 = Invoke-RestMethod -Uri "http://127.0.0.1:9001/api/v1/ping" -Method Get
    Write-Host "    [GET] http://127.0.0.1:9001/api/v1/ping -> Resposta: '$Res1'" -ForegroundColor Green
    if ($Res1 -ne "Pong da API Publica (Instancia 1)") { throw "Resposta incorreta na porta 9001" }

    # Teste 1.2: Porta 9002 (Admin/Metrics)
    $Res2 = Invoke-RestMethod -Uri "http://127.0.0.1:9002/admin/ping" -Method Get
    Write-Host "    [GET] http://127.0.0.1:9002/admin/ping  -> Resposta: '$Res2'" -ForegroundColor Green
    if ($Res2 -ne "Pong da Area Admin (Instancia 2)") { throw "Resposta incorreta na porta 9002" }

    # Teste 1.3: Isolamento (Chamar rota 9002 na porta 9001 - Esperado 404)
    try {
        Invoke-RestMethod -Uri "http://127.0.0.1:9001/admin/ping" -Method Get -ErrorAction Stop
        throw "Deveria ter retornado 404"
    } catch {
        if ($_.Exception.Response.StatusCode -eq 404) {
            Write-Host "    [GET] http://127.0.0.1:9001/admin/ping  -> Resposta: 404 Not Found (Isolamento OK)" -ForegroundColor Green
        } else {
            throw "Erro inesperado: $_"
        }
    }
} finally {
    # 6. Encerra o processo do servidor
    Write-Host " -> Encerrando servidor Multi-Instance..." -ForegroundColor Gray
    Stop-Process -Id $SampleProcess.Id -Force -ErrorAction SilentlyContinue
    Sleep 1
}


Write-Host ""
Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host " FASE 2: TESTE DE INTEGRAÇÃO NORMAL CLÁSSICO (E2E)         " -ForegroundColor Cyan
Write-Host "==========================================================" -ForegroundColor Cyan

# Garante que a porta 9999 esteja livre
Stop-ProcessesOnPorts -Ports @(9999)

$ServerSrcDir = Join-Path $ScriptDir "src"
$ServerExe = Join-Path $ScriptDir "IntegrationServer.exe"

# 1. Limpeza
if (Test-Path $ServerExe) { Remove-Item -Path $ServerExe -Force }

# 2. Configurações de compilação do IntegrationServer
$SearchPath = '..\..\src;modules;modules\jhonson\src;modules\restrequest4delphi\src;modules\restrequest4delphi\src\core;modules\restrequest4delphi\src\interfaces;modules\cors\src;modules\basic-auth\src'
$CfgPath = Join-Path $ServerSrcDir "IntegrationServer.cfg"
$CfgContent = @(
    "-B",
    "-Q",
    "-E`"..`"",
    "-NS`"System;Xml;Data;Datasnap;Web;Soap;Winapi`"",
    "-I`"$SearchPath`"",
    "-U`"$SearchPath`""
)
Set-Content -Path $CfgPath -Value ($CfgContent -join "`r`n") -Force

# 3. Compilação do IntegrationServer
Write-Host " -> Compilando Servidor de Integração Clássico..." -ForegroundColor Gray
$BuildCommand = "call `"{0}`" && cd /d `"{1}`" && dcc32.exe IntegrationServer.dpr" -f $RsvarsPath, $ServerSrcDir
$BuildOutput = cmd.exe /c $BuildCommand 2>&1
$BuildExitCode = $LASTEXITCODE

if (Test-Path $CfgPath) { Remove-Item -Path $CfgPath -Force }

if ($BuildExitCode -eq 0 -and (Test-Path $ServerExe)) {
    Write-Host " -> Compilação: SUCESSO" -ForegroundColor Green
} else {
    Write-Host " -> Compilação: ERRO" -ForegroundColor Red
    Write-Host $BuildOutput -ForegroundColor DarkRed
    exit 1
}

# 4. Executa o Servidor Clássico em background
Write-Host " -> Executando servidor clássico em background..." -ForegroundColor Gray
$ServerProcess = Start-Process -FilePath $ServerExe -NoNewWindow -PassThru

# Aguarda inicialização física da porta
$TimeoutSeconds = 10
$Start = Get-Date
$Ready = $false
# Aguarda inicialização física da porta
$TimeoutSeconds = 10
$Start = Get-Date
$Ready = $false
while (((Get-Date) - $Start).TotalSeconds -lt $TimeoutSeconds) {
    try {
        $ResPing = Invoke-RestMethod -Uri "http://127.0.0.1:9999/ping" -Method Get
        if ($ResPing.message -eq "pong") {
            $Ready = $true
            Break
        }
    } catch {
        Sleep -Milliseconds 200
    }
}
if (-not $Ready) {
    throw "Servidor clássico não inicializou a tempo na porta 9999"
}

try {
    # 5. Faz chamadas HTTP E2E
    Write-Host " -> Testando Requisições HTTP (Fachada Clássica):" -ForegroundColor Gray

    # Teste 2.1: Rota pública /ping (GET)
    $ResPing = Invoke-RestMethod -Uri "http://127.0.0.1:9999/ping" -Method Get
    Write-Host "    [GET] http://127.0.0.1:9999/ping           -> Resposta: '$($ResPing.message)'" -ForegroundColor Green
    if ($ResPing.message -ne "pong") { throw "Resposta do ping incorreta" }

    # Teste 2.2: Rota privada sem Auth (GET - Esperado 401)
    try {
        Invoke-RestMethod -Uri "http://127.0.0.1:9999/secure/private" -Method Get -ErrorAction Stop
        throw "Deveria ter retornado 401"
    } catch {
        if ($_.Exception.Response.StatusCode -eq 401) {
            Write-Host "    [GET] http://127.0.0.1:9999/secure/private -> Resposta: 401 Unauthorized (Auth OK)" -ForegroundColor Green
        } else {
            throw "Erro inesperado: $_"
        }
    }

    # Teste 2.3: Rota privada com Auth Correta (GET - admin:secret -> Authorization header)
    $Headers = @{ Authorization = "Basic YWRtaW46c2VjcmV0" }
    $ResPrivate = Invoke-RestMethod -Uri "http://127.0.0.1:9999/secure/private" -Method Get -Headers $Headers
    Write-Host "    [GET] http://127.0.0.1:9999/secure/private -> Resposta: '$ResPrivate'" -ForegroundColor Green
    if ($ResPrivate -ne "private-ok") { throw "Falha na rota segura com autenticação" }

} finally {
    # 6. Encerra o processo do servidor
    Write-Host " -> Encerrando servidor clássico..." -ForegroundColor Gray
    Stop-Process -Id $ServerProcess.Id -Force -ErrorAction SilentlyContinue
    Sleep 1
}

Write-Host "==========================================================" -ForegroundColor Green
Write-Host "  TESTES FINAIS DE INTEGRAÇÃO E2E CONCLUÍDOS COM SUCESSO! " -ForegroundColor Green
Write-Host "==========================================================" -ForegroundColor Green
exit 0
