# Script de Teste de Integridade: Roteamento Avançado (Regex e Opcionais)
$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$ProjectDir = (Get-Item $ScriptDir).Parent.Parent.Parent.FullName
$DprPath = Join-Path $ScriptDir "ConsoleAdvancedRouting.dpr"
$ExePath = Join-Path $ScriptDir "ConsoleAdvancedRouting.exe"

# 1. Localiza rsvars.bat do Delphi
$StudioPath = "C:\Program Files (x86)\Embarcadero\Studio"
$RsvarsPath = $null

if (Test-Path $StudioPath) {
    $Dirs = Get-ChildItem $StudioPath | Where-Object { $_.Attributes -match "Directory" } | Sort-Object Name -Descending
    foreach ($Dir in $Dirs) {
        $Bat = Join-Path $Dir.FullName "bin\rsvars.bat"
        if (Test-Path $Bat) {
            $RsvarsPath = $Bat
            break
        }
    }
}

if (-not $RsvarsPath) {
    Write-Error "Não foi possível localizar o rsvars.bat do Delphi para compilação."
}

Write-Host "=== Iniciando Teste de Integridade: Roteamento Avançado ===" -ForegroundColor Cyan
Write-Host " -> Usando compilador em: $RsvarsPath" -ForegroundColor Gray

# 2. Compilação do Sample
Write-Host " -> Compilando ConsoleAdvancedRouting.dpr..." -ForegroundColor Gray
if (Test-Path $ExePath) { Remove-Item $ExePath -Force }

$BuildCommand = "call `"{0}`" && cd /d `"{1}`" && dcc32.exe -I`"{2}\src`" -U`"{2}\src`" -NS`"System;System.Win;Winapi;Data;Web`" ConsoleAdvancedRouting.dpr" -f $RsvarsPath, $ScriptDir, $ProjectDir
$BuildOutput = cmd.exe /c $BuildCommand 2>&1
$BuildExitCode = $LASTEXITCODE

if ($BuildExitCode -ne 0) {
    Write-Host $BuildOutput -ForegroundColor Red
    Write-Error "Falha na compilação do ConsoleAdvancedRouting!"
}
Write-Host " -> Compilação concluída com sucesso." -ForegroundColor Green

# 3. Função de Execução de Cenário de Teste
function Run-TestScenario($UseRadix) {
    $RouterName = if ($UseRadix) { "Radix Router" } else { "Tree Router (Padrão)" }
    Write-Host "----------------------------------------------------" -ForegroundColor Gray
    Write-Host " Testando com Roteador: $RouterName" -ForegroundColor Yellow
    Write-Host "----------------------------------------------------" -ForegroundColor Gray

    # Limpa executáveis rodando em background
    Get-Process "ConsoleAdvancedRouting" -ErrorAction SilentlyContinue | Stop-Process -Force

    # Inicia o servidor em background
    $ServerProcess = if ($UseRadix) {
        Start-Process -FilePath $ExePath -ArgumentList "-radix" -NoNewWindow -PassThru
    } else {
        Start-Process -FilePath $ExePath -NoNewWindow -PassThru
    }
    Start-Sleep -Seconds 1 # Aguarda bind da porta

    $Failed = $false
    try {
        # Caso 1: Rota Estática
        Write-Host " -> Caso 1: GET /users/new" -ForegroundColor Gray
        $Res = Invoke-WebRequest -Uri "http://localhost:9000/users/new" -UseBasicParsing
        if ($Res.Content -like "*Rota estatica*") {
            Write-Host "    [PASSOU]" -ForegroundColor Green
        } else {
            Write-Host "    [FALHOU] Resposta inesperada: $($Res.Content)" -ForegroundColor Red
            $Failed = $true
        }

        # Caso 2: Regex Numérico (/users/123)
        Write-Host " -> Caso 2: GET /users/123" -ForegroundColor Gray
        $Res = Invoke-WebRequest -Uri "http://localhost:9000/users/123" -UseBasicParsing
        if ($Res.Content -like "*ID (Numerico): 123*") {
            Write-Host "    [PASSOU]" -ForegroundColor Green
        } else {
            Write-Host "    [FALHOU] Resposta inesperada: $($Res.Content)" -ForegroundColor Red
            $Failed = $true
        }

        # Caso 3: Opcional Texto (/users/john)
        Write-Host " -> Caso 3: GET /users/john" -ForegroundColor Gray
        $Res = Invoke-WebRequest -Uri "http://localhost:9000/users/john" -UseBasicParsing
        if ($Res.Content -like "*ID (Texto/Opcional): john*") {
            Write-Host "    [PASSOU]" -ForegroundColor Green
        } else {
            Write-Host "    [FALHOU] Resposta inesperada: $($Res.Content)" -ForegroundColor Red
            $Failed = $true
        }

        # Caso 4: Opcional Vazio (/users)
        Write-Host " -> Caso 4: GET /users" -ForegroundColor Gray
        $Res = Invoke-WebRequest -Uri "http://localhost:9000/users" -UseBasicParsing
        if ($Res.Content -like "*parametro opcional ausente*") {
            Write-Host "    [PASSOU]" -ForegroundColor Green
        } else {
            Write-Host "    [FALHOU] Resposta inesperada: $($Res.Content)" -ForegroundColor Red
            $Failed = $true
        }

        # Caso 5: Rota Regex inválida (/users/abc deve cair no opcional texto)
        Write-Host " -> Caso 5: GET /users/abc" -ForegroundColor Gray
        $Res = Invoke-WebRequest -Uri "http://localhost:9000/users/abc" -UseBasicParsing
        if ($Res.Content -like "*ID (Texto/Opcional): abc*") {
            Write-Host "    [PASSOU]" -ForegroundColor Green
        } else {
            Write-Host "    [FALHOU] Resposta inesperada: $($Res.Content)" -ForegroundColor Red
            $Failed = $true
        }

        # Caso 6: Rota não mapeada (/users/123/edit deve retornar 404)
        Write-Host " -> Caso 6: GET /users/123/edit" -ForegroundColor Gray
        try {
            $Res = Invoke-WebRequest -Uri "http://localhost:9000/users/123/edit" -UseBasicParsing
            Write-Host "    [FALHOU] Esperado 404, obteve: $($Res.StatusCode)" -ForegroundColor Red
            $Failed = $true
        } catch [System.Net.WebException] {
            $ErrRes = $_.Exception.Response
            if ($ErrRes -and $ErrRes.StatusCode -eq "NotFound") {
                Write-Host "    [PASSOU] Retornou 404 conforme esperado." -ForegroundColor Green
            } else {
                Write-Host "    [FALHOU] Resposta de erro inesperada: $_" -ForegroundColor Red
                $Failed = $true
            }
        }

    } finally {
        # Finaliza o processo
        $ServerProcess | Stop-Process -Force -ErrorAction SilentlyContinue
        Start-Sleep -Milliseconds 500
    }

    if ($Failed) {
        Write-Error "Cenário de teste de integridade falhou para o $RouterName!"
    }
}

# 4. Executa os testes para ambos os roteadores
try {
    Run-TestScenario -UseRadix $false
    Run-TestScenario -UseRadix $true
    Write-Host "=== Todos os Testes de Integridade de Rede PASSARAM com Sucesso! ===" -ForegroundColor Green
} finally {
    # Garante a limpeza de DCUs e temporários
    Get-ChildItem $ScriptDir -Filter "*.dcu" | Remove-Item -Force
    Get-Process "ConsoleAdvancedRouting" -ErrorAction SilentlyContinue | Stop-Process -Force
}
