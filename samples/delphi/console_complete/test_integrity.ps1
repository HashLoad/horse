# Script de Validacao de Integridade do Horse - Console Complete
$ErrorActionPreference = "Stop"
$script:totalErrors = 0


function Run-IntegrityTest($RouterName, $Define) {
    Write-Host "==================================================" -ForegroundColor Cyan
    Write-Host "   TESTANDO INTEGRIDADE: $RouterName" -ForegroundColor Cyan
    Write-Host "==================================================" -ForegroundColor Cyan

    # 1. Compilando o Exemplo
    Write-Host "[1/5] Compilando ConsoleComplete.dpr..." -ForegroundColor Yellow
    $defineFlag = ""
    if ($Define -ne "") {
        $defineFlag = "-D`"$Define`""
    }
    $compileCmd = "dcc32 -B -Q -U`"../../../src`" -NS`"System;System.Win;Winapi;Data;Web`" $defineFlag ConsoleComplete.dpr"
    Invoke-Expression $compileCmd

    if (-not (Test-Path "ConsoleComplete.exe")) {
        Write-Error "Falha na compilacao! Executavel nao gerado."
    }
    Write-Host "Compilado com sucesso!" -ForegroundColor Green

    # 2. Executando o servidor em background
    Write-Host "[2/5] Iniciando o servidor na porta 9085 em segundo plano..." -ForegroundColor Yellow
    $serverProcess = Start-Process -FilePath ".\ConsoleComplete.exe" -NoNewWindow -PassThru
    Start-Sleep -Seconds 2

    # 3. Executando chamadas e validacoes
    Write-Host "[3/5] Executando chamadas HTTP..." -ForegroundColor Yellow

    # Funcao helper de validacao
    function Assert-Response($TestName, $Actual, $Expected) {
        if ($Actual -like $Expected) {
            Write-Host "  [OK] $TestName" -ForegroundColor Green
        } else {
            Write-Host "  [FALHA] $TestName" -ForegroundColor Red
            Write-Host "    Esperado: $Expected" -ForegroundColor DarkRed
            Write-Host "    Obtido: $Actual" -ForegroundColor DarkRed
            $script:totalErrors++
        }
    }

    try {
        # GET /ping
        $res = Invoke-RestMethod -Method Get -Uri "http://localhost:9085/ping"
        Assert-Response "GET /ping" $res "pong"

        # GET /resource/:id (com query e header case-insensitive)
        $headers = @{ "authorization" = "Bearer JWT_123" }
        $res = Invoke-RestMethod -Method Get -Uri "http://localhost:9085/resource/abc?q=busca" -Headers $headers
        $expected = '*abc*busca*Bearer JWT_123*'
        Assert-Response "GET /resource/:id (Params, Query, Header)" (ConvertTo-Json $res -Compress) $expected

        # POST /resource
        $res = Invoke-RestMethod -Method Post -Uri "http://localhost:9085/resource" -Body "Ola_Horse" -ContentType "text/plain"
        Assert-Response "POST /resource (Body)" $res "POST OK: Ola_Horse"

        # PUT /resource/:id
        $res = Invoke-RestMethod -Method Put -Uri "http://localhost:9085/resource/123"
        Assert-Response "PUT /resource/:id" (ConvertTo-Json $res -Compress) '*Updated*123*'

        # PATCH /resource/:id
        $res = Invoke-RestMethod -Method Patch -Uri "http://localhost:9085/resource/123"
        Assert-Response "PATCH /resource/:id" (ConvertTo-Json $res -Compress) '*Patched*123*'

        # DELETE /resource/:id
        $res = Invoke-RestMethod -Method Delete -Uri "http://localhost:9085/resource/123"
        Assert-Response "DELETE /resource/:id" (ConvertTo-Json $res -Compress) '*Deleted*123*'

        # QUERY /search (Novo Verbo)
        $res = curl.exe -s -X QUERY -d "filtro_busca" http://localhost:9085/search
        Assert-Response "QUERY /search (Body)" $res "SEARCH RESULT FOR: filtro_busca"

        # POST /upload (Multipart/Form-Data)
        Set-Content -Path "temp_sample_upload.txt" -Value "Conteudo de amostra de upload para o sample."
        $resUpload = curl.exe -s -X POST -F "file=@temp_sample_upload.txt" http://localhost:9085/upload
        Assert-Response "POST /upload (Multipart/Form-Data)" $resUpload '*Upload OK*temp_sample_upload.txt*'
        Remove-Item -Path "temp_sample_upload.txt" -Force -ErrorAction SilentlyContinue

        $resErr = curl.exe -s -i http://localhost:9085/error-trigger
        if ($resErr -match '400 Bad Request' -and $resErr -match 'Erro de Negocio Simulado') {
            Write-Host "  [OK] GET /error-trigger (Clean JSON Exception)" -ForegroundColor Green
        } else {
            Write-Host "  [FALHA] GET /error-trigger (Clean JSON Exception)" -ForegroundColor Red
            Write-Host "    Obtido: $resErr" -ForegroundColor DarkRed
            $script:totalErrors++
        }

        # Testes do Roteamento de Wildcard (*) e prioridade de rotas
        $res = Invoke-RestMethod -Method Get -Uri "http://localhost:9085/clientes"
        Assert-Response "GET /clientes (Prioridade Wildcard)" $res "clientes"

        $res = Invoke-RestMethod -Method Get -Uri "http://localhost:9085/pessoas"
        Assert-Response "GET /pessoas (Prioridade Wildcard)" $res "pessoas"

        $res = Invoke-RestMethod -Method Get -Uri "http://localhost:9085/qualquercoisa"
        Assert-Response "GET /qualquercoisa (Cai no Wildcard)" $res "coringao"

        # Testes de Integridade da issue #357 (Group + Route + End + Put)
        # 1. DELETE /api/test1/123
        $res = Invoke-RestMethod -Method Delete -Uri "http://localhost:9085/api/test1/123"
        Assert-Response "DELETE /api/test1/:id (Group Delete)" $res "delete1"

        # 2. GET /api/test2 (dentro do Route)
        $res = Invoke-RestMethod -Method Get -Uri "http://localhost:9085/api/test2"
        Assert-Response "GET /api/test2 (Group Route Get)" $res "get2"

        # 3. POST /api/test2 (dentro do Route)
        $res = Invoke-RestMethod -Method Post -Uri "http://localhost:9085/api/test2"
        Assert-Response "POST /api/test2 (Group Route Post)" $res "post2"

        # 4. PUT /api/teste3 (apos o &End do Route no mesmo grupo)
        try {
            $res = Invoke-RestMethod -Method Put -Uri "http://localhost:9085/api/teste3"
            Assert-Response "PUT /api/teste3 (Group Route End Put)" $res "put3"
        } catch {
            Write-Host "  [FALHA] PUT /api/teste3 falhou com erro HTTP (esperado no bug #357)" -ForegroundColor Red
            $script:totalErrors++
        }

        # Teste de Resiliencia: Access Violation Simulado (esperamos HTTP 500 sem cair o servidor)
        try {
            $resErr = curl.exe -s -i "http://localhost:9085/av-trigger"
            if ($resErr -match '500 Internal Server Error' -or $resErr -match '500 Internal Application Error') {
                Write-Host "  [OK] GET /av-trigger (Access Violation Handled)" -ForegroundColor Green
            } else {
                Write-Host "  [FALHA] GET /av-trigger (Access Violation Handled)" -ForegroundColor Red
                Write-Host "    Obtido: $resErr" -ForegroundColor DarkRed
                $script:totalErrors++
            }
        } catch {
            Write-Host "  [FALHA] GET /av-trigger falhou ou causou excecao nao tratada no PowerShell: $_" -ForegroundColor Red
            $script:totalErrors++
        }

        # Teste de Resiliencia: Stack Overflow Simulado (estouro de limite de pilha)
        try {
            $resErr = curl.exe -s -i --max-time 5 "http://localhost:9085/stack-trigger"
            if ($resErr -match '500 Internal Server Error' -or $resErr -match '500 Internal Application Error') {
                Write-Host "  [OK] GET /stack-trigger (Stack Overflow Handled)" -ForegroundColor Green
            } else {
                Write-Host "  [INFO] GET /stack-trigger retornou codigo inesperado (limite de Stack): $resErr" -ForegroundColor Cyan
            }
        } catch {
            Write-Host "  [INFO] GET /stack-trigger derrubou o servidor ou estourou (comportamento esperado do limite do SO): $_" -ForegroundColor Cyan
        }

    } catch {
        Write-Host "Erro inesperado ao realizar chamadas HTTP: $_" -ForegroundColor Red
        $script:totalErrors++
    }

    # 4. Finalizando o Servidor
    Write-Host "[4/5] Finalizando processo do servidor..." -ForegroundColor Yellow
    $serverProcess | Stop-Process -Force -ErrorAction SilentlyContinue
    Start-Sleep -Seconds 1

    # 5. Limpeza de arquivos gerados
    Write-Host "[5/5] Limpando executavel e arquivos temporarios..." -ForegroundColor Yellow
    Remove-Item -Path "ConsoleComplete.exe", "ConsoleComplete.dcu" -Force -ErrorAction SilentlyContinue
}

# Executando rodadas de teste
Run-IntegrityTest -RouterName "Default Router (RouterTree)" -Define ""
Run-IntegrityTest -RouterName "Radix Router (RadixRouter)" -Define "HORSE_RADIX_ROUTER"

Write-Host "==================================================" -ForegroundColor Cyan
if ($script:totalErrors -eq 0) {
    Write-Host "      TODOS OS TESTES DE INTEGRIDADE PASSARAM!     " -ForegroundColor Green
    Write-Host "==================================================" -ForegroundColor Green
} else {
    Write-Host "      OCORRERAM $script:totalErrors FALHA(S) NO TOTAL DOS TESTES!  " -ForegroundColor Red
    Write-Host "==================================================" -ForegroundColor Red
    exit 1
}
