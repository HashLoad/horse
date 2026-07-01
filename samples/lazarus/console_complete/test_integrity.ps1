# Script de Validacao de Integridade do Horse no FPC/Lazarus - Windows
$ErrorActionPreference = "Stop"

Write-Host "==================================================" -ForegroundColor Cyan
Write-Host "   INICIANDO TESTE DE INTEGRIDADE DO LAZARUS (FPC) " -ForegroundColor Cyan
Write-Host "==================================================" -ForegroundColor Cyan

# 1. Compilando o Exemplo
Write-Host "[1/5] Compilando ConsoleComplete.lpr..." -ForegroundColor Yellow
$compileCmd = "fpc -Mdelphi -Sh -Fu`"../../../src`" ConsoleComplete.lpr"
Invoke-Expression $compileCmd

if (-not (Test-Path "ConsoleComplete.exe")) {
    Write-Error "Falha na compilacao! Executavel nao gerado."
}
Write-Host "Compilado com sucesso!" -ForegroundColor Green

# 2. Executando o servidor em background
Write-Host "[2/5] Iniciando o servidor na porta 9086 em segundo plano..." -ForegroundColor Yellow
$serverProcess = Start-Process -FilePath ".\ConsoleComplete.exe" -NoNewWindow -PassThru
Start-Sleep -Seconds 2

# 3. Executando chamadas e validacoes
Write-Host "[3/5] Executando chamadas HTTP..." -ForegroundColor Yellow
$errors = 0

# Funcao helper de validacao
function Assert-Response($TestName, $Actual, $Expected) {
    if ($Actual -like $Expected) {
        Write-Host "  [OK] $TestName" -ForegroundColor Green
    } else {
        Write-Host "  [FALHA] $TestName" -ForegroundColor Red
        Write-Host "    Esperado: $Expected" -ForegroundColor DarkRed
        Write-Host "    Obtido: $Actual" -ForegroundColor DarkRed
        $global:errors++
    }
}

try {
    # GET /ping
    $res = Invoke-RestMethod -Method Get -Uri "http://localhost:9086/ping"
    Assert-Response "GET /ping" $res "pong"

    # GET /resource/:id (com query e header case-insensitive)
    $headers = @{ "authorization" = "Bearer JWT_123" }
    $res = Invoke-RestMethod -Method Get -Uri "http://localhost:9086/resource/abc?q=busca" -Headers $headers
    $expected = '*abc*busca*Bearer JWT_123*'
    Assert-Response "GET /resource/:id (Params, Query, Header)" (ConvertTo-Json $res -Compress) $expected

    # POST /resource
    $res = Invoke-RestMethod -Method Post -Uri "http://localhost:9086/resource" -Body "Ola_Horse" -ContentType "text/plain"
    Assert-Response "POST /resource (Body)" $res "POST OK: Ola_Horse"

    # PUT /resource/:id
    $res = Invoke-RestMethod -Method Put -Uri "http://localhost:9086/resource/123"
    Assert-Response "PUT /resource/:id" (ConvertTo-Json $res -Compress) '*Updated*123*'

    # PATCH /resource/:id
    $res = Invoke-RestMethod -Method Patch -Uri "http://localhost:9086/resource/123"
    Assert-Response "PATCH /resource/:id" (ConvertTo-Json $res -Compress) '*Patched*123*'

    # DELETE /resource/:id
    $res = Invoke-RestMethod -Method Delete -Uri "http://localhost:9086/resource/123"
    Assert-Response "DELETE /resource/:id" (ConvertTo-Json $res -Compress) '*Deleted*123*'

    # QUERY /search (Novo Verbo)
    $res = curl.exe -s -X QUERY -d "filtro_busca" http://localhost:9086/search
    Assert-Response "QUERY /search (Body)" $res "SEARCH RESULT FOR: filtro_busca"

    # POST /upload (Multipart/Form-Data)
    Set-Content -Path "temp_sample_upload.txt" -Value "Conteudo de amostra de upload para o sample."
    $resUpload = curl.exe -s -X POST -F "file=@temp_sample_upload.txt" http://localhost:9086/upload
    Assert-Response "POST /upload (Multipart/Form-Data)" $resUpload '*Upload OK*temp_sample_upload.txt*'
    Remove-Item -Path "temp_sample_upload.txt" -Force -ErrorAction SilentlyContinue

    # GET /error-trigger (EHorseException)
    $resErr = curl.exe -s -i http://localhost:9086/error-trigger
    if ($resErr -match '400 Bad Request' -and $resErr -match 'Erro de Negocio Simulado') {
        Write-Host "  [OK] GET /error-trigger (Clean JSON Exception)" -ForegroundColor Green
    } else {
        Write-Host "  [FALHA] GET /error-trigger (Clean JSON Exception)" -ForegroundColor Red
        Write-Host "    Obtido: $resErr" -ForegroundColor DarkRed
        $global:errors++
    }

} catch {
    Write-Host "Erro inesperado ao realizar chamadas HTTP: $_" -ForegroundColor Red
    $global:errors++
}

# 4. Finalizando o Servidor
Write-Host "[4/5] Finalizando processo do servidor..." -ForegroundColor Yellow
$serverProcess | Stop-Process -Force -ErrorAction SilentlyContinue
Start-Sleep -Seconds 1

# 5. Limpeza de arquivos gerados
Write-Host "[5/5] Limpando executavel e arquivos temporarios..." -ForegroundColor Yellow
Remove-Item -Path "ConsoleComplete.exe", "ConsoleComplete.o", "ConsoleComplete.pps" -Force -ErrorAction SilentlyContinue

# Conclusao
Write-Host "==================================================" -ForegroundColor Cyan
if ($errors -eq 0) {
    Write-Host "      TODOS OS TESTES DE INTEGRIDADE PASSARAM!     " -ForegroundColor Green
    Write-Host "==================================================" -ForegroundColor Green
} else {
    Write-Host "      OCORRERAM $errors FALHA(S) NO TESTE!         " -ForegroundColor Red
    Write-Host "==================================================" -ForegroundColor Red
    exit 1
}
