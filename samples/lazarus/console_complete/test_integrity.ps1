# Script de Validacao de Integridade do Horse no FPC/Lazarus - Windows
$ErrorActionPreference = "Stop"
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
$OutputEncoding = [System.Text.Encoding]::UTF8

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
    if ($Actual -is [array]) {
        $Actual = $Actual -join "`n"
    }
    if ($Actual.Contains($Expected)) {
        Write-Host "  [OK] $TestName" -ForegroundColor Green
    } else {
        Write-Host "  [FALHA] $TestName" -ForegroundColor Red
        Write-Host "    Esperado conter: $Expected" -ForegroundColor DarkRed
        Write-Host "    Obtido: $Actual" -ForegroundColor DarkRed
        $global:errors++
    }
}

try {
    # CORS Preflight (OPTIONS)
    $res = curl.exe -i -s -X OPTIONS http://localhost:9086/ping
    Assert-Response "CORS Preflight (Status 204)" $res "HTTP/1.1 204"
    Assert-Response "CORS Preflight (Origin Header)" $res "Access-Control-Allow-Origin: *"
    Assert-Response "CORS Preflight (Methods Header)" $res "Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS, PATCH, QUERY"
    Assert-Response "CORS Preflight (Headers Header)" $res "Access-Control-Allow-Headers: Content-Type, Authorization, X-Requested-With"

    # GET /ping (CORS origin header & Status 200)
    $res = curl.exe -i -s http://localhost:9086/ping
    Assert-Response "GET /ping (Status 200)" $res "HTTP/1.1 200"
    Assert-Response "GET /ping (Origin Header)" $res "Access-Control-Allow-Origin: *"
    Assert-Response "GET /ping (Body)" $res "pong"

    # GET /resource/:id (Decoding & Case-Insensitive Headers)
    $res = curl.exe -s -H "AuThOrIzAtIoN: Bearer JWT_123" "http://localhost:9086/resource/caf%C3%A9?q=teste%20completo"
    Assert-Response "GET /resource/:id (Decoding Parameter - Acento)" $res ('"id":"caf' + [char]233 + '"')
    Assert-Response "GET /resource/:id (Decoding Query - Espaco)" $res '"query":"teste completo"'
    Assert-Response "GET /resource/:id (Case-Insensitive Header Extraction)" $res '"auth":"Bearer JWT_123"'

    # POST /resource
    $res = curl.exe -i -s -X POST -H "Content-Type: text/plain" -d "Ola_Horse" http://localhost:9086/resource
    Assert-Response "POST /resource (Status 201)" $res "HTTP/1.1 201"
    Assert-Response "POST /resource (Body)" $res "POST OK: Ola_Horse"

    # PUT /resource/:id
    $res = curl.exe -i -s -X PUT http://localhost:9086/resource/123
    Assert-Response "PUT /resource/:id (Status 200)" $res "HTTP/1.1 200"
    Assert-Response "PUT /resource/:id (Body)" $res "Updated"

    # PATCH /resource/:id
    $res = curl.exe -i -s -X PATCH http://localhost:9086/resource/123
    Assert-Response "PATCH /resource/:id (Status 200)" $res "HTTP/1.1 200"
    Assert-Response "PATCH /resource/:id (Body)" $res "Patched"

    # DELETE /resource/:id
    $res = curl.exe -i -s -X DELETE http://localhost:9086/resource/123
    Assert-Response "DELETE /resource/:id (Status 200)" $res "HTTP/1.1 200"
    Assert-Response "DELETE /resource/:id (Body)" $res "Deleted"

    # QUERY /search
    $res = curl.exe -i -s -X QUERY -d "filtro_busca" http://localhost:9086/search
    Assert-Response "QUERY /search (Status 200)" $res "HTTP/1.1 200"
    Assert-Response "QUERY /search (Body)" $res "SEARCH RESULT FOR: filtro_busca"

    # POST /upload (Multipart)
    Set-Content -Path "temp_sample_upload.txt" -Value "Conteudo de amostra de upload"
    $res = curl.exe -i -s -X POST -F "file=@temp_sample_upload.txt" http://localhost:9086/upload
    Assert-Response "POST /upload (Status 200)" $res "HTTP/1.1 200"
    Assert-Response "POST /upload (Multipart Body)" $res "Upload OK"
    Remove-Item -Path "temp_sample_upload.txt" -Force -ErrorAction SilentlyContinue

    # GET /error-trigger (Clean JSON Exception)
    $res = curl.exe -i -s http://localhost:9086/error-trigger
    Assert-Response "GET /error-trigger (Status 400)" $res "HTTP/1.1 400"
    Assert-Response "GET /error-trigger (Clean JSON Exception)" $res "Erro de Negocio Simulado"
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
